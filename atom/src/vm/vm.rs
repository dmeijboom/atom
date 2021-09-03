use std::collections::{BTreeMap, HashMap};
use std::convert::TryInto;
use std::mem;
use std::ops::{Add, BitAnd, BitOr, Div, Mul, Sub};
use std::rc::Rc;

use indexmap::map::IndexMap;
use wyhash2::WyHash;

use atom_ir::{Code, Label, IR};
use atom_runtime::{
    AtomApi, AtomRef, Class, Closure, Fn, FnArg, FnPtr, Interface, Method, Object, Result,
    RuntimeError, Symbol, Value, ValueType,
};

use crate::compiler;
use crate::std::get_hooks;

use super::call_context::{CallContext, CallStack, Target};
use super::module::ModuleId;
use super::module_cache::ExternalHook;
use super::module_cache::ModuleCache;
use super::stack::Stack;

macro_rules! impl_op {
    ($vm:expr, int_only: $opname:ident) => {{
        let right = $vm.stack.pop()?;
        let left = $vm.stack.pop()?;

        $vm.stack.push(match left {
            Value::Int(val) => Value::Int(val.$opname(&right.try_into()?)),
            _ => {
                return Err(RuntimeError::new(format!(
                    "invalid type '{}' and '{}' in {}",
                    left.get_type().name(),
                    right.get_type().name(),
                    stringify!($ident)
                )));
            }
        });

        Ok(())
    }};

    ($vm:expr, compare: $opname:ident) => {{
        let right = $vm.stack.pop()?;
        let left = $vm.stack.pop()?;

        $vm.stack.push(match left {
            Value::Int(val) => Value::Bool(val.$opname(&right.try_into()?)),
            Value::Float(val) => Value::Bool(val.$opname(&right.try_into()?)),
            _ => {
                return Err(RuntimeError::new(format!(
                    "invalid type '{}' and '{}' in {}",
                    left.get_type().name(),
                    right.get_type().name(),
                    stringify!($opname)
                )));
            }
        });

        Ok(())
    }};

    ($vm:expr, $opname:ident) => {{
        let right = $vm.stack.pop()?;
        let left = $vm.stack.pop()?;

        $vm.stack.push(match left {
            Value::Int(val) => Value::Int(val.$opname(&right.try_into()?)),
            Value::Float(val) => Value::Float(val.$opname(&right.try_into()?)),
            _ => {
                return Err(RuntimeError::new(format!(
                    "invalid type '{}' and '{}' in {}",
                    left.get_type().name(),
                    right.get_type().name(),
                    stringify!($opname)
                )));
            }
        });

        Ok(())
    }};
}

enum Flow<'i> {
    Continue,
    Return(Value),
    TailCall(usize),
    JumpTo(&'i Label),
}

pub struct VM {
    stack: Stack,
    call_stack: CallStack,
    module_cache: ModuleCache,
    type_classes: HashMap<ValueType, AtomRef<Class>, WyHash>,
}

impl VM {
    pub fn new() -> Result<Self> {
        let mut vm = Self {
            stack: Stack::new(),
            call_stack: CallStack::new(),
            module_cache: ModuleCache::new(),
            type_classes: HashMap::with_hasher(WyHash::default()),
        };

        for hook in get_hooks() {
            vm.module_cache.add_external_hook(hook);
        }

        Ok(vm)
    }

    pub fn add_external_hook(&mut self, hook: ExternalHook) {
        self.module_cache.add_external_hook(hook);
    }

    pub fn register_module(
        &mut self,
        module: compiler::Module,
        filename: Option<String>,
    ) -> Result<()> {
        self.module_cache.register(module, filename)?;

        Ok(())
    }

    fn get_class(&self, value: &Value) -> Result<AtomRef<Class>> {
        Ok(match &value {
            Value::Object(object) => AtomRef::clone(&object.class),
            _ => {
                if let Some(class) = self.type_classes.get(&value.get_type()) {
                    return Ok(AtomRef::clone(class));
                }

                self.module_cache
                    .get_class("std.core", value.get_type().name())?
            }
        })
    }

    fn validate_class(&self, class: &Class, interface: &Interface) -> bool {
        for name in interface.functions.iter() {
            if !class.methods.contains_key(name) {
                return false;
            }
        }

        true
    }

    fn eval_tail_call(&mut self, arg_count: usize) -> Result<()> {
        // Reset locals, we can do this quite easily as the first n-locals are the arguments
        let context = self.call_stack.current_mut()?;
        let mut values = self.stack.pop_many(arg_count)?;

        for i in 0..arg_count {
            let local = unsafe { context.locals.get_unchecked_mut(i) };
            let arg_value = unsafe { values.get_unchecked_mut(i) };

            mem::swap(local, arg_value);
        }

        if context.locals.len() > arg_count {
            context.locals.drain(arg_count..);
        }

        Ok(())
    }

    #[inline(always)]
    fn eval_call(
        &mut self,
        keywords: &[String],
        arg_count: usize,
        store_result: bool,
    ) -> Result<()> {
        let value = self
            .stack
            .pop()
            .map_err(|_| RuntimeError::new("expected function on stack".to_string()))?;

        match value {
            Value::Fn(func) => self.eval_function_call(func, keywords, arg_count, store_result),
            Value::Closure(closure) => {
                self.eval_closure_call(closure, keywords, arg_count, store_result)
            }
            Value::Class(class) => self.eval_class_init(class, keywords, arg_count, store_result),
            Value::Method(method) => {
                self.eval_method_call(method, keywords, arg_count, store_result)
            }
            _ => Err(RuntimeError::new(format!(
                "type '{}' is not callable",
                value.get_type().name()
            ))),
        }
    }

    #[inline]
    fn eval_closure_call(
        &mut self,
        closure: AtomRef<Closure>,
        keywords: &[String],
        arg_count: usize,
        store_result: bool,
    ) -> Result<()> {
        let return_value = self.eval_func(Target::Closure(closure), keywords, arg_count)?;

        if store_result {
            self.stack.push(return_value);
        }

        Ok(())
    }

    #[inline]
    fn eval_function_call(
        &mut self,
        func: AtomRef<Fn>,
        keywords: &[String],
        arg_count: usize,
        store_result: bool,
    ) -> Result<()> {
        let return_value = self.eval_func(Target::Fn(func), keywords, arg_count)?;

        if store_result {
            self.stack.push(return_value);
        }

        Ok(())
    }

    fn eval_class_init(
        &mut self,
        class: AtomRef<Class>,
        keywords: &[String],
        arg_count: usize,
        store_result: bool,
    ) -> Result<()> {
        if keywords.len() != arg_count {
            return Err(RuntimeError::new(format!(
                "unable to initialize '{}' with non-keyword arguments",
                class.as_ref()
            )));
        }

        // Verify if there aren't any unknown field names
        for keyword in keywords {
            if !class.fields.contains_key(keyword) {
                return Err(RuntimeError::new(format!(
                    "unable to initialize '{}' with unknown field: {}",
                    class.as_ref(),
                    keyword
                )));
            }
        }

        let mut is_sorted = true;

        // Verify if all fields are initialized
        for (i, (field_name, _)) in class.fields.iter().enumerate() {
            if !keywords.contains(field_name) {
                return Err(RuntimeError::new(format!(
                    "unable to initialize '{}' without field: {}",
                    class.as_ref(),
                    field_name,
                )));
            }

            if is_sorted && field_name != &keywords[i] {
                is_sorted = false;
            }
        }

        let fields = if is_sorted {
            self.stack.pop_many(arg_count)?
        } else {
            let mut i = 0;
            let mut fields = self.stack.pop_many_t(class.fields.len(), |value| {
                let index = class
                    .fields
                    .get(&keywords[i])
                    .map(|field| field.id)
                    .unwrap();

                i += 1;

                (index, value)
            })?;

            fields.sort_unstable_by_key(|(i, _)| *i);
            fields.into_iter().map(|(_, value)| value).collect()
        };

        if store_result {
            self.stack
                .push(Value::Object(AtomRef::new(Object::new(class, fields))));
        }

        Ok(())
    }

    #[inline]
    fn eval_method_call(
        &mut self,
        method: AtomRef<Method>,
        keywords: &[String],
        arg_count: usize,
        store_result: bool,
    ) -> Result<()> {
        let return_value = self.eval_func(Target::Method(method), keywords, arg_count)?;

        if store_result {
            self.stack.push(return_value);
        }

        Ok(())
    }

    fn prepare_args(
        &self,
        mut values: Vec<Value>,
        keywords: &[String],
        args: &IndexMap<String, FnArg>,
    ) -> Result<BTreeMap<usize, Value>> {
        let mut ordered_values = BTreeMap::new();

        for name in keywords.iter() {
            if let Some(arg_idx) = args.keys().position(|arg| arg == name) {
                ordered_values.insert(arg_idx, values.remove(0));
                continue;
            }

            return Err(RuntimeError::new(format!("no such argument '{}'", name)));
        }

        for i in 0..args.len() {
            if ordered_values.contains_key(&i) {
                continue;
            }

            ordered_values.insert(i, values.remove(0));
        }

        Ok(ordered_values)
    }

    fn eval_func(
        &mut self,
        target: Target,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<Value> {
        let (func, receiver, values) = match &target {
            Target::Fn(func) => (AtomRef::clone(func), None, None),
            Target::Method(method) => (
                AtomRef::clone(&method.func),
                Some(method.receiver.clone()),
                None,
            ),
            Target::Closure(closure) => (
                AtomRef::clone(&closure.func),
                None,
                Some(closure.values.clone()),
            ),
        };

        match &func.ptr {
            FnPtr::Native(source) => {
                if arg_count != func.args.len() {
                    return Err(RuntimeError::new(format!(
                        "invalid argument count for target: {}(...) (expected {}, not {})",
                        target,
                        func.args.len(),
                        arg_count,
                    )));
                }

                let locals = if keywords.is_empty() {
                    self.stack.pop_many(arg_count)?
                } else {
                    let unordered_values = self.stack.pop_many(arg_count)?;

                    self.prepare_args(unordered_values, keywords, &func.args)
                        .map_err(|e| {
                            let message = e.message.clone();
                            e.with_message(format!("{} in target {}(...)", message, target,))
                        })?
                        .into_values()
                        .collect()
                };

                let module_id = target.origin().module_id;

                self.call_stack.push(CallContext::new(
                    target,
                    receiver,
                    if let Some(values) = values {
                        vec![values, locals].concat()
                    } else {
                        locals
                    },
                ));

                let source = Rc::clone(source);

                if let Some(result) = self._eval(module_id, source)? {
                    self.call_stack.pop();

                    return Ok(result);
                }
            }
            FnPtr::External(closure) => {
                if !keywords.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "unable to use keyword arguments in external target: {}(..)",
                        target,
                    )));
                }

                self.call_stack.push(CallContext::new(target, None, vec![]));

                let values = self.stack.pop_many(arg_count)?;

                if let Some(return_value) = closure(self, receiver, values)? {
                    self.call_stack.pop();

                    return Ok(return_value);
                }
            }
        };

        self.call_stack.pop();

        Ok(Value::Void)
    }

    #[inline(always)]
    fn eval_const<T: Into<Value>>(&mut self, data: T) {
        self.stack.push(data.into());
    }

    #[inline(always)]
    fn eval_symbol(&mut self, name: &str) {
        self.stack.push(if name == "nil" {
            Value::Option(None)
        } else {
            Value::Symbol(Symbol::new(&name))
        });
    }

    #[inline(always)]
    fn eval_single<'i>(&mut self, module_id: ModuleId, ir: &'i IR) -> Result<Flow<'i>> {
        match &ir.code {
            Code::ConstInt(val) => self.eval_const(*val),
            Code::ConstBool(val) => self.eval_const(*val),
            Code::ConstSymbol(name) => self.eval_symbol(name),
            Code::ConstFloat(val) => self.eval_const(*val),
            Code::ConstChar(val) => self.eval_const(*val),
            Code::ConstByte(val) => self.eval_const(*val),
            Code::ConstString(val) => self.eval_const(val.clone()),
            Code::MakeRange => self.eval_make_range()?,
            Code::MakeTuple(len) => self.eval_make_tuple(*len)?,
            Code::MakeArray(len) => self.eval_make_array(*len)?,
            Code::MakeMap(len) => self.eval_make_map(*len)?,
            Code::MakeTemplate(len) => self.eval_make_template(*len)?,
            Code::Discard => self.stack.delete()?,
            Code::Return => return Ok(Flow::Return(self.stack.pop()?)),
            Code::MakeRef => self.eval_make_ref()?,
            Code::Deref => self.eval_deref()?,
            Code::LogicalAnd => self.eval_logical_and()?,
            Code::ArithmeticBitOr => self.eval_arithmetic_bit_or()?,
            Code::ArithmeticBitAnd => self.eval_arithmetic_bit_and()?,
            Code::ArithmeticAdd => self.eval_arithmetic_add()?,
            Code::ArithmeticSub => self.eval_arithmetic_sub()?,
            Code::ArithmeticMul => self.eval_arithmetic_mul()?,
            Code::ArithmeticDiv => self.eval_arithmetic_div()?,
            Code::ArithmeticExp => self.eval_arithmetic_exp()?,
            Code::ComparisonEq => self.eval_comparison_eq()?,
            Code::ComparisonNeq => self.eval_comparison_neq()?,
            Code::ComparisonGt => self.eval_comparison_gt()?,
            Code::ComparisonGte => self.eval_comparison_gte()?,
            Code::ComparisonLt => self.eval_comparison_lt()?,
            Code::ComparisonLte => self.eval_comparison_lte()?,
            Code::AssertIsType => self.eval_assert_is_type()?,
            Code::Not => self.eval_not()?,
            Code::Unwrap => self.eval_unwrap()?,
            Code::Validate => self.eval_validate()?,
            Code::Cast(type_name) => self.eval_cast(type_name)?,
            Code::Call(arg_count) => self.eval_call(&[], *arg_count, true)?,
            Code::CallKeywords((keywords, arg_count)) => {
                self.eval_call(keywords, *arg_count, true)?
            }
            Code::CallVoid(arg_count) => self.eval_call(&[], *arg_count, false)?,
            Code::CallKeywordsVoid((keywords, arg_count)) => {
                self.eval_call(keywords, *arg_count, false)?
            }
            Code::TailCall(arg_count) => return Ok(Flow::TailCall(*arg_count)),
            Code::Store(id) => self.eval_store(*id, false)?,
            Code::StoreMut(id) => self.eval_store(*id, true)?,
            Code::LoadIndex => self.eval_load_index(false)?,
            Code::TeeIndex => self.eval_load_index(true)?,
            Code::StoreIndex => self.eval_store_index()?,
            Code::LoadMember(member) => self.eval_load_member(module_id, member, false)?,
            Code::TeeMember(member) => self.eval_load_member(module_id, member, true)?,
            Code::StoreMember(member) => self.eval_store_member(module_id, member)?,
            Code::Load(id) => self.eval_load(*id)?,
            Code::LoadReceiver => self.eval_load_receiver()?,
            Code::LoadGlobal(id) => self.eval_load_global(module_id, *id)?,
            Code::LoadFn(id) => self.eval_load_fn(module_id, *id)?,
            Code::LoadClosure(id) => self.eval_load_closure(module_id, *id)?,
            Code::LoadClass(id) => self.eval_load_class(module_id, *id)?,
            Code::LoadInterface(id) => self.eval_load_interface(module_id, *id)?,
            Code::LoadTarget => self.eval_load_target()?,
            Code::SetLabel(_) => {}
            Code::Branch((true_label, false_label)) => {
                return Ok(Flow::JumpTo(self.eval_branch(true_label, false_label)?))
            }
            Code::Jump(label) => return Ok(Flow::JumpTo(label)),
            Code::JumpIfTrue(label) => return self.eval_jump_if_true(label),
            Code::Raise => self.eval_raise(module_id)?,
        };

        Ok(Flow::Continue)
    }

    fn eval_make_range(&mut self) -> Result<()> {
        let to = self.stack.pop()?;
        let from = self.stack.pop()?;

        if from.get_type() != ValueType::Int || to.get_type() != ValueType::Int {
            return Err(RuntimeError::new(format!(
                "unable to construct Range with '{}' and '{}', expected: Int",
                from.get_type().name(),
                to.get_type().name()
            ))
            .with_kind("TypeError".to_string()));
        }

        let object = Object::new(self.find_class("std.core", "Range")?, vec![from, to]);

        self.stack.push(Value::Object(AtomRef::new(object)));

        Ok(())
    }

    fn eval_make_tuple(&mut self, len: usize) -> Result<()> {
        let values = self.stack.pop_many(len)?;

        self.stack.push(Value::Tuple(values.into()));

        Ok(())
    }

    fn eval_make_array(&mut self, len: usize) -> Result<()> {
        let values = self.stack.pop_many(len)?;

        self.stack.push(Value::Array(AtomRef::new(values)));

        Ok(())
    }

    fn eval_make_map(&mut self, len: usize) -> Result<()> {
        let mut map = HashMap::new();
        let mut key_values = self.stack.pop_many(len * 2)?;

        for _ in 0..len {
            let key = key_values.remove(0);
            let value = key_values.remove(0);

            map.insert(key, value);
        }

        self.stack.push(Value::Map(AtomRef::new(map)));

        Ok(())
    }

    fn eval_make_template(&mut self, len: usize) -> Result<()> {
        let s = self
            .stack
            .pop_many(len)?
            .into_iter()
            .map(|value| format!("{}", value))
            .collect::<String>();

        self.stack.push(Value::String(AtomRef::new(s)));

        Ok(())
    }

    fn eval_make_ref(&mut self) -> Result<()> {
        let value = self.stack.pop()?;

        self.stack.push(Value::Ref(AtomRef::new(value)));

        Ok(())
    }

    fn eval_deref(&mut self) -> Result<()> {
        let value = self.stack.pop()?;

        if let Value::Ref(value) = value {
            self.stack.push(value.clone_inner_or_unwrap());

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "unable to dereference: {}",
            value.get_type().name()
        )))
    }

    fn eval_logical_and(&mut self) -> Result<()> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;

        self.stack.push(match left {
            Value::Bool(val) => Value::Bool(val && right.try_into()?),
            _ => {
                return Err(RuntimeError::new(format!(
                    "invalid types: {} and {} in logical and",
                    left.get_type().name(),
                    right.get_type().name()
                )))
            }
        });

        Ok(())
    }

    fn eval_arithmetic_bit_or(&mut self) -> Result<()> {
        impl_op!(self, int_only: bitor)
    }

    fn eval_arithmetic_bit_and(&mut self) -> Result<()> {
        impl_op!(self, int_only: bitand)
    }

    fn eval_arithmetic_add(&mut self) -> Result<()> {
        impl_op!(self, add)
    }

    fn eval_arithmetic_sub(&mut self) -> Result<()> {
        impl_op!(self, sub)
    }

    fn eval_arithmetic_mul(&mut self) -> Result<()> {
        impl_op!(self, mul)
    }

    fn eval_arithmetic_div(&mut self) -> Result<()> {
        impl_op!(self, div)
    }

    fn eval_arithmetic_exp(&mut self) -> Result<()> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;

        self.stack.push(match left {
            Value::Int(val) => {
                let right_val: i64 = right.try_into()?;

                Value::Int(val.pow(right_val as u32))
            }
            Value::Float(val) => Value::Float(val.powf(right.try_into()?)),
            _ => {
                return Err(RuntimeError::new(format!(
                    "invalid types: {} and {} in exponent",
                    left.get_type().name(),
                    right.get_type().name()
                )))
            }
        });

        Ok(())
    }

    fn eval_comparison_eq(&mut self) -> Result<()> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;

        self.stack.push(Value::Bool(left.eq(&right)));

        Ok(())
    }

    fn eval_comparison_neq(&mut self) -> Result<()> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;

        self.stack.push(Value::Bool(left.ne(&right)));

        Ok(())
    }

    fn eval_comparison_gt(&mut self) -> Result<()> {
        impl_op!(self, compare: gt)
    }

    fn eval_comparison_gte(&mut self) -> Result<()> {
        impl_op!(self, compare: ge)
    }

    fn eval_comparison_lt(&mut self) -> Result<()> {
        impl_op!(self, compare: lt)
    }

    fn eval_comparison_lte(&mut self) -> Result<()> {
        impl_op!(self, compare: le)
    }

    fn eval_assert_is_type(&mut self) -> Result<()> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;
        let left_class = self.get_class(&left)?;

        if let Value::Class(right_class) = right {
            self.stack
                .push(Value::Bool(left_class.as_ref() == right_class.as_ref()));

            return Ok(());
        }

        if let Value::Interface(interface) = right {
            self.stack
                .push(Value::Bool(self.validate_class(&left_class, &interface)));

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "unable to assert type with: {}",
            right,
        )))
    }

    fn eval_unwrap(&mut self) -> Result<()> {
        let value: Option<Box<Value>> = self.stack.pop()?.try_into()?;

        if let Some(value) = value {
            self.stack.push(*value);

            return Ok(());
        }

        Err(RuntimeError::new("unable to unwrap nil value".to_string()))
    }

    fn eval_not(&mut self) -> Result<()> {
        let value: bool = self.stack.pop()?.try_into()?;

        self.stack.push(Value::Bool(!value));

        Ok(())
    }

    fn eval_validate(&mut self) -> Result<()> {
        let interface_value = self.stack.pop()?;

        if let Value::Interface(interface) = interface_value {
            let value = self.stack.pop()?;
            let class = self.get_class(&value)?;

            if !self.validate_class(&class, &interface) {
                return Err(RuntimeError::new(format!(
                    "validation failed, class '{}' doesn't implement interface: {}",
                    class.as_ref(),
                    interface.as_ref(),
                )));
            }

            self.stack.push(value);

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "unable to validate non-interface: {}",
            interface_value.get_type().name()
        )))
    }

    fn eval_cast(&mut self, type_name: &str) -> Result<()> {
        let value = self.stack.pop()?;

        self.stack.push(match value {
            Value::Int(val) if type_name == "Float" => Value::Float(val as f64),
            Value::Int(val) if type_name == "Byte" => Value::Byte(val as u8),
            Value::Float(val) if type_name == "Int" => Value::Int(val as i64),
            Value::Char(val) if type_name == "Byte" => Value::Byte(val as u8),
            Value::Byte(val) if type_name == "Int" => Value::Int(val as i64),
            Value::Byte(val) if type_name == "Char" => Value::Char(val as char),
            Value::Bool(val) if type_name == "Int" => Value::Int(val as i64),
            _ => {
                return Err(RuntimeError::new(format!(
                    "unable to cast to invalid type: {}",
                    type_name
                )));
            }
        });

        Ok(())
    }

    fn eval_store(&mut self, id: usize, _is_mutable: bool) -> Result<()> {
        let value = self.stack.pop()?;
        let context = self.call_stack.current_mut()?;
        let locals_len = context.locals.len();

        if id < locals_len {
            let item = unsafe { context.locals.get_unchecked_mut(id) };

            *item = value;
        } else {
            let diff = id - context.locals.len();

            // Insert padding if required
            for _ in 0..diff {
                context.locals.push(Value::Void);
            }

            context.locals.insert(id, value);
        }

        Ok(())
    }

    fn eval_load_index(&mut self, push_back: bool) -> Result<()> {
        let index = self.stack.pop()?;
        let value = self.stack.pop()?;

        let elems = if let Value::Array(array) = &value {
            Some(array.as_slice())
        } else if let Value::Tuple(tuple) = &value {
            Some(tuple.as_ref())
        } else {
            None
        };

        if let Some(elems) = elems {
            let index: usize = index
                .try_into()
                .map_err(|e| RuntimeError::new(format!("{} in index lookup", e)))?;

            if let Some(item) = elems.get(index) {
                let item = item.clone();

                if push_back {
                    self.stack.push(value);
                }

                self.stack.push(item);

                return Ok(());
            }

            return Err(RuntimeError::new(
                format!("index out of bounds: {}", index,),
            ));
        }

        if let Value::Map(map) = &value {
            if let Some(item) = map.get(&index) {
                let item = item.clone();

                if push_back {
                    self.stack.push(value);
                }

                self.stack.push(item);

                return Ok(());
            }

            return Err(RuntimeError::new(
                format!("index out of bounds: {}", index,),
            ));
        }

        Err(RuntimeError::new(format!(
            "unable to index type: {}",
            value.get_type().name()
        )))
    }

    fn eval_store_index(&mut self) -> Result<()> {
        let value = self.stack.pop()?;
        let index = self.stack.pop()?;

        let data = self.stack.pop()?;

        match data {
            Value::Array(mut array) => {
                let index: usize = index
                    .try_into()
                    .map_err(|e| RuntimeError::new(format!("{} in index lookup", e)))?;

                let array = array.as_mut();

                if array.get(index).is_some() {
                    array[index] = value;

                    return Ok(());
                }

                Err(RuntimeError::new(
                    format!("index out of bounds: {}", index,),
                ))
            }
            Value::Map(mut map) => {
                map.as_mut().insert(index, value);

                Ok(())
            }
            _ => Err(RuntimeError::new(format!(
                "unable to index type: {}",
                data.get_type().name()
            ))),
        }
    }

    fn eval_load_member(
        &mut self,
        module_id: ModuleId,
        member: &str,
        push_back: bool,
    ) -> Result<()> {
        let receiver = self.stack.pop()?;
        let class = self.get_class(&receiver)?;

        if let Some(field) = class.fields.get(member) {
            if let Value::Object(object) = receiver {
                if !field.public && module_id != class.origin.module_id {
                    return Err(RuntimeError::new(format!(
                        "unable to access private field '{}' of class: {}",
                        member,
                        class.as_ref()
                    )));
                }

                let field = object.get_field(field.id).cloned().ok_or_else(|| {
                    RuntimeError::new(format!(
                        "unable to get unknown field '{}' of class: {}",
                        member,
                        class.as_ref()
                    ))
                })?;

                if push_back {
                    self.stack.push(Value::Object(object));
                }

                self.stack.push(field);

                return Ok(());
            }

            return Err(RuntimeError::new(format!(
                "unable to lookup field of type: {}",
                receiver.get_type().name(),
            )));
        }

        if let Some(func) = class.methods.get(member) {
            if push_back {
                self.stack.push(receiver.clone());
            }

            self.stack.push(Value::Method(AtomRef::new(Method {
                receiver,
                func: AtomRef::clone(func),
                class,
            })));

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "no such field or method '{}' for: {}",
            member,
            self.get_class(&receiver).unwrap().as_ref(),
        )))
    }

    fn eval_store_member(&mut self, module_id: ModuleId, member: &str) -> Result<()> {
        let object = self.stack.pop()?;
        let class = self.get_class(&object)?;
        let value = self.stack.pop()?;

        if let Some(field) = class.fields.get(member) {
            if !field.public && module_id != class.origin.module_id {
                return Err(RuntimeError::new(format!(
                    "unable to access private field '{}' of class: {}",
                    member,
                    class.as_ref()
                )));
            }

            if !field.mutable {
                return Err(RuntimeError::new(format!(
                    "unable to assign to immutable field '{}' of class: {}",
                    member,
                    class.as_ref()
                )));
            }

            if let Value::Object(mut object) = object {
                object.as_mut().set_field_value(field.id, value);

                return Ok(());
            }

            return Err(RuntimeError::new(format!(
                "unable to store member on invalid type '{}' expected Object",
                object.get_type().name(),
            )));
        }

        Err(RuntimeError::new(format!(
            "no such field '{}' for class: {}",
            member,
            class.as_ref(),
        )))
    }

    fn eval_load(&mut self, id: usize) -> Result<()> {
        if !self.call_stack.is_empty() {
            let context = self.call_stack.current()?;

            if let Some(value) = context.locals.get(id) {
                self.stack.push(value.clone());

                return Ok(());
            }
        }

        Err(RuntimeError::new(format!(
            "undefined local with id: {}",
            id
        )))
    }

    fn eval_load_receiver(&mut self) -> Result<()> {
        let receiver = self.call_stack.current().and_then(|context| {
            context.receiver.clone().ok_or_else(|| {
                RuntimeError::new("unable to load 'this' outside of a method".to_string())
            })
        })?;

        self.stack.push(receiver);

        Ok(())
    }

    fn eval_load_global(&mut self, module_id: ModuleId, id: usize) -> Result<()> {
        let current_module = self.module_cache.get_module_by_id(module_id)?;
        let value = current_module
            .globals
            .get(id)
            .cloned()
            .ok_or_else(|| RuntimeError::new(format!("global with ID '{}' not found", id)))?;

        self.stack.push(value);

        Ok(())
    }

    fn eval_load_fn(&mut self, module_id: ModuleId, id: usize) -> Result<()> {
        let current_module = self.module_cache.get_module_by_id(module_id)?;
        let value = current_module
            .funcs
            .get(id)
            .map(|val| AtomRef::clone(val))
            .ok_or_else(|| RuntimeError::new(format!("function with ID '{}' not found", id)))?;

        self.stack.push(Value::Fn(value));

        Ok(())
    }

    fn eval_load_closure(&mut self, module_id: ModuleId, id: usize) -> Result<()> {
        let current_module = self.module_cache.get_module_by_id(module_id)?;
        let func = current_module
            .closures
            .get(id)
            .map(|val| AtomRef::clone(val))
            .ok_or_else(|| RuntimeError::new(format!("closure with ID '{}' not found", id)))?;

        let context = self.call_stack.current()?;

        self.stack.push(Value::Closure(AtomRef::new(Closure {
            func,
            values: context.locals.clone(),
        })));

        Ok(())
    }

    fn eval_load_class(&mut self, module_id: ModuleId, id: usize) -> Result<()> {
        let current_module = self.module_cache.get_module_by_id(module_id)?;
        let value = current_module
            .classes
            .get_index(id)
            .map(|(_, val)| AtomRef::clone(val))
            .ok_or_else(|| RuntimeError::new(format!("class with ID '{}' not found", id)))?;

        self.stack.push(Value::Class(value));

        Ok(())
    }

    fn eval_load_interface(&mut self, module_id: ModuleId, id: usize) -> Result<()> {
        let current_module = self.module_cache.get_module_by_id(module_id)?;
        let value = current_module
            .interfaces
            .get(id)
            .map(|val| AtomRef::clone(val))
            .ok_or_else(|| RuntimeError::new(format!("interface with ID '{}' not found", id)))?;

        self.stack.push(Value::Interface(value));

        Ok(())
    }

    fn eval_load_target(&mut self) -> Result<()> {
        if let Some(context) = self.call_stack.last() {
            self.stack.push(match &context.target {
                Target::Fn(func) => Value::Fn(AtomRef::clone(func)),
                Target::Method(method) => Value::Method(AtomRef::clone(method)),
                Target::Closure(closure) => Value::Closure(AtomRef::clone(closure)),
            });

            return Ok(());
        }

        Err(RuntimeError::new(
            "unable to load target outside of call".to_string(),
        ))
    }

    fn eval_branch<'s>(
        &mut self,
        true_label: &'s Label,
        false_label: &'s Label,
    ) -> Result<&'s Label> {
        let value = self.stack.pop()?.try_into()?;

        Ok(if value { true_label } else { false_label })
    }

    fn eval_jump_if_true<'i>(&mut self, label: &'i Label) -> Result<Flow<'i>> {
        let value = self.stack.pop()?.try_into()?;

        if value {
            self.stack.push(Value::Bool(value));

            return Ok(Flow::JumpTo(label));
        }

        Ok(Flow::Continue)
    }

    fn eval_raise(&mut self, module_id: ModuleId) -> Result<()> {
        let value = self.stack.pop()?;

        // @TODO: come up with a generic solution for raising errors from atom
        if let Value::Object(object) = &value {
            if object.class.fields.contains_key("message") {
                let kind = object.class.name.clone();

                self.stack.push(value);
                self.eval_load_member(module_id, "message", false)?;

                let message = self.stack.pop()?;

                return Err(RuntimeError::new(format!("{}", message)).with_kind(kind));
            }
        }

        Err(RuntimeError::new(format!("{}", value)))
    }

    fn find_label(&self, instructions: &[IR], search: &str) -> Result<usize> {
        for (i, ir) in instructions.iter().enumerate() {
            if let Code::SetLabel(label) = &ir.code {
                if label == search {
                    return Ok(i);
                }
            }
        }

        Err(RuntimeError::new(format!(
            "unable to find label: {}",
            search,
        )))
    }

    #[inline(always)]
    fn _eval(&mut self, module_id: ModuleId, instructions: Rc<Vec<IR>>) -> Result<Option<Value>> {
        let mut i = 0;
        let mut return_addr = vec![];
        let instructions_len = instructions.len();

        loop {
            while i < instructions_len {
                let ir = unsafe { instructions.get_unchecked(i) };

                // Evaluates the instruction and optionally returns a label to jump to
                match self.eval_single(module_id, ir) {
                    Ok(flow) => match flow {
                        Flow::Continue => i += 1,
                        Flow::Return(value) => {
                            if let Some(addr) = return_addr.pop() {
                                i = addr;

                                self.stack.push(value);

                                continue;
                            }

                            return Ok(Some(value));
                        }
                        Flow::JumpTo(label) => {
                            i = match label.index {
                                Some(index) => index,
                                None => self.find_label(&*instructions, &label.name)?,
                            };

                            continue;
                        }
                        Flow::TailCall(arg_count) => {
                            return_addr.push(i + 1);

                            self.eval_tail_call(arg_count)?;

                            i = 0;

                            continue;
                        }
                    },
                    Err(e) => {
                        let module = self.module_cache.get_module_by_id(module_id)?;

                        if e.location.is_none() {
                            let e = e
                                .with_location(ir.location.clone())
                                .with_module(&module.name);

                            if let Some(filename) = &module.filename {
                                return Err(e.with_filename(filename.clone()));
                            }

                            return Err(e);
                        };

                        return Err(e);
                    }
                };
            }

            // We're finished but there is stil a return-address, this means we're expected to have pushed a return-value
            if let Some(addr) = return_addr.pop() {
                i = addr;

                self.stack.push(Value::Void);

                continue;
            }

            break;
        }

        Ok(None)
    }

    pub fn eval(&mut self, module: &str, instructions: Vec<IR>) -> Result<()> {
        let module_id = self.module_cache.get_module(module)?.id;

        self._eval(module_id, Rc::new(instructions))
            .map_err(|e| e.with_stack_trace(self.call_stack.rewind()))?;

        Ok(())
    }

    pub fn result(&mut self) -> Option<Value> {
        self.stack.pop().ok()
    }
}

impl AtomApi for VM {
    fn find_class(&self, module_name: &str, class_name: &str) -> Result<AtomRef<Class>> {
        self.module_cache.get_class(module_name, class_name)
    }
}
