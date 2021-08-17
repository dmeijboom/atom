use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::mem;
use std::ops::{Add, BitAnd, BitOr, Div, Mul, Sub};
use std::path::Path;
use std::rc::Rc;

use indexmap::map::IndexMap;

use crate::compiler::{Code, Label, IR};
use crate::runtime::convert::{to_bool, to_float, to_int};
use crate::runtime::{
    with_auto_deref, with_auto_deref_mut, Method, Object, Result, RuntimeError, TypeId, Value,
};
use crate::std::core::DEFAULT_IMPORTS;
use crate::std::get_middleware;
use crate::utils;
use crate::utils::Error;
use crate::vm::call_stack::{CallContext, Target};
use crate::vm::module_cache::{
    ArgumentDesc, FuncSource, InterfaceDesc, Module, ModuleCache, ModuleId, TypeDesc,
};
use crate::vm::{ClassDesc, FuncDesc};

use super::call_stack::CallStack;
use super::stack::Stack;
use super::stacked::Stacked;

fn argument_to_stacked(func: &FuncDesc, i: usize, value: Value) -> Stacked {
    // Immutable locals can be placed on the stack as their contents will not be changed but we
    // don't want to do this for non-primitive types because that will be slower in most cases
    if !value.get_type().is_primitive()
        || func
            .args
            .get_index(i)
            .map(|(_, arg)| arg.mutable)
            .unwrap_or(false)
    {
        Stacked::ByRef(Rc::new(RefCell::new(value)))
    } else {
        Stacked::ByValue(value)
    }
}

macro_rules! arithmetic_op {
    ($vm:expr, int_only: $opname:ident) => {
        let right = $vm.stack.pop()?.into_value();
        let left = $vm.stack.pop()?.into_value();

        $vm.stack.push(match left {
            Value::Int(val) => Value::Int(val.$opname(to_int(right)?)),
            _ => {
                return Err(RuntimeError::new(format!(
                    "invalid type '{}' and '{}' in {}",
                    left.get_type().name(),
                    right.get_type().name(),
                    stringify!($ident)
                )))
            }
        });
    };

    ($vm:expr, $opname:ident) => {
        let right = $vm.stack.pop()?.into_value();
        let left = $vm.stack.pop()?.into_value();

        $vm.stack.push(match left {
            Value::Int(val) => Value::Int(val.$opname(to_int(right)?)),
            Value::Float(val) => Value::Float(val.$opname(to_float(right)?)),
            _ => {
                return Err(RuntimeError::new(format!(
                    "invalid type '{}' and '{}' in {}",
                    left.get_type().name(),
                    right.get_type().name(),
                    stringify!($ident)
                )))
            }
        });
    };
}

enum Flow<'i> {
    Continue,
    Return(Value),
    TailCall(usize),
    JumpTo(&'i Label),
}

fn setup_std(vm: &mut VM) -> Result<()> {
    for (module_name, middleware) in get_middleware() {
        vm.module_cache.register_middleware(module_name, middleware);
    }

    let module = utils::parse_and_compile(
        include_str!("../std/atom/std/core.atom"),
        Some("std/atom/std/core.atom".into()),
    )
    .map_err(|e| match e {
        Error::Runtime(e) => e,
        Error::Compile(e) => {
            RuntimeError::new(format!("failed to compile 'std/atom/core.atom': {}", e))
                .with_pos(e.pos)
        }
        Error::ParseError(e) => {
            RuntimeError::new(format!("failed to parse 'std/atom/core.atom': {}", e))
                .with_pos(e.location.offset..e.location.offset + 1)
        }
    })?;

    vm.register_module(module)?;

    Ok(())
}

pub struct VM {
    stack: Stack,
    call_stack: CallStack,
    module_cache: ModuleCache,
}

impl VM {
    pub fn new() -> Result<Self> {
        let mut vm = Self {
            stack: Stack::new(),
            call_stack: CallStack::new(),
            module_cache: ModuleCache::new(),
        };

        setup_std(&mut vm)?;

        Ok(vm)
    }

    pub fn add_module_lookup_path(&mut self, path: impl AsRef<Path>) {
        self.module_cache
            .add_lookup_path(path.as_ref().to_path_buf());
    }

    pub fn register_module(&mut self, module: Module) -> Result<()> {
        let mut module = module;

        // skip default imports for the core module
        if module.name != "std.core" {
            for name in DEFAULT_IMPORTS {
                self.import_in_module(&mut module, name)?;
            }
        }

        while !module.imports.is_empty() {
            let path = module.imports.remove(0);

            self.import_in_module(&mut module, &path)?;
        }

        self.module_cache.add(module)?;

        Ok(())
    }

    pub fn get_type_id(&self, module: &str, name: &str) -> Result<TypeId> {
        Ok(self.module_cache.lookup_type(module, name)?.id)
    }

    pub fn get_fn_self(&self) -> Option<Rc<RefCell<Value>>> {
        if let Ok(context) = self.call_stack.current() {
            return context.this.as_ref().and_then(|rc| Some(Rc::clone(rc)));
        }

        None
    }

    fn validate_class(&self, class: &ClassDesc, interface: &InterfaceDesc) -> bool {
        for name in interface.functions.iter() {
            if !class.methods.contains_key(name) {
                return false;
            }
        }

        true
    }

    fn import_in_module(&mut self, module: &mut Module, path: &str) -> Result<()> {
        let mut components = path.split('.').collect::<Vec<_>>();

        if components.len() < 2 {
            return Err(RuntimeError::new(format!("invalid import path: {}", path)));
        }

        let name = components.pop().unwrap();
        let module_name = components.join(".");

        if module.globals.contains_key(name) {
            return Err(RuntimeError::new(format!(
                "unable to redefine global: {}",
                name
            )));
        }

        if !self.module_cache.contains_module(&module_name) {
            if let Some(path) = self.module_cache.find_module_path(&module_name) {
                let source = fs::read_to_string(&path).map_err(|e| {
                    RuntimeError::new(format!("failed to import module '{}': {}", module_name, e))
                })?;

                let module = utils::parse_and_compile(&source, Some(path)).map_err(|e| {
                    RuntimeError::new(format!(
                        "failed to import module '{}': {:?}",
                        module_name, e
                    ))
                })?;

                self.register_module(module)?;
            } else {
                return Err(RuntimeError::new(format!(
                    "unable to import '{}' module '{}' not found",
                    path, module_name
                )));
            }
        }

        if let Ok(type_val) = self.module_cache.lookup_type(&module_name, name) {
            let (type_name, is_public) = match &type_val.desc {
                TypeDesc::Class(class_desc) => ("Class", class_desc.public),
                TypeDesc::Interface(interface_desc) => ("Interface", interface_desc.public),
                TypeDesc::Function(fn_desc) => ("Fn", fn_desc.public),
            };

            if !is_public {
                return Err(RuntimeError::new(format!(
                    "unable to import private {}: {}'",
                    type_name,
                    self.module_cache.fmt_type(type_val.id),
                )));
            }

            module
                .globals
                .insert(name.to_string(), Value::Type(type_val.id));

            return Ok(());
        }

        Err(RuntimeError::new(format!("unable to import: {}", path)))
    }

    fn eval_tail_call(&mut self, arg_count: usize) -> Result<()> {
        // Reset locals, we can do this quite easily as the first n-locals are the arguments
        let context = self.call_stack.current_mut()?;
        let mut values = self.stack.pop_many(arg_count)?;

        for i in 0..arg_count {
            let local = unsafe { context.locals.get_unchecked_mut(i) };
            let arg_value = unsafe { values.get_unchecked_mut(i) };

            match local {
                Stacked::ByValue(value) => mem::swap(value, arg_value),
                Stacked::ByRef(value_ref) => {
                    let mut value = Value::Void;

                    mem::swap(&mut value, arg_value);

                    let mut arg_value_ref = Rc::new(RefCell::new(value));
                    mem::swap(value_ref, &mut arg_value_ref);
                }
            };
        }

        if context.locals.len() > arg_count {
            context.locals.drain(arg_count..);
        }

        if !context.named_locals.is_empty() {
            context.named_locals.clear();
        }

        Ok(())
    }

    fn eval_call(&mut self, keywords: &[String], arg_count: usize) -> Result<()> {
        let value = self
            .stack
            .pop()
            .map_err(|_| RuntimeError::new("expected function on stack".to_string()))?
            .into_value();

        if let Value::Type(id) = value {
            let type_val = self.module_cache.lookup_type_by_id(id)?;

            if let TypeDesc::Function(_) = &type_val.desc {
                return self.eval_function_call(id, keywords, arg_count);
            }

            if let TypeDesc::Class(_) = &type_val.desc {
                return self.eval_class_init(id, keywords, arg_count);
            }
        }

        if let Value::Method(method) = value {
            return self.eval_method_call(&method, keywords, arg_count);
        }

        Err(RuntimeError::new(format!(
            "type '{}' is not callable",
            value.get_type().name()
        )))
    }

    fn eval_function_call(
        &mut self,
        id: TypeId,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let type_val = self.module_cache.lookup_type_by_id(id)?;
        let func = type_val.try_as_func()?;
        let target = Target {
            type_id: type_val.id,
            module_id: type_val.module_id,
            method_id: None,
        };

        self.call_stack
            .push(CallContext::new(func.pos.clone(), target.clone(), None));

        let return_value = self.eval_func(&target, keywords, arg_count)?;

        self.call_stack.pop();
        self.stack.push(return_value);

        Ok(())
    }

    fn eval_class_init(&mut self, id: TypeId, keywords: &[String], arg_count: usize) -> Result<()> {
        let type_val = self.module_cache.lookup_type_by_id(id)?;
        let class_desc = type_val.try_as_class()?;

        if keywords.len() != arg_count {
            return Err(RuntimeError::new(format!(
                "unable to initialize '{}' with non-keyword arguments",
                self.module_cache.fmt_type(type_val.id),
            )));
        }

        // Verify if there aren't any unknown field names
        for keyword in keywords {
            if !class_desc.fields.contains_key(keyword) {
                return Err(RuntimeError::new(format!(
                    "unable to initialize '{}' with unknown field: {}",
                    self.module_cache.fmt_type(type_val.id),
                    keyword
                )));
            }
        }

        let mut is_sorted = true;

        // Verify if all fields are initialized
        for (i, (field_name, _)) in class_desc.fields.iter().enumerate() {
            if !keywords.contains(field_name) {
                return Err(RuntimeError::new(format!(
                    "unable to initialize '{}' without field: {}",
                    self.module_cache.fmt_type(type_val.id),
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
            let mut fields = self.stack.pop_many_t(class_desc.fields.len(), |value| {
                let index = class_desc.fields.get_index_of(&keywords[i]).unwrap();

                i += 1;

                (index, value)
            })?;

            fields.sort_unstable_by_key(|(i, _)| *i);
            fields.into_iter().map(|(_, value)| value).collect()
        };

        self.stack
            .push(Value::Object(Object::new(type_val.id, fields)));

        Ok(())
    }

    fn eval_method_call(
        &mut self,
        method: &Method,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let type_val = self.module_cache.lookup_type_by_id(method.class_id)?;
        let class_desc = type_val.try_as_class()?;
        let method_desc = class_desc.get_method(method.id)?;
        let target = Target {
            type_id: method.class_id,
            module_id: type_val.module_id,
            method_id: Some(method.id),
        };

        self.call_stack.push(CallContext::new(
            method_desc.func.pos.clone(),
            target.clone(),
            Some(Rc::clone(&method.object)),
        ));

        let return_value = self.eval_func(&target, keywords, arg_count)?;

        self.call_stack.pop();
        self.stack.push(return_value);

        Ok(())
    }

    fn prepare_args(
        &self,
        mut values: Vec<Value>,
        keywords: &[String],
        args: &IndexMap<String, ArgumentDesc>,
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
        target: &Target,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<Value> {
        let func = match target.method_id {
            Some(id) => {
                let class_desc = self
                    .module_cache
                    .lookup_type_by_id(target.type_id)?
                    .try_as_class()?;

                &class_desc.get_method(id)?.func
            }
            None => self
                .module_cache
                .lookup_type_by_id(target.type_id)?
                .try_as_func()?,
        };

        match &func.source {
            FuncSource::Native(source) => {
                if arg_count != func.args.len() {
                    return Err(RuntimeError::new(format!(
                        "invalid argument count for target: {}(...) (expected {}, not {})",
                        self.fmt_target(&target),
                        func.args.len(),
                        arg_count,
                    )));
                }

                if keywords.is_empty() {
                    let mut i = 0;

                    self.call_stack.current_mut()?.locals =
                        self.stack.pop_many_t(arg_count, |value| {
                            let stacked = argument_to_stacked(func, i, value);

                            i += 1;

                            stacked
                        })?;
                } else {
                    let unordered_values = self.stack.pop_many(arg_count)?;
                    let values = self
                        .prepare_args(unordered_values, keywords, &func.args)
                        .map_err(|e| {
                            let message = e.message.clone();
                            e.with_message(format!(
                                "{} in target {}(...)",
                                message,
                                self.fmt_target(&target),
                            ))
                        })?
                        .into_values()
                        .collect::<Vec<Value>>();

                    let call_context = self.call_stack.current_mut()?;

                    for (i, value) in values.into_iter().enumerate() {
                        call_context
                            .locals
                            .push(argument_to_stacked(func, i, value));
                    }
                }

                let source = Rc::clone(source);

                if let Some(result) = self._eval(target.module_id, source)? {
                    return Ok(result);
                }
            }
            FuncSource::External(closure) => {
                if !keywords.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "unable to use keyword arguments in external target: {}(..)",
                        self.fmt_target(&target),
                    )));
                }

                let values = self.stack.pop_many(arg_count)?;

                if let Some(return_value) = closure(self, values)? {
                    return Ok(return_value);
                }
            }
        };

        Ok(Value::Void)
    }

    fn eval_op(
        &mut self,
        name: &str,
        op: impl FnOnce(Value, Value) -> Result<Value>,
    ) -> Result<()> {
        let right = self.stack.pop()?.into_value();
        let left = self.stack.pop()?.into_value();

        self.stack
            .push(op(left, right).map_err(|e| RuntimeError::new(format!("{} in {}", e, name)))?);

        Ok(())
    }

    fn eval_comparison_op(
        &mut self,
        int_op: impl FnOnce(i64, i64) -> bool,
        float_op: impl FnOnce(f64, f64) -> bool,
    ) -> Result<()> {
        let right = self.stack.pop()?.into_value();
        let left = self.stack.pop()?.into_value();

        self.stack.push(
            match left {
                Value::Int(val) => Ok(Value::Bool(int_op(val, to_int(right)?))),
                Value::Float(val) => Ok(Value::Bool(float_op(val, to_float(right)?))),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            }
            .map_err(|e| RuntimeError::new(format!("{} in comparison", e)))?,
        );

        Ok(())
    }

    fn eval_single<'i>(&mut self, module_id: ModuleId, ir: &'i IR) -> Result<Flow<'i>> {
        match &ir.code {
            Code::ConstNil => self.stack.push(Value::Option(None)),
            Code::ConstInt(val) => self.stack.push(Value::Int(*val)),
            Code::ConstBool(val) => self.stack.push(Value::Bool(*val)),
            Code::ConstFloat(val) => self.stack.push(Value::Float(*val)),
            Code::ConstChar(val) => self.stack.push(Value::Char(*val)),
            Code::ConstByte(val) => self.stack.push(Value::Byte(*val)),
            Code::ConstString(val) => self.stack.push(Value::String(val.clone())),
            Code::MakeRange => self.eval_make_range()?,
            Code::MakeArray(len) => self.eval_make_array(*len)?,
            Code::MakeMap(len) => self.eval_make_map(*len)?,
            Code::MakeTemplate(len) => self.eval_make_template(*len)?,
            Code::Discard => self.stack.delete()?,
            Code::Return => return Ok(Flow::Return(self.eval_return()?)),
            Code::MakeRef => self.eval_make_ref()?,
            Code::Deref => self.eval_deref()?,
            Code::LogicalAnd => self.eval_logical_and()?,
            Code::ArithmeticBitOr => self.eval_arithmetic_bit_or()?,
            Code::ArithmeticBitAnd => self.eval_arithmetic_bit_and()?,
            Code::ArithmeticAdd => self.eval_arithmetic_add()?,
            Code::ArithmeticSub => self.eval_arithmetic_sub()?,
            Code::ArithmeticMul => self.eval_arithmetic_mul()?,
            Code::ArithmeticExp => self.eval_arithmetic_exp()?,
            Code::ArithmeticDiv => self.eval_arithmetic_div()?,
            Code::ComparisonEq => self.eval_comparison_eq(true)?,
            Code::ComparisonNeq => self.eval_comparison_eq(false)?,
            Code::ComparisonGt => {
                self.eval_comparison_op(|left, right| left > right, |left, right| left > right)?
            }
            Code::ComparisonGte => {
                self.eval_comparison_op(|left, right| left >= right, |left, right| left >= right)?
            }
            Code::ComparisonLt => {
                self.eval_comparison_op(|left, right| left < right, |left, right| left < right)?
            }
            Code::ComparisonLte => {
                self.eval_comparison_op(|left, right| left <= right, |left, right| left <= right)?
            }
            Code::Not => self.eval_not()?,
            Code::Validate => self.eval_validate()?,
            Code::Cast(type_name) => self.eval_cast(type_name)?,
            Code::Call(arg_count) => self.eval_call(&[], *arg_count)?,
            Code::CallKeywords((keywords, arg_count)) => self.eval_call(keywords, *arg_count)?,
            Code::TailCall(arg_count) => return Ok(Flow::TailCall(*arg_count)),
            Code::Store(id) => self.eval_store(*id, false)?,
            Code::StoreMut(id) => self.eval_store(*id, true)?,
            Code::LoadIndex => self.eval_load_index()?,
            Code::StoreIndex => self.eval_store_index()?,
            Code::LoadMember(member) => self.eval_load_member(module_id, member, false)?,
            Code::TeeMember(member) => self.eval_load_member(module_id, member, true)?,
            Code::StoreMember(member) => self.eval_store_member(module_id, member)?,
            Code::Load(id) => self.eval_load(*id)?,
            Code::LoadSelf => self.eval_load_self()?,
            Code::LoadName(name) => self.eval_load_name(module_id, name)?,
            Code::LoadTarget => self.eval_load_target()?,
            Code::SetLabel(_) => {}
            Code::Branch((true_label, false_label)) => {
                return Ok(Flow::JumpTo(self.eval_branch(true_label, false_label)?))
            }
            Code::Jump(label) => return Ok(Flow::JumpTo(label)),
            Code::JumpIfTrue(label) => return self.eval_jump_if_true(label),
            Code::Raise => self.eval_raise()?,
        };

        Ok(Flow::Continue)
    }

    fn eval_make_range(&mut self) -> Result<()> {
        let to = self.stack.pop().and_then(|s| to_int(s.into_value()))?;
        let from = self.stack.pop().and_then(|s| to_int(s.into_value()))?;

        self.stack.push(Value::Range(from..to));

        Ok(())
    }

    fn eval_make_array(&mut self, len: usize) -> Result<()> {
        let values = self.stack.pop_many(len)?;

        self.stack.push(Value::Array(values));

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

        self.stack.push(Value::Map(map));

        Ok(())
    }

    fn eval_make_template(&mut self, len: usize) -> Result<()> {
        let s = self
            .stack
            .pop_many(len)?
            .into_iter()
            .map(|value| self.fmt_value(&value))
            .collect::<String>();

        self.stack.push(Value::String(s));

        Ok(())
    }

    fn eval_return(&mut self) -> Result<Value> {
        Ok(self.stack.pop()?.into_value())
    }

    fn eval_make_ref(&mut self) -> Result<()> {
        let value = match self.stack.pop()? {
            Stacked::ByValue(value) => Value::Ref(Rc::new(RefCell::new(value))),
            Stacked::ByRef(value_ref) => Value::Ref(Rc::clone(&value_ref)),
        };

        self.stack.push(value);

        Ok(())
    }

    fn eval_deref(&mut self) -> Result<()> {
        let value = self.stack.pop()?.into_value();

        if let Value::Ref(value) = value {
            self.stack.push(value.borrow().clone());

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "unable to dereference: {}",
            value.get_type().name()
        )))
    }

    fn eval_logical_and(&mut self) -> Result<()> {
        self.eval_op("logical and", |left, right| match &left {
            Value::Bool(val) => Ok(Value::Bool(*val && to_bool(right)?)),
            _ => Err(RuntimeError::new(format!(
                "invalid types: {} and {}",
                left.get_type().name(),
                right.get_type().name()
            ))),
        })
    }

    fn eval_arithmetic_bit_or(&mut self) -> Result<()> {
        arithmetic_op!(self, int_only: bitor);

        Ok(())
    }

    fn eval_arithmetic_bit_and(&mut self) -> Result<()> {
        arithmetic_op!(self, int_only: bitand);

        Ok(())
    }

    fn eval_arithmetic_add(&mut self) -> Result<()> {
        arithmetic_op!(self, add);

        Ok(())
    }

    fn eval_arithmetic_sub(&mut self) -> Result<()> {
        arithmetic_op!(self, sub);

        Ok(())
    }

    fn eval_arithmetic_mul(&mut self) -> Result<()> {
        arithmetic_op!(self, mul);

        Ok(())
    }

    fn eval_arithmetic_div(&mut self) -> Result<()> {
        arithmetic_op!(self, div);

        Ok(())
    }

    fn eval_arithmetic_exp(&mut self) -> Result<()> {
        self.eval_op("exponent", |left, right| match &left {
            Value::Int(val) => Ok(Value::Int(val.pow(to_int(right)? as u32))),
            Value::Float(val) => Ok(Value::Float(val.powf(to_float(right)?))),
            _ => Err(RuntimeError::new(format!(
                "invalid types: {} and {}",
                left.get_type().name(),
                right.get_type().name()
            ))),
        })
    }

    fn eval_comparison_eq(&mut self, eq: bool) -> Result<()> {
        let right = self.stack.pop()?.into_value();
        let left = self.stack.pop()?.into_value();

        self.stack.push(Value::Bool((left == right) == eq));

        Ok(())
    }

    fn eval_not(&mut self) -> Result<()> {
        let value = self.stack.pop().and_then(|s| to_bool(s.into_value()))?;

        self.stack.push(Value::Bool(!value));

        Ok(())
    }

    fn eval_validate(&mut self) -> Result<()> {
        let value = self.stack.pop()?.into_value();

        if let Value::Type(id) = &value {
            let type_val = self.module_cache.lookup_type_by_id(*id)?;

            if let Ok(interface) = type_val.try_as_interface() {
                let stacked = self.stack.pop()?;

                {
                    let value = stacked.borrow();
                    let class_id = match &*value {
                        Value::Object(object) => Ok(object.class.clone()),
                        _ => self
                            .module_cache
                            .lookup_type("std.core", value.get_type().name())
                            .and_then(|type_val| Ok(type_val.id)),
                    }?;
                    let class = self
                        .module_cache
                        .lookup_type_by_id(class_id)?
                        .try_as_class()?;

                    if !self.validate_class(class, interface) {
                        return Err(RuntimeError::new(format!(
                            "validation failed, class '{}' doesn't implement interface: {}",
                            self.module_cache.fmt_type(class_id),
                            self.module_cache.fmt_type(type_val.id),
                        )));
                    }
                }

                match stacked {
                    Stacked::ByValue(value) => self.stack.push(value),
                    Stacked::ByRef(value_ref) => self.stack.push_ref(value_ref),
                };

                return Ok(());
            }
        }

        Err(RuntimeError::new(format!(
            "unable to validate non-interface: {}",
            value.get_type().name()
        )))
    }

    fn eval_cast(&mut self, type_name: &str) -> Result<()> {
        let value = self.stack.pop()?.into_value();

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

    fn eval_store(&mut self, id: usize, is_mutable: bool) -> Result<()> {
        let value = self.stack.pop()?.into_value();
        let context = self.call_stack.current_mut()?;
        let locals_len = context.locals.len();
        let stacked = if !value.get_type().is_primitive() || is_mutable {
            Stacked::ByRef(Rc::new(RefCell::new(value)))
        } else {
            Stacked::ByValue(value)
        };

        if id < locals_len {
            let item = unsafe { context.locals.get_unchecked_mut(id) };

            *item = stacked;
        } else {
            context.locals.insert(id, stacked);
        }

        Ok(())
    }

    fn eval_load_index(&mut self) -> Result<()> {
        let index = self.stack.pop()?.into_value();
        let data = self.stack.pop()?.into_value();

        with_auto_deref(&data, |value| {
            if let Value::Array(array) = value {
                let index = to_int(index).map_err(|e| {
                    let message = format!("{} in index lookup", e);

                    e.with_message(message)
                })?;

                if let Some(item) = array.get(index as usize) {
                    self.stack.push(item.clone());

                    return Ok(());
                }

                return Err(RuntimeError::new(
                    format!("index out of bounds: {}", index,),
                ));
            }

            if let Value::Map(map) = value {
                if let Some(item) = map.get(&index) {
                    self.stack.push(item.clone());

                    return Ok(());
                }

                return Err(RuntimeError::new(format!(
                    "index out of bounds: {}",
                    self.fmt_value(&index)
                )));
            }

            Err(RuntimeError::new(format!(
                "unable to index type: {}",
                data.get_type().name()
            )))
        })
    }

    fn eval_store_index(&mut self) -> Result<()> {
        let value = self.stack.pop()?.into_value();
        let index = self.stack.pop()?.into_value();
        let value_ref = self.stack.pop()?.into_ref();
        let mut data = value_ref.borrow_mut();

        with_auto_deref_mut(&mut data, |data| {
            if let Value::Array(array) = &mut *data {
                let index = to_int(index).map_err(|e| {
                    let message = format!("{} in index lookup", e);

                    e.with_message(message)
                })?;

                if array.get(index as usize).is_some() {
                    array[index as usize] = value;

                    return Ok(());
                }

                return Err(RuntimeError::new(
                    format!("index out of bounds: {}", index,),
                ));
            }

            if let Value::Map(map) = &mut *data {
                map.insert(index.clone(), value);

                return Ok(());
            }

            Err(RuntimeError::new(format!(
                "unable to index type: {}",
                data.get_type().name()
            )))
        })
    }

    fn eval_load_member(
        &mut self,
        module_id: ModuleId,
        member: &str,
        push_back: bool,
    ) -> Result<()> {
        let stacked = self.stack.pop()?;
        let id = with_auto_deref(&stacked.borrow(), |value| match value {
            Value::Object(object) => Ok(object.class.clone()),
            _ => self
                .module_cache
                .lookup_type("std.core", value.get_type().name())
                .and_then(|type_val| Ok(type_val.id)),
        })?;
        let type_val = self.module_cache.lookup_type_by_id(id)?;
        let desc = type_val.try_as_class()?;

        if let Some(index) = desc.fields.get_index_of(member) {
            let field = &desc.fields[member];

            if !field.public && module_id != type_val.module_id {
                return Err(RuntimeError::new(format!(
                    "unable to access private field '{}' of class: {}",
                    member,
                    self.module_cache.fmt_type(id),
                )));
            }

            let field = with_auto_deref(&stacked.borrow(), |value| {
                if let Value::Object(object) = value {
                    return object.get_field(index).cloned().ok_or_else(|| {
                        RuntimeError::new(format!(
                            "unable to get unknown field '{}' of class: {}",
                            member,
                            self.module_cache.fmt_type(id),
                        ))
                    });
                }

                Err(RuntimeError::new(format!(
                    "unable to lookup field of class: {}",
                    self.module_cache.fmt_type(id)
                )))
            })?;

            if push_back {
                match stacked {
                    Stacked::ByValue(value) => self.stack.push(value),
                    Stacked::ByRef(value_ref) => self.stack.push_ref(value_ref),
                }
            }

            self.stack.push(field);

            return Ok(());
        }

        if let Some((method_id, _, method)) = desc.methods.get_full(member) {
            if !method.func.public && module_id != type_val.module_id {
                return Err(RuntimeError::new(format!(
                    "unable to access private method '{}(...)' of class: {}",
                    member,
                    self.module_cache.fmt_type(id),
                )));
            }

            let object = match stacked {
                Stacked::ByValue(value) => Rc::new(RefCell::new(value)),
                Stacked::ByRef(value_ref) => Rc::clone(&value_ref),
            };

            if push_back {
                self.stack.push_ref(Rc::clone(&object));
            }

            self.stack.push(Value::Method(
                Method {
                    object,
                    id: method_id,
                    class_id: type_val.id,
                }
                .into(),
            ));

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "no such field or method '{}' for: {}",
            member,
            self.fmt_value(&stacked.borrow()),
        )))
    }

    fn eval_store_member(&mut self, module_id: ModuleId, member: &str) -> Result<()> {
        let object_ref = self.stack.pop()?.into_ref();
        let mut data = object_ref.borrow_mut();
        let class_id = with_auto_deref(&data, |value| match value {
            Value::Object(object) => Ok(object.class.clone()),
            _ => self
                .module_cache
                .lookup_type("std.core", value.get_type().name())
                .and_then(|type_val| Ok(type_val.id)),
        })?;
        let value = self.stack.pop()?.into_value();
        let type_val = self.module_cache.lookup_type_by_id(class_id)?;
        let class_desc = type_val.try_as_class()?;

        if let Some(index) = class_desc.fields.get_index_of(member) {
            let field = &class_desc.fields[member];

            if !field.public && module_id != type_val.module_id {
                return Err(RuntimeError::new(format!(
                    "unable to access private field '{}' of class: {}",
                    member,
                    self.module_cache.fmt_type(class_id)
                )));
            }

            with_auto_deref_mut(&mut data, move |object| {
                Ok(match object {
                    Value::Object(object) => object.set_field_value(index, value),
                    _ => {
                        return Err(RuntimeError::new(format!(
                            "unable to store member on invalid type '{}' expected Object",
                            object.get_type().name(),
                        )));
                    }
                })
            })?;

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "no such field '{}' for class: {}",
            member,
            self.module_cache.fmt_type(class_id),
        )))
    }

    fn eval_load(&mut self, id: usize) -> Result<()> {
        if !self.call_stack.is_empty() {
            let context = self.call_stack.current()?;

            if let Some(stacked) = context.locals.get(id) {
                self.stack.push_stacked(stacked.clone());

                return Ok(());
            }
        }

        Err(RuntimeError::new(format!(
            "undefined local with id: {}",
            id
        )))
    }

    fn eval_load_self(&mut self) -> Result<()> {
        if !self.call_stack.is_empty() {
            let context = self.call_stack.current()?;

            if let Some(this) = &context.this {
                self.stack.push_ref(Rc::clone(this));

                return Ok(());
            }
        }

        Err(RuntimeError::new(format!(
            "unable to load 'this' outside of Fn scope"
        )))
    }

    fn eval_load_name(&mut self, module_id: ModuleId, name: &str) -> Result<()> {
        if !self.call_stack.is_empty() {
            let context = self.call_stack.current()?;

            if let Some(stacked) = context.named_locals.get(name) {
                self.stack.push_stacked(stacked.clone());

                return Ok(());
            }
        }

        let current_module = self.module_cache.lookup_module_by_id(module_id)?;

        if let Some(value) = current_module.globals.get(name).cloned() {
            self.stack.push(value);

            return Ok(());
        }

        if let Some(typedef) = self.module_cache.lookup_module_type_id(module_id, name) {
            self.stack.push(Value::Type(typedef));

            return Ok(());
        }

        Err(RuntimeError::new(format!("no such name: {}", name)))
    }

    fn eval_load_target(&mut self) -> Result<()> {
        if let Some(context) = self.call_stack.last() {
            self.stack.push(Value::Type(context.target.type_id));

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
        let value = self.stack.pop().and_then(|s| to_bool(s.into_value()))?;

        Ok(if value { true_label } else { false_label })
    }

    fn eval_jump_if_true<'i>(&mut self, label: &'i Label) -> Result<Flow<'i>> {
        let value = self.stack.pop().and_then(|s| to_bool(s.into_value()))?;

        if value {
            self.stack.push(Value::Bool(value));

            return Ok(Flow::JumpTo(label));
        }

        Ok(Flow::Continue)
    }

    fn eval_raise(&mut self) -> Result<()> {
        let value = self.stack.pop()?.into_value();

        Err(RuntimeError::new(self.fmt_value(&value)))
    }

    pub fn fmt_target(&self, target: &Target) -> String {
        match target.method_id {
            Some(id) => {
                if let Ok(class) = self
                    .module_cache
                    .lookup_type_by_id(target.type_id)
                    .and_then(|t| t.try_as_class())
                {
                    if let Some((method_name, _)) = class.methods.get_index(id) {
                        return format!(
                            "{}.{}",
                            self.module_cache.fmt_type(target.type_id),
                            method_name
                        );
                    }
                }

                "!".to_string()
            }
            None => self.module_cache.fmt_type(target.type_id),
        }
    }

    pub fn fmt_value(&self, value: &Value) -> String {
        match value {
            Value::Void => "!".to_string(),
            Value::Int(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Char(val) => format!("{}", val),
            Value::Byte(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::Option(val) => match val {
                None => format!("std.core.Option(None)"),
                Some(val) => format!("std.core.Option({})", self.fmt_value(val)),
            },
            Value::Ref(value_ref) => {
                format!("*{}", self.fmt_value(&value_ref.borrow()))
            }
            Value::Range(val) => format!("{}..{}", val.start, val.end),
            Value::String(val) => val.to_string(),
            Value::Type(id) => self.module_cache.fmt_type(*id),
            Value::Method(method) => {
                if let Ok(class) = self
                    .module_cache
                    .lookup_type_by_id(method.class_id)
                    .and_then(|t| t.try_as_class())
                {
                    if let Some((method_name, _)) = class.methods.get_index(method.id) {
                        return format!(
                            "{}.{}(...)",
                            self.module_cache.fmt_type(method.class_id),
                            method_name,
                        );
                    }
                }

                "!".to_string()
            }
            Value::Object(object) => {
                let class_desc = self
                    .module_cache
                    .lookup_type_by_id(object.class)
                    .unwrap()
                    .try_as_class()
                    .unwrap();

                format!(
                    "{}({})",
                    self.module_cache.fmt_type(object.class),
                    class_desc
                        .fields
                        .iter()
                        .map(|(key, field_desc)| format!(
                            "{}{}: {}",
                            if field_desc.public { "*" } else { "" },
                            key,
                            self.fmt_value(object.get_field(field_desc.id).unwrap())
                        ))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            Value::Array(array) => format!(
                "[{}]",
                array
                    .iter()
                    .map(|item| self.fmt_value(item))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Map(map) => format!(
                "{{{}}}",
                map.iter()
                    .map(|(key, value)| format!(
                        "{}: {}",
                        self.fmt_value(key),
                        self.fmt_value(value)
                    ))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
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

    fn _eval(&mut self, module_id: ModuleId, instructions: Rc<Vec<IR>>) -> Result<Option<Value>> {
        let mut i = 0;
        let mut return_addr = vec![];
        let instructions_len = instructions.len();

        loop {
            while i < instructions_len {
                let ir = &instructions[i];

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
                        if e.pos.is_none() {
                            return Err(e
                                .with_pos(ir.pos.clone())
                                .with_module(self.module_cache.lookup_module_by_id(module_id)?));
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
        let module_id = self.module_cache.lookup_module(module)?.id;

        self._eval(module_id, Rc::new(instructions))
            .map_err(|e| e.with_stack_trace(self.call_stack.rewind(&self.module_cache)))?;

        Ok(())
    }

    pub fn result(&mut self) -> Option<Value> {
        self.stack.pop().ok().and_then(|stacked| match stacked {
            Stacked::ByValue(value) => Some(value),
            Stacked::ByRef(value_ref) => Rc::try_unwrap(value_ref)
                .ok()
                .map(|value| value.into_inner()),
        })
    }
}
