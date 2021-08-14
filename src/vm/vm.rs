use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::path::Path;
use std::rc::Rc;

use indexmap::map::IndexMap;
use smallvec::SmallVec;

use crate::compiler::{Code, Label, IR};
use crate::runtime::convert::{to_bool, to_float, to_int};
use crate::runtime::{
    with_auto_deref, with_auto_deref_mut, Method, Object, Result, RuntimeError, TypeId, Value,
};
use crate::std::core::DEFAULT_IMPORTS;
use crate::std::get_middleware;
use crate::utils;
use crate::utils::Error;
use crate::vm::call_stack::CallContext;
use crate::vm::module_cache::{
    ArgumentDesc, FuncSource, InterfaceDesc, Module, ModuleCache, Type, TypeDesc,
};
use crate::vm::stacked::StackedBorrowedMut;
use crate::vm::ClassDesc;

use super::call_stack::CallStack;
use super::stack::Stack;
use super::stacked::Stacked;

enum Flow<'i> {
    JumpTo(&'i Label),
    Return,
    Continue,
}

enum Target {
    Method(TypeId),
    Function(TypeId),
}

impl Target {
    pub fn id(&self) -> &TypeId {
        match self {
            Target::Method(id) => &id,
            Target::Function(id) => &id,
        }
    }
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
        self.module_cache.lookup_type_id(module, name)
    }

    pub fn get_local_mut(&mut self, local_name: &str) -> Option<StackedBorrowedMut<'_>> {
        if let Ok(context) = self.call_stack.current_mut() {
            return context
                .named_locals
                .iter_mut()
                .filter(|(name, _)| name.as_str() == local_name)
                .next()
                .and_then(|(_, stacked)| Some(stacked.borrow_mut()));
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
        let mut components = path.split(".").collect::<Vec<_>>();

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

        if let Ok(type_desc) = self.module_cache.lookup_type(&module_name, name) {
            let value = match type_desc {
                (id, TypeDesc::Class(class_desc)) => {
                    if !class_desc.public {
                        return Err(RuntimeError::new(format!(
                            "unable to import private Class: {}'",
                            self.module_cache.fmt_class(&id),
                        )));
                    }

                    Value::Class(id)
                }
                (id, TypeDesc::Interface(interface_desc)) => {
                    if !interface_desc.public {
                        return Err(RuntimeError::new(format!(
                            "unable to import private Interface: {}'",
                            self.module_cache.fmt_interface(&id),
                        )));
                    }

                    Value::Interface(id)
                }
                (id, TypeDesc::Function(fn_desc)) => {
                    if !fn_desc.public {
                        return Err(RuntimeError::new(format!(
                            "unable to import private Fn: {}(...)'",
                            self.module_cache.fmt_func(&id),
                        )));
                    }

                    Value::Function(id)
                }
            };

            module.globals.insert(name.to_string(), value);

            return Ok(());
        }

        Err(RuntimeError::new(format!("unable to import: {}", path)))
    }

    fn eval_call(&mut self, keywords: &[String], arg_count: usize) -> Result<()> {
        // make sure each keyword is unique
        if !keywords.is_empty() {
            let mut unique_keys = vec![];

            for key in keywords.iter() {
                if unique_keys.contains(key) {
                    return Err(RuntimeError::new(format!(
                        "duplicate keyword argument: {}",
                        key
                    )));
                }

                unique_keys.push(key.to_string());
            }
        }

        let value = self
            .stack
            .pop()
            .map_err(|_| RuntimeError::new("expected function on stack".to_string()))?
            .into_value();

        if let Value::Function(id) = value {
            return self.eval_function_call(&id, keywords, arg_count);
        }

        if let Value::Method(method) = value {
            return self.eval_method_call(&method, keywords, arg_count);
        }

        if let Value::Class(id) = value {
            return self.eval_class_init(&id, keywords, arg_count);
        }

        Err(RuntimeError::new(format!(
            "type '{}' is not callable",
            value.get_type().name()
        )))
    }

    fn eval_class_init(
        &mut self,
        id: &TypeId,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let desc = self.module_cache.lookup_class_by_id(&id)?;

        if keywords.len() != arg_count {
            return Err(RuntimeError::new(format!(
                "unable to initialize {} with non-keyword arguments",
                self.module_cache.fmt_class(id),
            )));
        }

        let mut values = self
            .stack
            .pop_many(arg_count)?
            .into_iter()
            .enumerate()
            .map(|(keyword_idx, value)| {
                let name = &keywords[keyword_idx];
                let arg_idx = desc.fields.get_index_of(name);

                (arg_idx, name.as_str(), value)
            })
            .collect::<Vec<_>>();

        if values.len() != desc.fields.len() {
            let mut field_names = desc
                .fields
                .keys()
                .cloned()
                .map(|key| (key.clone(), key))
                .collect::<HashMap<_, _>>();

            for (_, name, _) in values {
                field_names.remove(name);
            }

            return Err(RuntimeError::new(format!(
                "unable to initialize {} with missing fields: {}",
                self.module_cache.fmt_class(id),
                field_names
                    .into_iter()
                    .map(|(key, _)| key)
                    .collect::<Vec<_>>()
                    .join(", "),
            )));
        }

        values.sort_unstable_by_key(|(index, _, _)| *index);

        let mut fields = SmallVec::with_capacity(values.len());

        for (index, name, value) in values {
            if index.is_some() {
                fields.push(value);
                continue;
            }

            return Err(RuntimeError::new(format!(
                "unable to initialize {} with unknown field: {}",
                self.module_cache.fmt_class(id),
                name,
            )));
        }

        self.stack
            .push(Value::Object(Object::new(id.clone(), fields).into()));

        Ok(())
    }

    fn eval_method_call(
        &mut self,
        method: &Method,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let desc = self.module_cache.lookup_method_by_id(&method.id)?;

        let mut context = CallContext::new_with_locals(
            desc.func.pos.clone(),
            method.id.clone(),
            desc.func.args.len(),
        );

        context.named_locals.insert(
            "this".to_string(),
            Stacked::ByRef(Rc::clone(&method.object)),
        );

        self.call_stack.push(context);

        self.eval_func(Target::Method(method.id.clone()), keywords, arg_count)?;

        let context = self.call_stack.pop().unwrap();

        self.stack
            .push(context.return_value.unwrap_or_else(|| Value::Void));

        Ok(())
    }

    fn prepare_args(
        &self,
        mut values: SmallVec<[Value; 2]>,
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

    fn eval_func(&mut self, target: Target, keywords: &[String], arg_count: usize) -> Result<()> {
        let func = match &target {
            Target::Method(id) => &self.module_cache.lookup_method_by_id(id)?.func,
            Target::Function(id) => self.module_cache.lookup_function_by_id(id)?,
        };

        match &func.source {
            FuncSource::Native(source) => {
                if arg_count != func.args.len() {
                    return Err(RuntimeError::new(format!(
                        "invalid argument count for target: {}(...) (expected {}, not {})",
                        match &target {
                            Target::Method(method) => self.module_cache.fmt_method(method),
                            Target::Function(func) => self.module_cache.fmt_func(func),
                        },
                        func.args.len(),
                        arg_count,
                    )));
                }

                let values = self.stack.pop_many(arg_count)?;
                let ordered_values = if keywords.is_empty() {
                    values
                } else {
                    self.prepare_args(values, keywords, &func.args)
                        .map_err(|e| {
                            let message = e.message.clone();
                            e.with_message(format!(
                                "{} in target {}",
                                message,
                                self.module_cache.fmt_func(target.id()),
                            ))
                        })?
                        .into_values()
                        .collect()
                };

                let mut i = 0;
                let call_context = self.call_stack.current_mut()?;

                for value in ordered_values {
                    let is_mutable = func
                        .args
                        .get_index(i)
                        .and_then(|(_, arg)| Some(arg.mutable))
                        .unwrap_or(false);

                    // Immutable locals can be placed on the stack as their contents will not be changed but we
                    // don't want to do this for non-primitive types because that will be slower in most cases
                    if !value.get_type().is_primitive() || is_mutable {
                        call_context
                            .locals
                            .insert(i, Stacked::ByRef(Rc::new(RefCell::new(value))));
                    } else {
                        call_context.locals.insert(i, Stacked::ByValue(value));
                    }

                    i += 1;
                }

                let source = Rc::clone(source);

                self._eval(target.id().module, source)?;
            }
            FuncSource::External(closure) => {
                if !keywords.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "unable to use keyword arguments in external target: {}(..)",
                        match &target {
                            Target::Method(method) => self.module_cache.fmt_method(method),
                            Target::Function(func) => self.module_cache.fmt_func(func),
                        },
                    )));
                }

                let values = self.stack.pop_many(arg_count)?;

                if let Some(return_value) = closure(self, values)? {
                    self.call_stack.current_mut()?.return_value = Some(return_value);
                }
            }
        };

        Ok(())
    }

    fn eval_function_call(
        &mut self,
        id: &TypeId,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let func = self.module_cache.lookup_function_by_id(id)?;

        self.call_stack.push(CallContext::new_with_locals(
            func.pos.clone(),
            id.clone(),
            func.args.len(),
        ));

        self.eval_func(Target::Function(id.clone()), keywords, arg_count)?;

        let context = self.call_stack.pop().unwrap();

        self.stack
            .push(context.return_value.unwrap_or_else(|| Value::Void));

        Ok(())
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

    fn eval_single<'i>(&mut self, ir: &'i IR) -> Result<Flow<'i>> {
        match &ir.code {
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
            Code::Discard => self.stack.delete()?.into(),
            Code::Return => {
                self.eval_return()?;

                return Ok(Flow::Return);
            }
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
            Code::Call(arg_count) => self.eval_call(&vec![], *arg_count)?.into(),
            Code::CallWithKeywords((keywords, arg_count)) => {
                self.eval_call(keywords, *arg_count)?.into()
            }
            Code::Store(id) => self.eval_store(*id, false)?,
            Code::StoreMut(id) => self.eval_store(*id, true)?,
            Code::LoadIndex => self.eval_load_index()?,
            Code::StoreIndex => self.eval_store_index()?,
            Code::LoadMember(member) => self.eval_load_member(member, false)?,
            Code::TeeMember(member) => self.eval_load_member(member, true)?,
            Code::StoreMember(member) => self.eval_store_member(member)?,
            Code::Load(id) => self.eval_load(*id)?,
            Code::LoadName(name) => self.eval_load_name(name)?,
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

        self.stack.push(Value::Array(values.to_vec()));

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

    fn eval_return(&mut self) -> Result<()> {
        let return_value = self.stack.pop()?.into_value();
        let context = self.call_stack.current_mut()?;

        context.return_value = Some(return_value);

        Ok(())
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
        self.eval_op("bitwise or", |left, right| match &left {
            Value::Int(val) => Ok(Value::Int(*val | to_int(right)?)),
            _ => Err(RuntimeError::new(format!(
                "invalid types: {} and {}",
                left.get_type().name(),
                right.get_type().name()
            ))),
        })
    }

    fn eval_arithmetic_bit_and(&mut self) -> Result<()> {
        self.eval_op("bitwise and", |left, right| match &left {
            Value::Int(val) => Ok(Value::Int(*val & to_int(right)?)),
            _ => Err(RuntimeError::new(format!(
                "invalid types: {} and {}",
                left.get_type().name(),
                right.get_type().name()
            ))),
        })
    }

    fn eval_arithmetic_add(&mut self) -> Result<()> {
        self.eval_op("addition", |left, right| match &left {
            Value::Int(val) => Ok(Value::Int(*val + to_int(right)?)),
            Value::Float(val) => Ok(Value::Float(*val + to_float(right)?)),
            _ => Err(RuntimeError::new(format!(
                "invalid types: {} and {}",
                left.get_type().name(),
                right.get_type().name()
            ))),
        })
    }

    fn eval_arithmetic_sub(&mut self) -> Result<()> {
        self.eval_op("subtraction", |left, right| match &left {
            Value::Int(val) => Ok(Value::Int(*val - to_int(right)?)),
            Value::Float(val) => Ok(Value::Float(*val - to_float(right)?)),
            _ => Err(RuntimeError::new(format!(
                "invalid types: {} and {}",
                left.get_type().name(),
                right.get_type().name()
            ))),
        })
    }

    fn eval_arithmetic_mul(&mut self) -> Result<()> {
        self.eval_op("multiplication", |left, right| match &left {
            Value::Int(val) => Ok(Value::Int(*val * to_int(right)?)),
            Value::Float(val) => Ok(Value::Float(*val * to_float(right)?)),
            _ => Err(RuntimeError::new(format!(
                "invalid types: {} and {}",
                left.get_type().name(),
                right.get_type().name()
            ))),
        })
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

    fn eval_arithmetic_div(&mut self) -> Result<()> {
        self.eval_op("division", |left, right| match &left {
            Value::Int(val) => Ok(Value::Int(*val / to_int(right)?)),
            Value::Float(val) => Ok(Value::Float(*val / to_float(right)?)),
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

        if let Value::Interface(id) = value {
            let interface = self.module_cache.lookup_interface_by_id(&id)?;
            let stacked = self.stack.pop()?;

            {
                let value = stacked.borrow();
                let class_id = match &*value {
                    Value::Object(object) => Ok(object.class.clone()),
                    _ => self
                        .module_cache
                        .lookup_type_id("std.core", value.get_type().name()),
                }?;
                let class = self.module_cache.lookup_class_by_id(&class_id)?;

                if !self.validate_class(class, interface) {
                    return Err(RuntimeError::new(format!(
                        "validation failed, class '{}' doesn't implement interface: {}",
                        self.module_cache.fmt_class(&class_id),
                        self.module_cache.fmt_interface(&id),
                    )));
                }
            }

            match stacked {
                Stacked::ByValue(value) => self.stack.push(value),
                Stacked::ByRef(value_ref) => self.stack.push_ref(value_ref),
            };

            return Ok(());
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

                if let Some(_) = array.get(index as usize) {
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

    fn eval_load_member(&mut self, member: &str, push_back: bool) -> Result<()> {
        let stacked = self.stack.pop()?;
        let id = with_auto_deref(&stacked.borrow(), |value| match value {
            Value::Object(object) => Ok(object.class.clone()),
            _ => self
                .module_cache
                .lookup_type_id("std.core", value.get_type().name()),
        })?;
        let desc = self.module_cache.lookup_class_by_id(&id)?;

        if let Some(index) = desc.fields.get_index_of(member) {
            let field = &desc.fields[member];

            if !field.public && self.module_cache.current_id()? != id.module {
                return Err(RuntimeError::new(format!(
                    "unable to access private field '{}' of class: {}",
                    member,
                    self.module_cache.fmt_class(&id),
                )));
            }

            let field = with_auto_deref(&stacked.borrow(), |value| {
                if let Value::Object(object) = value {
                    return object
                        .get_field(index)
                        .and_then(|value| Some(value.clone()))
                        .ok_or_else(|| {
                            RuntimeError::new(format!(
                                "unable to get unknown field '{}' of class: {}",
                                member,
                                self.module_cache.fmt_class(&id),
                            ))
                        });
                }

                Err(RuntimeError::new(format!(
                    "unable to lookup field of class: {}",
                    self.module_cache.fmt_class(&id)
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

        if let Some(index) = desc.methods.get_index_of(member) {
            let method = &desc.methods[member];

            if !method.func.public && self.module_cache.current_id()? != id.module {
                return Err(RuntimeError::new(format!(
                    "unable to access private method '{}(...)' of class: {}",
                    member,
                    self.module_cache.fmt_class(&id),
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
                    id: TypeId::new_with_class(id.module, index, id.name),
                    name: member.to_string(),
                    object,
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

    fn eval_store_member(&mut self, member: &str) -> Result<()> {
        let object_ref = self.stack.pop()?.into_ref();
        let mut data = object_ref.borrow_mut();
        let class_id = with_auto_deref(&data, |value| match value {
            Value::Object(object) => Ok(object.class.clone()),
            _ => self
                .module_cache
                .lookup_type_id("std.core", value.get_type().name()),
        })?;
        let value = self.stack.pop()?.into_value();
        let desc = self.module_cache.lookup_class_by_id(&class_id)?;

        if let Some(index) = desc.fields.get_index_of(member) {
            let field = &desc.fields[member];

            if !field.public && self.module_cache.current_id()? != class_id.module {
                return Err(RuntimeError::new(format!(
                    "unable to access private field '{}' of class: {}.{}",
                    member, class_id.module, class_id.name
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
            "no such field '{}' for class: {}.{}",
            member, class_id.module, class_id.name,
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

    fn eval_load_name(&mut self, name: &str) -> Result<()> {
        if !self.call_stack.is_empty() {
            let context = self.call_stack.current()?;

            if let Some(stacked) = context.named_locals.get(name) {
                self.stack.push_stacked(stacked.clone());

                return Ok(());
            }
        }

        let current_module = self.module_cache.current()?;

        if let Some(value) = current_module
            .globals
            .get(name)
            .and_then(|value| Some(value.clone()))
        {
            self.stack.push(value);

            return Ok(());
        }

        if let Some(typedef) = current_module.find_type(name) {
            self.stack.push(match typedef {
                Type::Class(id) => Value::Class(TypeId::new(current_module.id, id)),
                Type::Function(id) => Value::Function(TypeId::new(current_module.id, id)),
                Type::Interface(id) => Value::Interface(TypeId::new(current_module.id, id)),
            });

            return Ok(());
        }

        Err(RuntimeError::new(format!("no such name: {}", name)))
    }

    fn eval_load_target(&mut self) -> Result<()> {
        if let Some(context) = self.call_stack.last() {
            self.stack.push(Value::Function(TypeId::new(
                context.target.module,
                context.target.name,
            )));

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

    pub fn fmt_value(&self, value: &Value) -> String {
        match value {
            Value::Void => "!".to_string(),
            Value::Int(val) => format!("{}", val),
            Value::Float(val) => format!("{}", val),
            Value::Char(val) => format!("{}", val),
            Value::Byte(val) => format!("{}", val),
            Value::Bool(val) => format!("{}", val),
            Value::Ref(value_ref) => {
                format!("*{}", self.fmt_value(&value_ref.borrow()))
            }
            Value::Range(val) => format!("{}..{}", val.start, val.end),
            Value::String(val) => format!("{}", val),
            Value::Class(id) => format!("{}", self.module_cache.fmt_class(id)),
            Value::Function(id) => format!("{}(...)", self.module_cache.fmt_func(id)),
            Value::Interface(id) => format!("{}", self.module_cache.fmt_interface(id)),
            Value::Method(method) => {
                format!("{}(...)", self.module_cache.fmt_method(&method.id))
            }
            Value::Object(object) => {
                let class_desc = self.module_cache.lookup_class_by_id(&object.class).unwrap();

                format!(
                    "{}({})",
                    self.module_cache.fmt_class(&object.class),
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

    fn find_label(&self, instructions: &Vec<IR>, search: &str) -> Result<usize> {
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

    fn _eval(&mut self, new_module_id: usize, instructions: Rc<Vec<IR>>) -> Result<()> {
        let module_id = self.module_cache.current_id().ok();

        self.module_cache.set_current(new_module_id);

        let mut i = 0;

        while i < instructions.len() {
            let ir = &instructions[i];

            // evaluates the instruction and optionally returns a label to jump to
            match self.eval_single(ir) {
                Ok(flow) => match flow {
                    Flow::Continue => i += 1,
                    Flow::Return => {
                        break;
                    }
                    Flow::JumpTo(label) => {
                        i = match label.index {
                            Some(index) => index,
                            None => self.find_label(&*instructions, &label.name)?,
                        };

                        continue;
                    }
                },
                Err(e) => {
                    if e.pos.is_none() {
                        return Err(e.with_pos(ir.pos.clone()));
                    };

                    return Err(e);
                }
            };
        }

        if let Some(id) = module_id {
            self.module_cache.set_current(id);
        }

        Ok(())
    }

    pub fn eval(&mut self, module: &str, instructions: Vec<IR>) -> Result<()> {
        let module_id = self.module_cache.lookup_module(module)?.id;

        self._eval(module_id, Rc::new(instructions)).map_err(|e| {
            let e = e.with_stack_trace(self.call_stack.rewind(&self.module_cache));

            if let Ok(module) = self.module_cache.current() {
                return e.with_module(module);
            }

            e
        })?;

        Ok(())
    }

    pub fn result(&mut self) -> Option<Value> {
        self.stack.pop().ok().and_then(|stacked| match stacked {
            Stacked::ByValue(value) => Some(value),
            Stacked::ByRef(value_ref) => Rc::try_unwrap(value_ref)
                .ok()
                .and_then(|value| Some(value.into_inner())),
        })
    }
}
