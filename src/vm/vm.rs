use std::cell::{RefCell, RefMut};
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::hash::Hash;
use std::path::Path;
use std::rc::Rc;

use indexmap::map::IndexMap;
use smallvec::SmallVec;

use crate::compiler::{Code, IR};
use crate::runtime::convert::{to_bool, to_float, to_int, to_object};
use crate::runtime::{
    with_auto_deref, with_auto_deref_mut, Method, Object, Result, RuntimeError, TypeId, Value,
    ValueType,
};
use crate::std::core::DEFAULT_IMPORTS;
use crate::std::get_middleware;
use crate::utils;
use crate::utils::Error;
use crate::vm::call_stack::CallContext;
use crate::vm::module_cache::{
    ArgumentDesc, FuncSource, InterfaceDesc, Module, ModuleCache, Type, TypeDesc,
};
use crate::vm::stack::Stacked;
use crate::vm::ClassDesc;

use super::call_stack::CallStack;
use super::stack::Stack;

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

fn get_cloned<K: Eq + Hash, V: Clone>(map: &HashMap<K, V>, key: &K) -> Option<V> {
    map.get(key).and_then(|value| Some(value.clone()))
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

    pub fn get_local_mut(&mut self, local_name: &str) -> Option<RefMut<Value>> {
        if let Ok(context) = self.call_stack.current_mut() {
            return context
                .named_locals
                .iter_mut()
                .filter(|(name, _)| name.as_str() == local_name)
                .next()
                .and_then(|(_, value)| Some(value.borrow_mut()));
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
            .map_err(|_| RuntimeError::new("expected function on stack".to_string()))?;

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

        let mut fields: SmallVec<[Value; 5]> = SmallVec::with_capacity(values.len());

        for (index, name, value) in values {
            if index.is_some() {
                fields.push(value);
                continue;
            }

            return Err(RuntimeError::new(format!(
                "unable to initialize {}.{} without field: {}",
                id.module, id.name, name,
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

        let mut context = CallContext::new(desc.func.pos.clone(), method.id.clone());

        context.named_locals.insert(
            "this".to_string(),
            if method.object.borrow().get_type() == ValueType::Ref {
                Rc::clone(&method.object)
            } else {
                let object = to_object(&self.module_cache, method.object.borrow().clone())?;

                Rc::new(RefCell::new(Value::Object(object.into())))
            },
        );

        self.call_stack.push(context);

        self.eval_func(Target::Method(method.id.clone()), keywords, arg_count)?;

        let context = self.call_stack.pop().unwrap();

        self.stack
            .push(context.return_value.unwrap_or_else(|| Value::Invalid));

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

                if keywords.is_empty() {
                    let call_context = self.call_stack.current_mut()?;

                    for (i, value) in values.into_iter().enumerate() {
                        call_context.locals.insert(i, Rc::new(RefCell::new(value)));
                    }
                } else {
                    let ordered_values =
                        self.prepare_args(values, keywords, &func.args)
                            .map_err(|e| {
                                let message = e.message.clone();
                                e.with_message(format!(
                                    "{} in target {}",
                                    message,
                                    self.module_cache.fmt_func(target.id()),
                                ))
                            })?;
                    let call_context = self.call_stack.current_mut()?;

                    for (i, value) in ordered_values {
                        call_context.locals.insert(i, Rc::new(RefCell::new(value)));
                    }
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

        self.call_stack
            .push(CallContext::new(func.pos.clone(), id.clone()));

        self.eval_func(Target::Function(id.clone()), keywords, arg_count)?;

        let context = self.call_stack.pop().unwrap();

        self.stack
            .push(context.return_value.unwrap_or_else(|| Value::Invalid));

        Ok(())
    }

    fn eval_op(
        &mut self,
        name: &str,
        op: impl FnOnce(Value, Value) -> Result<Value>,
    ) -> Result<()> {
        let right = self.stack.pop()?;
        let left = self.stack.pop()?;

        self.stack
            .push(op(left, right).map_err(|e| RuntimeError::new(format!("{} in {}", e, name)))?);

        Ok(())
    }

    fn eval_comparison_op(&mut self, op: impl FnOnce(Ordering) -> bool) -> Result<()> {
        self.eval_op("ordering comparison", |left, right| match left {
            Value::Int(val) => Ok(Value::Bool(op(val.cmp(&to_int(right)?)))),
            Value::Float(val) => {
                if let Some(ord) = val.partial_cmp(&to_float(right)?) {
                    return Ok(Value::Bool(op(ord)));
                }

                Err(RuntimeError::new(
                    "unable to compare invalid Float".to_string(),
                ))
            }
            _ => Err(RuntimeError::new(format!(
                "invalid types: {} and {}",
                left.get_type().name(),
                right.get_type().name()
            ))),
        })
    }

    fn eval_single<'i>(&mut self, ir: &'i IR) -> Result<Option<&'i str>> {
        match &ir.code {
            Code::ConstInt(val) => self.stack.push(Value::Int(*val)),
            Code::ConstBool(val) => self.stack.push(Value::Bool(*val)),
            Code::ConstFloat(val) => self.stack.push(Value::Float(*val)),
            Code::ConstChar(val) => self.stack.push(Value::Char(*val)),
            Code::ConstByte(val) => self.stack.push(Value::Byte(*val)),
            Code::ConstString(val) => self.stack.push(Value::String(val.clone())),
            Code::MakeRange => {
                let to = self.stack.pop().and_then(to_int)?;
                let from = self.stack.pop().and_then(to_int)?;

                self.stack.push(Value::Range(from..to));
            }
            Code::MakeArray(len) => {
                let values = self.stack.pop_many(*len)?;

                self.stack.push(Value::Array(values));
            }
            Code::MakeMap(len) => {
                let mut map = HashMap::new();
                let mut key_values = self.stack.pop_many(len * 2)?;

                for _ in 0..*len {
                    let key = key_values.remove(0);
                    let value = key_values.remove(0);

                    map.insert(key, value);
                }

                self.stack.push(Value::Map(map));
            }
            Code::Discard => self.stack.delete()?.into(),
            Code::Return => {
                let return_value = self.stack.pop()?;
                let context = self.call_stack.current_mut()?;

                context.finished = true;
                context.return_value = Some(return_value);
            }
            Code::MakeRef => {
                let value = match self.stack.pop_stacked()? {
                    Stacked::ByValue(value) => Value::Ref(Rc::new(RefCell::new(value))),
                    Stacked::ByRef(value_ref) => Value::Ref(Rc::clone(&value_ref)),
                };

                self.stack.push(value);
            }
            Code::Deref => {
                let value = self.stack.pop()?;

                if let Value::Ref(value) = value {
                    self.stack.push(value.borrow().clone());

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!(
                    "unable to dereference: {}",
                    value.get_type().name()
                )));
            }
            Code::LogicalAnd => self.eval_op("logical and", |left, right| match &left {
                Value::Bool(val) => Ok(Value::Bool(*val && to_bool(right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticBitOr => self.eval_op("bitwise or", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val | to_int(right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticBitAnd => self.eval_op("bitwise and", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val & to_int(right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticAdd => self.eval_op("addition", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val + to_int(right)?)),
                Value::Float(val) => Ok(Value::Float(*val + to_float(right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticSub => self.eval_op("subtraction", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val - to_int(right)?)),
                Value::Float(val) => Ok(Value::Float(*val - to_float(right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticMul => self.eval_op("multiplication", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val * to_int(right)?)),
                Value::Float(val) => Ok(Value::Float(*val * to_float(right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticDiv => self.eval_op("division", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val / to_int(right)?)),
                Value::Float(val) => Ok(Value::Float(*val / to_float(right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ComparisonEq => {
                let right = self.stack.pop()?;
                let left = self.stack.pop()?;

                self.stack.push(Value::Bool(left == right));
            }
            Code::ComparisonNeq => {
                let right = self.stack.pop()?;
                let left = self.stack.pop()?;

                self.stack.push(Value::Bool(left != right));
            }
            Code::ComparisonGt => self.eval_comparison_op(|ord| ord == Ordering::Greater)?,
            Code::ComparisonGte => {
                self.eval_comparison_op(|ord| ord == Ordering::Greater || ord == Ordering::Equal)?
            }
            Code::ComparisonLt => self.eval_comparison_op(|ord| ord == Ordering::Less)?.into(),
            Code::ComparisonLte => {
                self.eval_comparison_op(|ord| ord == Ordering::Less || ord == Ordering::Equal)?
            }
            Code::Not => {
                let value = self.stack.pop().and_then(to_bool)?;

                self.stack.push(Value::Bool(!value));
            }
            Code::Validate => {
                let value = self.stack.pop()?;

                if let Value::Interface(id) = value {
                    let interface = self.module_cache.lookup_interface_by_id(&id)?;
                    let stacked = self.stack.pop_stacked()?;

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

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!(
                    "unable to validate non-interface: {}",
                    value.get_type().name()
                )));
            }
            Code::Cast(type_name) => {
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
            }
            Code::Call(arg_count) => self.eval_call(&vec![], *arg_count)?.into(),
            Code::CallWithKeywords((keywords, arg_count)) => {
                self.eval_call(keywords, *arg_count)?.into()
            }
            Code::Store(id) => {
                let value = self.stack.pop()?;

                self.call_stack
                    .current_mut()?
                    .locals
                    .insert(id.clone(), Rc::new(RefCell::new(value)));
            }
            Code::StoreMut(id) => {
                let value = self.stack.pop()?;

                self.call_stack
                    .current_mut()?
                    .locals
                    .insert(id.clone(), Rc::new(RefCell::new(value)));
            }
            Code::LoadIndex => {
                let index = self.stack.pop()?;
                let data = self.stack.pop()?;

                if let Value::Array(array) = data {
                    let index = to_int(index).map_err(|e| {
                        let message = format!("{} in index lookup", e);

                        e.with_message(message)
                    })?;

                    if let Some(item) = array.get(index as usize) {
                        self.stack.push(item.clone());

                        return Ok(None);
                    }

                    return Err(RuntimeError::new(
                        format!("index out of bounds: {}", index,),
                    ));
                }

                if let Value::Map(map) = data {
                    if let Some(item) = map.get(&index) {
                        self.stack.push(item.clone());

                        return Ok(None);
                    }

                    return Err(RuntimeError::new(format!("index out of bounds: {}", index)));
                }

                return Err(RuntimeError::new(format!(
                    "unable to index type: {}",
                    data.get_type().name()
                )));
            }
            Code::StoreIndex => {
                let value = self.stack.pop()?;
                let index = self.stack.pop()?;
                let value_ref = self.stack.pop_ref()?;
                let mut data = value_ref.borrow_mut();

                if let Value::Array(array) = &mut *data {
                    let index = to_int(index).map_err(|e| {
                        let message = format!("{} in index lookup", e);

                        e.with_message(message)
                    })?;

                    if let Some(_) = array.get(index as usize) {
                        array[index as usize] = value;

                        return Ok(None);
                    }

                    return Err(RuntimeError::new(
                        format!("index out of bounds: {}", index,),
                    ));
                }

                if let Value::Map(map) = &mut *data {
                    if let Some(_) = map.get(&index) {
                        map.insert(index, value);

                        return Ok(None);
                    }

                    return Err(RuntimeError::new(format!("index out of bounds: {}", index)));
                }

                return Err(RuntimeError::new(format!(
                    "unable to index type: {}",
                    data.get_type().name()
                )));
            }
            Code::LoadMember(member) | Code::TeeMember(member) => {
                let stacked = self.stack.pop_stacked()?;
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
                        match value {
                            Value::Object(object) => object
                                .get_field(index)
                                .and_then(|value| Some(value.clone())),
                            _ => {
                                let object = to_object(&self.module_cache, value.clone())?;

                                object
                                    .get_field(index)
                                    .and_then(|value| Some(value.clone()))
                            }
                        }
                        .ok_or_else(|| {
                            RuntimeError::new(format!(
                                "unable to get unknown field '{}' of class: {}",
                                member,
                                self.module_cache.fmt_class(&id),
                            ))
                        })
                    })?;

                    if let Code::TeeMember(_) = ir.code {
                        match stacked {
                            Stacked::ByValue(value) => self.stack.push(value),
                            Stacked::ByRef(value_ref) => self.stack.push_ref(value_ref),
                        }
                    }

                    self.stack.push(field);

                    return Ok(None);
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

                    if let Code::TeeMember(_) = ir.code {
                        self.stack.push_ref(Rc::clone(&object));
                    }

                    self.stack.push(Value::Method(
                        Method {
                            id: TypeId::new_with_class(id.module, index, id.name),
                            name: member.clone(),
                            object,
                        }
                        .into(),
                    ));

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!(
                    "no such field or method '{}' for class: {}",
                    member,
                    self.module_cache.fmt_class(&id),
                )));
            }
            Code::StoreMember(member) => {
                let object_ref = self.stack.pop_ref()?;
                let mut data = object_ref.borrow_mut();
                let class_id = with_auto_deref(&data, |value| match value {
                    Value::Object(object) => Ok(object.class.clone()),
                    _ => self
                        .module_cache
                        .lookup_type_id("std.core", value.get_type().name()),
                })?;
                let value = self.stack.pop()?;
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

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!(
                    "no such field '{}' for class: {}.{}",
                    member, class_id.module, class_id.name,
                )));
            }
            Code::Load(id) => {
                if !self.call_stack.is_empty() {
                    let context = self.call_stack.current()?;
                    let value = context
                        .locals
                        .get(*id)
                        .and_then(|value| Some(Rc::clone(value)));

                    if let Some(value) = value {
                        self.stack.push_ref(value);

                        return Ok(None);
                    }
                }

                return Err(RuntimeError::new(format!(
                    "undefined local with id: {}",
                    id
                )));
            }
            Code::LoadName(name) => {
                if !self.call_stack.is_empty() {
                    let context = self.call_stack.current()?;
                    let value = context
                        .named_locals
                        .get(name)
                        .and_then(|value| Some(Rc::clone(value)));

                    if let Some(value) = value {
                        self.stack.push_ref(value);

                        return Ok(None);
                    }
                }

                let current_module = self.module_cache.current()?;

                if let Some(value) = get_cloned(&current_module.globals, name) {
                    self.stack.push(value);

                    return Ok(None);
                }

                if let Some(typedef) = current_module.find_type(name) {
                    self.stack.push(match typedef {
                        Type::Class(id) => Value::Class(TypeId::new(current_module.id, id)),
                        Type::Function(id) => Value::Function(TypeId::new(current_module.id, id)),
                        Type::Interface(id) => Value::Interface(TypeId::new(current_module.id, id)),
                    });

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!("no such name: {}", name)));
            }
            Code::SetLabel(_) => {}
            Code::Branch((true_label, false_label)) => {
                let value = self.stack.pop().and_then(to_bool)?;

                return Ok(Some(if value { true_label } else { false_label }));
            }
            Code::Jump(label) => return Ok(Some(label)),
            Code::JumpIfTrue(label) => {
                let value = self.stack.pop().and_then(to_bool)?;

                if value {
                    self.stack.push(Value::Bool(value));

                    return Ok(Some(label));
                }
            }
            Code::Raise => {
                let value = self.stack.pop()?;

                return Err(RuntimeError::new(value.to_string()));
            }
        };

        Ok(None)
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

        let mut eval = || {
            let mut i = 0;

            while i < instructions.len() {
                let ir = &instructions[i];
                let pos = ir.pos.clone();

                // evaluates the instruction and optionally returns a label to jump to
                if let Some(label) = self.eval_single(ir).map_err(|e| {
                    if e.pos.is_none() {
                        return e.with_pos(pos);
                    } else {
                        e
                    }
                })? {
                    i = self.find_label(&*instructions, &label)?;
                    continue;
                }

                // break on early return
                if let Some(context) = self.call_stack.last() {
                    if context.finished {
                        break;
                    }
                }

                i += 1;
            }

            Ok(())
        };

        let result = eval();

        if let Some(id) = module_id {
            self.module_cache.set_current(id);
        }

        result
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
        self.stack
            .pop_stacked()
            .ok()
            .and_then(|stacked| match stacked {
                Stacked::ByValue(value) => Some(value),
                Stacked::ByRef(value_ref) => Rc::try_unwrap(value_ref)
                    .ok()
                    .and_then(|value| Some(value.into_inner())),
            })
    }
}
