use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::fs;
use std::hash::Hash;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::compiler::{Code, LocalId, IR};
use crate::runtime::convert::{to_bool, to_float, to_int, to_object};
use crate::runtime::{ClassId, FuncId, Method, Object, PointerType, Result, RuntimeError, Value};
use crate::std::core::{register, DEFAULT_IMPORTS};
use crate::utils;
use crate::utils::Error;
use crate::vm::call_stack::CallContext;
use crate::vm::module_cache::{FuncDesc, FuncSource, Module, ModuleCache};

use super::call_stack::CallStack;
use super::stack::Stack;

fn get_cloned<K: Eq + Hash, V: Clone>(map: &HashMap<K, V>, key: &K) -> Option<V> {
    map.get(key).and_then(|value| Some(value.clone()))
}

pub struct VM {
    stack: Stack,
    call_stack: CallStack,
    module_cache: ModuleCache,
    module_paths: Vec<PathBuf>,
}

impl VM {
    pub fn new() -> Result<Self> {
        let mut vm = Self {
            stack: Stack::new(),
            call_stack: CallStack::new(),
            module_cache: ModuleCache::new(),
            module_paths: vec![],
        };

        let std_core =
            utils::parse_and_compile(include_str!("../std/core.atom")).map_err(|e| match e {
                Error::Runtime(e) => e,
                Error::Compile(e) => {
                    RuntimeError::new(format!("failed to compile 'std/core.atom': {}", e))
                        .with_pos(e.pos)
                }
                Error::ParseError(e) => {
                    RuntimeError::new(format!("failed to parse 'std/core.atom': {}", e))
                        .with_pos(e.location.offset..e.location.offset + 1)
                }
            })?;

        let mut std_module = Module::new(std_core, Some("std/core.atom".into()));

        register(&mut std_module)?;

        vm.register_module(std_module)?;

        Ok(vm)
    }

    pub fn add_module_path(&mut self, path: impl AsRef<Path>) {
        self.module_paths.push(path.as_ref().to_path_buf());
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

        self.module_cache.add(module);

        Ok(())
    }

    pub fn get_local(&self, name: &str) -> Option<Value> {
        if let Ok(context) = self.call_stack.current() {
            return context
                .locals
                .iter()
                .filter(|(id, _)| id.scope_hint.is_none() && id.name == name)
                .next()
                .and_then(|(_, value)| Some(value.clone()));
        }

        None
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

        // try to load the module from the filesystem if it doesn't exist
        if !self.module_cache.contains_module(&module_name) {
            let mut filename = None;

            for module_path in self.module_paths.iter() {
                let mut path = module_path.clone();

                for component in components.iter().take(components.len() - 1) {
                    path.push(component);
                }

                if let Some(last_component) = components.last() {
                    path.push(format!("{}.atom", last_component));
                }

                if !path.exists() {
                    continue;
                }

                filename = Some(path);

                break;
            }

            if let Some(path) = filename {
                let source = fs::read_to_string(&path).map_err(|e| {
                    RuntimeError::new(format!("failed to import module '{}': {}", module_name, e))
                })?;

                let compiled_module = utils::parse_and_compile(&source).map_err(|e| {
                    RuntimeError::new(format!(
                        "failed to import module '{}': {:?}",
                        module_name, e
                    ))
                })?;

                self.register_module(Module::new(compiled_module, Some(path)))?;
            }
        }

        // first, let's try a function
        if let Ok(func) = self.module_cache.lookup_function(&module_name, name) {
            if !func.public {
                return Err(RuntimeError::new(format!(
                    "unable to import private Fn: {}.{}(...)'",
                    module_name, name,
                )));
            }

            module.globals.insert(
                name.to_string(),
                Value::Function(FuncId {
                    name: name.to_string(),
                    module: module_name.clone(),
                }),
            );

            return Ok(());
        }

        // no? well, maybe it's a class
        if let Ok(class) = self.module_cache.lookup_class(&module_name, name) {
            if !class.public {
                return Err(RuntimeError::new(format!(
                    "unable to import private Class: {}.{}(...)'",
                    module_name, name,
                )));
            }

            module.globals.insert(
                name.to_string(),
                Value::Class(ClassId {
                    name: name.to_string(),
                    module: module_name.clone(),
                }),
            );

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

        if let Value::Method(id) = value {
            return self.eval_method_call(&id, keywords, arg_count);
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
        id: &ClassId,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let desc = self.module_cache.lookup_class(&id.module, &id.name)?;

        if keywords.len() != arg_count {
            return Err(RuntimeError::new(format!(
                "unable to initialize {}.{} with non-keyword arguments",
                id.module, id.name
            )));
        }

        let mut object = Object {
            class: id.clone(),
            fields: vec![],
        };

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
                "unable to initialize {}.{} with missing fields: {}",
                id.module,
                id.name,
                field_names
                    .into_iter()
                    .map(|(key, _)| key)
                    .collect::<Vec<_>>()
                    .join(", "),
            )));
        }

        values.sort_unstable_by_key(|(index, _, _)| *index);

        for (index, name, value) in values {
            if index.is_some() {
                object.fields.push(value);
                continue;
            }

            return Err(RuntimeError::new(format!(
                "unable to initialize {}.{} without field: {}",
                id.module, id.name, name,
            )));
        }

        self.stack
            .push(Value::Object(Rc::new(RefCell::new(object))));

        Ok(())
    }

    fn eval_method_call(
        &mut self,
        method: &Method,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let (module, class) = {
            let object = method.object.borrow();
            (object.class.module.clone(), object.class.name.clone())
        };
        let desc = self
            .module_cache
            .lookup_method(&module, &class, &method.name)?;

        let mut context = CallContext::new(
            desc.func.pos.clone(),
            FuncId {
                module: module.clone(),
                name: format!("{}.{}", class, method.name),
            },
        );

        context.locals.insert(
            LocalId::new("this".to_string()),
            Value::Object(Rc::clone(&method.object)),
        );

        self.call_stack.push(context);

        let func = desc.func.clone();

        self.eval_func(
            &module,
            &format!("{}.{}", class, method.name),
            keywords,
            arg_count,
            func,
        )?;

        let context = self.call_stack.pop().unwrap();

        self.stack
            .push(context.return_value.unwrap_or_else(|| Value::Invalid));

        Ok(())
    }

    fn prepare_args(
        &self,
        mut values: Vec<Value>,
        keywords: &[String],
        args: &Vec<String>,
    ) -> Result<BTreeMap<usize, Value>> {
        let mut ordered_values = BTreeMap::new();

        for name in keywords.iter() {
            if let Ok(arg_idx) = args.binary_search(name) {
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
        module_name: &str,
        name: &str,
        keywords: &[String],
        arg_count: usize,
        func: FuncDesc,
    ) -> Result<()> {
        match func.source {
            FuncSource::Native(source) => {
                if arg_count != func.args.len() {
                    return Err(RuntimeError::new(format!(
                        "invalid argument count for Fn: {}.{}(...) (expected {}, not {})",
                        module_name,
                        name,
                        func.args.len(),
                        arg_count,
                    )));
                }

                let values = self.stack.pop_many(arg_count)?;
                let arg_names: Vec<String> = func.args.keys().cloned().collect();

                if keywords.is_empty() {
                    let call_context = self.call_stack.current_mut()?;

                    for (i, value) in values.into_iter().enumerate() {
                        call_context.args.insert(arg_names[i].to_string(), value);
                    }
                } else {
                    let ordered_values =
                        self.prepare_args(values, keywords, &arg_names)
                            .map_err(|e| {
                                let message = e.message.clone();
                                e.with_message(format!(
                                    "{} in Fn {}.{}",
                                    message, module_name, name,
                                ))
                            })?;
                    let call_context = self.call_stack.current_mut()?;

                    for (i, value) in ordered_values {
                        call_context.args.insert(arg_names[i].to_string(), value);
                    }
                }

                let source = Rc::clone(&source);
                self._eval(&module_name, source)?;
            }
            FuncSource::External(closure) => {
                if !keywords.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "unable to use keyword arguments in external Fn: {}.{}",
                        module_name, name,
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
        id: &FuncId,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let func = self
            .module_cache
            .lookup_function(&id.module, &id.name)?
            .clone();

        self.call_stack
            .push(CallContext::new(func.pos.clone(), id.clone()));

        self.eval_func(&id.module, &id.name, keywords, arg_count, func)?;

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

    fn eval_single(&mut self, ir: &IR) -> Result<Option<String>> {
        match &ir.code {
            Code::ConstInt(val) => self.stack.push(Value::Int(*val)),
            Code::ConstBool(val) => self.stack.push(Value::Bool(*val)),
            Code::ConstFloat(val) => self.stack.push(Value::Float(*val)),
            Code::ConstChar(val) => self.stack.push(Value::Char(*val)),
            Code::ConstString(val) => self.stack.push(Value::String(val.clone())),
            &Code::MakeRange => {
                let to = self.stack.pop().and_then(to_int)?;
                let from = self.stack.pop().and_then(to_int)?;

                self.stack.push(Value::Range(from..to));
            }
            Code::MakeArray(len) => {
                let values = self.stack.pop_many(*len)?;

                self.stack.push(Value::Array(Rc::new(RefCell::new(values))));
            }
            Code::MakeMap(len) => {
                let mut map = HashMap::new();
                let mut key_values = self.stack.pop_many(len * 2)?;

                for _ in 0..*len {
                    let key = key_values.remove(0);
                    let value = key_values.remove(0);

                    map.insert(key, value);
                }

                self.stack.push(Value::Map(Rc::new(RefCell::new(map))));
            }
            Code::Discard => self.stack.delete()?.into(),
            Code::Return => {
                let return_value = self.stack.pop()?;

                self.call_stack.current_mut()?.return_value = Some(return_value);
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
            Code::Call(arg_count) => self.eval_call(&vec![], *arg_count)?.into(),
            Code::CallWithKeywords((keywords, arg_count)) => {
                self.eval_call(keywords, *arg_count)?.into()
            }
            Code::Store(id) => {
                let value = self.stack.pop()?;

                self.call_stack
                    .current_mut()?
                    .locals
                    .insert(id.clone(), value);
            }
            Code::StorePtr => {
                let value = self.stack.pop()?;
                let pointer = self.stack.pop()?;

                if let Value::Pointer(pointer) = pointer {
                    match pointer {
                        PointerType::FieldPtr((object, field_idx)) => {
                            let (module, class) = {
                                let object = object.borrow();
                                (object.class.module.clone(), object.class.name.clone())
                            };
                            let desc = self.module_cache.lookup_class(&module, &class)?;
                            let (name, field_desc) = desc.fields.get_index(field_idx).unwrap();

                            if !field_desc.mutable {
                                return Err(RuntimeError::new(format!(
                                    "field '{}' is not mutable in class: {}.{}",
                                    name, module, class,
                                )));
                            }

                            let mut object = object.borrow_mut();

                            object.fields[field_idx] = value;
                        }
                        PointerType::ArrayItemPtr((array, field_idx)) => {
                            let mut array = array.borrow_mut();

                            array[field_idx] = value;
                        }
                        PointerType::MapItemPtr((map, key)) => {
                            let mut map = map.borrow_mut();

                            map.insert(*key, value);
                        }
                    };

                    return Ok(None);
                }

                return Err(RuntimeError::new(
                    "invalid type for StorePtr instruction".to_string(),
                ));
            }
            Code::StoreMut(id) => {
                let value = self.stack.pop()?;

                self.call_stack
                    .current_mut()?
                    .locals
                    .insert(id.clone(), value);
            }
            Code::LoadIndex | Code::LoadIndexPtr => {
                let index = self.stack.pop()?;
                let data = self.stack.pop()?;

                if let Value::Array(array) = data {
                    let index = to_int(index).map_err(|e| {
                        let message = format!("{} in index lookup", e);

                        e.with_message(message)
                    })?;
                    let items = array.borrow();

                    if let Some(item) = items.get(index as usize) {
                        self.stack.push(if ir.code == Code::LoadIndexPtr {
                            Value::Pointer(PointerType::ArrayItemPtr((
                                Rc::clone(&array),
                                index as usize,
                            )))
                        } else {
                            item.clone()
                        });

                        return Ok(None);
                    }

                    return Err(RuntimeError::new(
                        format!("index out of bounds: {}", index,),
                    ));
                }

                if let Value::Map(map) = data {
                    let items = map.borrow();

                    if let Some(item) = items.get(&index) {
                        self.stack.push(if ir.code == Code::LoadIndexPtr {
                            Value::Pointer(PointerType::MapItemPtr((Rc::clone(&map), index.into())))
                        } else {
                            item.clone()
                        });

                        return Ok(None);
                    }

                    return Err(RuntimeError::new(
                        format!("index out of bounds: {}", index,),
                    ));
                }

                return Err(RuntimeError::new(format!(
                    "unable to index type: {}",
                    data.get_type().name()
                )));
            }
            Code::LoadMember(member) | Code::TeeMember(member) | Code::LoadMemberPtr(member) => {
                let object = self.stack.pop().and_then(to_object)?;
                let (module, class) = {
                    let object = object.borrow();
                    (object.class.module.clone(), object.class.name.clone())
                };

                let desc = self.module_cache.lookup_class(&module, &class)?;

                if let Some(index) = desc.fields.get_index_of(member) {
                    let field = &desc.fields[member];

                    if !field.public && self.module_cache.current_name()? != module {
                        return Err(RuntimeError::new(format!(
                            "unable to access private field '{}' of class: {}.{}",
                            member, module, class
                        )));
                    }

                    self.stack.push(if let Code::LoadMember(_) = ir.code {
                        {
                            let object = object.borrow();
                            object.fields[index].clone()
                        }
                    } else {
                        Value::Pointer(PointerType::FieldPtr((Rc::clone(&object), index)))
                    });

                    return Ok(None);
                }

                if let Ok(_) = self.module_cache.lookup_method(&module, &class, member) {
                    let method = &desc.methods[member];

                    if !method.func.public && self.module_cache.current_name()? != module {
                        return Err(RuntimeError::new(format!(
                            "unable to access private method '{}(...)' of class: {}.{}",
                            member, module, class
                        )));
                    }

                    if let Code::TeeMember(_) = ir.code {
                        self.stack.push(Value::Object(Rc::clone(&object)));
                    }

                    self.stack.push(Value::Method(Method {
                        name: member.clone(),
                        object: Rc::clone(&object),
                    }));

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!(
                    "no such field or method '{}' for class: {}.{}",
                    member, module, class,
                )));
            }
            Code::Load(id) => {
                if !self.call_stack.is_empty() {
                    let context = self.call_stack.current_mut()?;

                    if let Some(value) = get_cloned(&context.locals, &id)
                        .or_else(|| get_cloned(&context.args, &id.name))
                    {
                        self.stack.push(value);

                        return Ok(None);
                    }
                }

                let current_module = self.module_cache.current()?;

                if let Some(value) = get_cloned(&current_module.globals, &id.name) {
                    self.stack.push(value);

                    return Ok(None);
                }

                if current_module.class_map.contains_key(&id.name) {
                    self.stack.push(
                        Value::Class(ClassId {
                            name: id.name.clone(),
                            module: self.module_cache.current_name()?.to_string(),
                        })
                        .into(),
                    );

                    return Ok(None);
                }

                if current_module.func_map.contains_key(&id.name) {
                    self.stack.push(
                        Value::Function(FuncId {
                            name: id.name.clone(),
                            module: current_module.name.clone(),
                        })
                        .into(),
                    );

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!("no such name: {}", id.name)));
            }
            Code::SetLabel(_) => {}
            Code::Branch((true_label, false_label)) => {
                let value = self.stack.pop().and_then(to_bool)?;

                return Ok(Some(if value {
                    true_label.clone()
                } else {
                    false_label.clone()
                }));
            }
            Code::Jump(label) => return Ok(Some(label.to_string())),
            Code::JumpIfTrue(label) => {
                let value = self.stack.pop().and_then(to_bool)?;

                if value {
                    self.stack.push(Value::Bool(value));

                    return Ok(Some(label.clone()));
                }
            }
        };

        Ok(None)
    }

    fn find_label(&self, instructions: Rc<Vec<IR>>, search: &str) -> Result<usize> {
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

    fn _eval(&mut self, module: &str, instructions: Rc<Vec<IR>>) -> Result<()> {
        let current_module = self
            .module_cache
            .current_name()
            .ok()
            .and_then(|name| Some(name.to_string()));

        self.module_cache.set_current(module);

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
                    i = self.find_label(Rc::clone(&instructions), &label)?;
                    continue;
                }

                // break on early return
                if let Some(context) = self.call_stack.last() {
                    if context.return_value.is_some() {
                        break;
                    }
                }

                i += 1;
            }

            Ok(())
        };

        let result = eval();

        if let Some(module) = current_module {
            self.module_cache.set_current(&module);
        }

        result
    }

    pub fn eval(&mut self, module: &str, instructions: Vec<IR>) -> Result<()> {
        self._eval(module, Rc::new(instructions)).map_err(|e| {
            let e = e.with_stack_trace(self.call_stack.rewind());

            if let Ok(module) = self.module_cache.current() {
                return e.with_module(module);
            }

            e
        })?;

        Ok(())
    }

    pub fn result(&mut self) -> Option<Value> {
        self.stack.pop().ok()
    }
}
