use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::ast::Pos;
use crate::compiler::{Code, LocalId, IR};
use crate::runtime::{
    convert, ClassId, FuncId, Object, PointerType, Result, RuntimeError, Trace, Value,
};
use crate::vm::module::{ClassDesc, FuncDesc};
use crate::vm::{module::Runnable, Module};

use super::stack::Stack;

fn find_index(input: &[String], search: &String) -> Option<usize> {
    for (index, item) in input.iter().enumerate() {
        if item == search {
            return Some(index);
        }
    }

    None
}

struct CallContext {
    pub pos: Pos,
    pub id: FuncId,
    pub return_value: Option<Value>,
    pub args: HashMap<String, Value>,
    pub locals: HashMap<LocalId, Value>,
}

struct ModuleCache {
    modules: HashMap<String, Module>,
    current_module: Option<String>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            current_module: None,
        }
    }

    pub fn add(&mut self, module: Module) {
        self.modules.insert(module.name.clone(), module);
    }

    fn set_current(&mut self, name: &str) {
        if !self.modules.contains_key(name) {
            panic!("unable to set unknown current module: {}", name);
        }

        self.current_module = Some(name.to_string());
    }

    fn current(&mut self) -> Result<&mut Module> {
        self.current_module
            .clone()
            .as_ref()
            .and_then(move |name| self.modules.get_mut(name))
            .ok_or_else(|| RuntimeError::new("no active module found".to_string()))
    }

    fn lookup_class(&self, module_name: &str, class_name: &str) -> Result<&ClassDesc> {
        if let Some(module) = self.modules.get(module_name) {
            if let Some(class_desc) = module.class_map.get(class_name) {
                return Ok(class_desc);
            }

            return Err(RuntimeError::new(format!(
                "no such class: {}.{}",
                module_name, class_name
            )));
        }

        Err(RuntimeError::new(format!(
            "no such module: {}",
            module_name
        )))
    }

    fn lookup_function(&self, module_name: &str, function_name: &str) -> Result<&FuncDesc> {
        if let Some(module) = self.modules.get(module_name) {
            if let Some(function_desc) = module.func_map.get(function_name) {
                return Ok(function_desc);
            }

            return Err(RuntimeError::new(format!(
                "no such function: {}.{}(..)",
                module_name, function_name
            )));
        }

        Err(RuntimeError::new(format!(
            "no such module: {}",
            module_name
        )))
    }
}

pub struct VM {
    stack: Stack,
    module_cache: ModuleCache,
    call_stack: Vec<CallContext>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            call_stack: vec![],
            module_cache: ModuleCache::new(),
        }
    }

    pub fn register_module(&mut self, module: Module) {
        self.module_cache.add(module);
    }

    fn call_context(&mut self) -> Result<&mut CallContext> {
        self.call_stack
            .last_mut()
            .ok_or_else(|| RuntimeError::new("expected call context".to_string()))
    }

    fn rewind_stack(&mut self) -> Vec<Trace> {
        let mut stack_trace = vec![];

        while !self.call_stack.is_empty() {
            let call_context = self.call_stack.remove(0);

            stack_trace.push(Trace {
                pos: call_context.pos.clone(),
                func: call_context.id,
            });
        }

        stack_trace
    }

    fn eval_call(&mut self, pos: &Pos, keywords: &[String], arg_count: usize) -> Result<()> {
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

        let value = self
            .stack
            .pop()
            .map_err(|_| RuntimeError::new("expected function on stack".to_string()))?;

        if let Value::Function(id) = value {
            return self.eval_function_call(pos, &id, keywords, arg_count);
        }

        if let Value::Class(id) = value {
            return self.eval_class_init(&id, keywords, arg_count);
        }

        Err(RuntimeError::new(format!(
            "expected callable on stack, not: {}",
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
                let arg_idx = desc.class.fields.get_index_by_key(name);

                (arg_idx, name.as_str(), value)
            })
            .collect::<Vec<_>>();

        if values.len() != desc.class.fields.len() {
            let mut field_names = desc
                .class
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

    fn eval_function_call(
        &mut self,
        pos: &Pos,
        id: &FuncId,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let desc = self.module_cache.lookup_function(&id.module, &id.name)?;
        if desc.module_name == id.module {
            self.call_stack.push(CallContext {
                id: id.clone(),
                pos: pos.clone(),
                args: HashMap::new(),
                locals: HashMap::new(),
                return_value: None,
            });

            match &desc.run {
                Runnable::Func(func) => {
                    if arg_count != func.args.len() {
                        return Err(RuntimeError::new(format!(
                            "invalid argument count for function: {}.{}(...) (expected {}, not {})",
                            id.module,
                            id.name,
                            func.args.len(),
                            arg_count,
                        )));
                    }

                    let body = func.body.clone();
                    let arg_names = func
                        .args
                        .iter()
                        .map(|arg| arg.name.clone())
                        .collect::<Vec<_>>();
                    let mut values = self.stack.pop_many(arg_count)?;
                    let mut ordered_values = BTreeMap::new();
                    let call_context = self.call_context()?;

                    for name in keywords.iter() {
                        if let Some(arg_idx) = find_index(&arg_names, name) {
                            ordered_values.insert(arg_idx, values.remove(0));
                            continue;
                        }

                        return Err(RuntimeError::new(format!(
                            "no such field '{}' for Fn: {}.{}",
                            name, id.module, id.name,
                        )));
                    }

                    for i in 0..arg_names.len() {
                        if ordered_values.contains_key(&i) {
                            continue;
                        }

                        ordered_values.insert(i, values.remove(0));
                    }

                    for (key, value) in ordered_values {
                        let name = &arg_names[key];

                        call_context.args.insert(name.to_string(), value);
                    }

                    self.eval_internal(body)?;
                }
                Runnable::External(func) => {
                    if !keywords.is_empty() {
                        return Err(RuntimeError::new(format!(
                            "unable to use keyword arguments in external Fn: {}.{}",
                            id.module, id.name
                        )));
                    }

                    let values = self.stack.pop_many(arg_count)?;

                    if let Some(return_value) = func(values)? {
                        self.call_context()?.return_value = Some(return_value);
                    }
                }
            };

            let context = self.call_stack.pop().unwrap();

            if let Some(value) = context.return_value {
                self.stack.push(value);
            } else {
                self.stack.push(Value::Invalid);
            }

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "no such function: {}.{}(...)",
            id.module, id.name
        )))
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
            Value::Int(val) => Ok(Value::Bool(op(val.cmp(&convert::to_int(&right)?)))),
            Value::Float(val) => {
                if let Some(ord) = val.partial_cmp(&convert::to_float(&right)?) {
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

    fn eval_single(&mut self, ir: IR) -> Result<Option<String>> {
        match &ir.code {
            Code::ConstInt(val) => self.stack.push(Value::Int(*val)),
            Code::ConstBool(val) => self.stack.push(Value::Bool(*val)),
            Code::ConstFloat(val) => self.stack.push(Value::Float(*val)),
            Code::ConstChar(val) => self.stack.push(Value::Char(*val)),
            Code::ConstString(val) => self.stack.push(Value::String(val.clone())),
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
            Code::Discard => self.stack.delete()?,
            Code::Return => {
                let return_value = self.stack.pop()?;

                self.call_context()?.return_value = Some(return_value);
            }
            Code::LogicalAnd => self.eval_op("logical and", |left, right| match &left {
                Value::Bool(val) => Ok(Value::Bool(*val && convert::to_bool(&right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticBitOr => self.eval_op("bitwise or", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val | convert::to_int(&right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticBitAnd => self.eval_op("bitwise and", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val & convert::to_int(&right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticAdd => self.eval_op("addition", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val + convert::to_int(&right)?)),
                Value::Float(val) => Ok(Value::Float(*val + convert::to_float(&right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticSub => self.eval_op("subtraction", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val - convert::to_int(&right)?)),
                Value::Float(val) => Ok(Value::Float(*val - convert::to_float(&right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticMul => self.eval_op("multiplication", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val * convert::to_int(&right)?)),
                Value::Float(val) => Ok(Value::Float(*val * convert::to_float(&right)?)),
                _ => Err(RuntimeError::new(format!(
                    "invalid types: {} and {}",
                    left.get_type().name(),
                    right.get_type().name()
                ))),
            })?,
            Code::ArithmeticDiv => self.eval_op("division", |left, right| match &left {
                Value::Int(val) => Ok(Value::Int(*val / convert::to_int(&right)?)),
                Value::Float(val) => Ok(Value::Float(*val / convert::to_float(&right)?)),
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
            Code::ComparisonLt => self.eval_comparison_op(|ord| ord == Ordering::Less)?,
            Code::ComparisonLte => {
                self.eval_comparison_op(|ord| ord == Ordering::Less || ord == Ordering::Equal)?
            }
            Code::Not => {
                let value = self.stack.pop()?;

                self.stack.push(Value::Bool(!convert::to_bool(&value)?));
            }
            Code::Call((keywords, arg_count)) => self.eval_call(&ir.pos, keywords, *arg_count)?,
            Code::Store(id) => {
                let value = self.stack.pop()?;

                self.call_context()?.locals.insert(id.clone(), value);
            }
            Code::StorePtr => {
                let value = self.stack.pop()?;
                let pointer = self.stack.pop()?;

                if let Value::Pointer(pointer) = pointer {
                    match pointer {
                        PointerType::FieldPtr((object, field_idx)) => {
                            let mut object = object.borrow_mut();
                            let desc = self
                                .module_cache
                                .lookup_class(&object.class.module, &object.class.name)?;
                            let (name, field_desc) =
                                desc.class.fields.get_key_value_by_index(field_idx).unwrap();

                            if !field_desc.mutable {
                                return Err(RuntimeError::new(format!(
                                    "field '{}' is not mutable in class: {}.{}",
                                    name, object.class.module, object.class.name
                                )));
                            }

                            object.fields[field_idx] = value;
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

                self.call_context()?.locals.insert(id.clone(), value);
            }
            Code::LoadMember(member) | Code::LoadMemberPtr(member) => {
                let value = self.stack.pop()?;

                if let Value::Object(object) = value {
                    let obj = object.borrow();
                    let desc = self
                        .module_cache
                        .lookup_class(&obj.class.module, &obj.class.name)?;

                    if let Some(index) = desc.class.fields.get_index_by_key(member) {
                        if let Code::LoadMember(_) = ir.code {
                            self.stack.push(obj.fields[index].clone());
                        } else {
                            self.stack.push(Value::Pointer(PointerType::FieldPtr((
                                Rc::clone(&object),
                                index,
                            ))))
                        }

                        return Ok(None);
                    }

                    return Err(RuntimeError::new(format!(
                        "no such field '{}' for class: {}.{}",
                        member, obj.class.module, obj.class.name
                    )));
                }
            }
            Code::Load(id) => {
                if self.call_stack.len() > 0 {
                    let context = self.call_context()?;

                    if let Some(value) =
                        context.locals.get(id).and_then(|value| Some(value.clone()))
                    {
                        self.stack.push(value);

                        return Ok(None);
                    }

                    if let Some(value) = context
                        .args
                        .get(&id.name)
                        .and_then(|value| Some(value.clone()))
                    {
                        self.stack.push(value);

                        return Ok(None);
                    }
                }

                if let Some(desc) = self.module_cache.current()?.class_map.get(&id.name) {
                    self.stack.push(
                        Value::Class(ClassId {
                            name: id.name.clone(),
                            module: desc.module_name.clone(),
                        })
                        .into(),
                    );

                    return Ok(None);
                }

                if let Some(desc) = self.module_cache.current()?.func_map.get(&id.name) {
                    self.stack.push(
                        Value::Function(FuncId {
                            name: id.name.clone(),
                            module: desc.module_name.clone(),
                        })
                        .into(),
                    );

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!("no such name: {}", id.name)));
            }
            Code::SetLabel(_) => {}
            Code::Branch((true_label, false_label)) => {
                let value = self.stack.pop()?;

                return if convert::to_bool(&value)? {
                    Ok(Some(true_label.clone()))
                } else {
                    Ok(Some(false_label.clone()))
                };
            }
            Code::Jump(label) => return Ok(Some(label.to_string())),
            Code::JumpIfTrue(label) => {
                let value = self.stack.pop()?;

                if convert::to_bool(&value)? {
                    return Ok(Some(label.clone()));
                }
            }
        };

        Ok(None)
    }

    fn eval_internal(&mut self, ir_list: Vec<IR>) -> Result<()> {
        let mut active_label: Option<String> = None;

        for ir in ir_list {
            // skip instructions if we're jumping to a label
            if active_label.is_some() {
                if let Code::SetLabel(label) = &ir.code {
                    if Some(label) == active_label.as_ref() {
                        active_label = None;
                    }
                }

                continue;
            }

            let pos = ir.pos.clone();

            active_label = self.eval_single(ir).map_err(|e| e.with_pos(pos))?;

            // break on early return
            if let Some(context) = self.call_stack.last() {
                if context.return_value.is_some() {
                    break;
                }
            }
        }

        Ok(())
    }

    pub fn eval(&mut self, module: &str, ir_list: Vec<IR>) -> Result<()> {
        self.module_cache.set_current(module);

        self.eval_internal(ir_list).map_err(|e| {
            let e = e.with_stack_trace(self.rewind_stack());

            if let Ok(module) = self.module_cache.current() {
                return e.with_module(module);
            }

            e
        })?;

        Ok(())
    }
}
