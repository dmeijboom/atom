use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use crate::compiler::{Code, LocalId, IR};
use crate::runtime::convert::{to_bool, to_float, to_int, to_object};
use crate::runtime::{ClassId, FuncId, Method, Object, PointerType, Result, RuntimeError, Value};
use crate::vm::call_stack::CallContext;
use crate::vm::module_cache::{FuncSource, Module, ModuleCache};

use super::call_stack::CallStack;
use super::stack::Stack;

pub struct VM {
    stack: Stack,
    module_cache: ModuleCache,
    call_stack: CallStack,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            call_stack: CallStack::new(),
            module_cache: ModuleCache::new(),
        }
    }

    pub fn register_module(&mut self, module: Module) {
        self.module_cache.add(module);
    }

    fn eval_call(&mut self, keywords: &[String], arg_count: usize) -> Result<()> {
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

        if arg_count != desc.func.args.len() {
            return Err(RuntimeError::new(format!(
                "invalid argument count for Method: {}.{}.{}(...) (expected {}, not {})",
                module,
                class,
                method.name,
                desc.func.args.len(),
                arg_count,
            )));
        }

        let arg_names = desc.func.args.keys().cloned().collect();
        let values = self.stack.pop_many(arg_count)?;
        let ordered_values = self
            .prepare_args(values, keywords, &arg_names)
            .map_err(|e| {
                let message = e.message.clone();
                e.with_message(format!(
                    "{} in Method {}.{}.{}",
                    message, module, class, method.name
                ))
            })?;
        let call_context = self.call_stack.current_mut()?;

        for (i, value) in ordered_values {
            call_context.args.insert(arg_names[i].to_string(), value);
        }

        let source = Rc::clone(&desc.func.source.native().unwrap());

        self._eval(&module, source)?;

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

    fn eval_function_call(
        &mut self,
        id: &FuncId,
        keywords: &[String],
        arg_count: usize,
    ) -> Result<()> {
        let func = self.module_cache.lookup_function(&id.module, &id.name)?;

        self.call_stack
            .push(CallContext::new(func.pos.clone(), id.clone()));

        match &func.source {
            FuncSource::Native(source) => {
                if arg_count != func.args.len() {
                    return Err(RuntimeError::new(format!(
                        "invalid argument count for Fn: {}.{}(...) (expected {}, not {})",
                        id.module,
                        id.name,
                        func.args.len(),
                        arg_count,
                    )));
                }

                let arg_names = func.args.keys().cloned().collect();
                let values = self.stack.pop_many(arg_count)?;
                let ordered_values =
                    self.prepare_args(values, keywords, &arg_names)
                        .map_err(|e| {
                            let message = e.message.clone();
                            e.with_message(format!("{} in Fn {}.{}", message, id.module, id.name,))
                        })?;
                let call_context = self.call_stack.current_mut()?;

                for (i, value) in ordered_values {
                    call_context.args.insert(arg_names[i].to_string(), value);
                }

                let source = Rc::clone(source);

                self._eval(&id.module, source)?;
            }
            FuncSource::External(closure) => {
                if !keywords.is_empty() {
                    return Err(RuntimeError::new(format!(
                        "unable to use keyword arguments in external Fn: {}.{}",
                        id.module, id.name
                    )));
                }

                let values = self.stack.pop_many(arg_count)?;

                if let Some(return_value) = closure(values)? {
                    self.call_stack.current_mut()?.return_value = Some(return_value);
                }
            }
        }

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

                self.stack.push(Value::Range(from..=to));
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
            Code::Call((keywords, arg_count)) => self.eval_call(keywords, *arg_count)?.into(),
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
            Code::LoadIndex => {
                let index = self.stack.pop().and_then(to_int)?;
                let value = self.stack.pop()?;

                if let Value::Array(array) = value {
                    let items = array.borrow();

                    if let Some(item) = items.get(index as usize) {
                        self.stack.push(item.clone());

                        return Ok(None);
                    }

                    return Err(RuntimeError::new(
                        format!("index out of bounds: {}", index,),
                    ));
                }

                return Err(RuntimeError::new(format!(
                    "unable to index type: {}",
                    value.get_type().name()
                )));
            }
            Code::LoadMember(member) | Code::LoadMemberPtr(member) => {
                let object = self.stack.pop().and_then(to_object)?;
                let (module, class) = {
                    let object = object.borrow();
                    (object.class.module.clone(), object.class.name.clone())
                };

                let desc = self.module_cache.lookup_class(&module, &class)?;

                if let Some(index) = desc.fields.get_index_of(member) {
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

                if self
                    .module_cache
                    .current()?
                    .class_map
                    .contains_key(&id.name)
                {
                    self.stack.push(
                        Value::Class(ClassId {
                            name: id.name.clone(),
                            module: self.module_cache.current_name()?.to_string(),
                        })
                        .into(),
                    );

                    return Ok(None);
                }

                if self.module_cache.current()?.func_map.contains_key(&id.name) {
                    self.stack.push(
                        Value::Function(FuncId {
                            name: id.name.clone(),
                            module: self.module_cache.current_name()?.to_string(),
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
}
