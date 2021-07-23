use std::cmp::Ordering;
use std::collections::HashMap;
use std::rc::Rc;

use crate::compiler::{Code, Func, LocalId, Module, IR};
use crate::runtime::{convert, FuncId, Result, RuntimeError, Value};

use super::stack::Stack;

enum Runnable {
    Func(Func),
    External(Box<dyn Fn(Vec<Value>) -> Result<Option<Value>>>),
}

struct FuncDesc {
    pub run: Runnable,
    pub module_name: String,
}

struct CallContext {
    pub return_value: Option<Value>,
    pub args: HashMap<String, Value>,
    pub locals: HashMap<LocalId, Value>,
}

pub struct VM {
    stack: Stack,
    call_stack: Vec<CallContext>,
    func_map: HashMap<String, FuncDesc>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            call_stack: vec![],
            func_map: HashMap::new(),
        }
    }

    pub fn register_external_fn<F: Fn(Vec<Value>) -> Result<Option<Value>> + 'static>(
        &mut self,
        module_name: &str,
        name: &str,
        func: F,
    ) {
        self.func_map.insert(
            name.to_string(),
            FuncDesc {
                module_name: module_name.to_string(),
                run: Runnable::External(Box::new(func)),
            },
        );
    }

    pub fn register(&mut self, module: Module) {
        for (name, func) in module.funcs {
            self.func_map.insert(
                name,
                FuncDesc {
                    run: Runnable::Func(func),
                    module_name: module.name.clone(),
                },
            );
        }
    }

    fn call_context(&mut self) -> Result<&mut CallContext> {
        self.call_stack
            .last_mut()
            .ok_or_else(|| RuntimeError::new("expected call context".to_string()))
    }

    fn eval_call(&mut self, len: usize) -> Result<()> {
        let value = self
            .stack
            .pop()
            .map_err(|_| RuntimeError::new("expected function on stack".to_string()))?;

        if let Value::Function(id) = value {
            if let Some(desc) = self.func_map.get(&id.name) {
                if desc.module_name == id.module {
                    self.call_stack.push(CallContext {
                        args: HashMap::new(),
                        locals: HashMap::new(),
                        return_value: None,
                    });

                    match &desc.run {
                        Runnable::Func(func) => {
                            if len != func.args.len() {
                                return Err(
                                    RuntimeError::new(
                                        format!(
                                            "invalid argument count for function: {}.{}(...) (expected {}, not {})",
                                            id.module,
                                            id.name,
                                            func.args.len(),
                                            len,
                                        ),
                                    ),
                                );
                            }

                            let body = func.body.clone();
                            let arg_names = func
                                .args
                                .iter()
                                .map(|arg| arg.name.clone())
                                .collect::<Vec<_>>();
                            let mut values = self.stack.pop_many(len)?;
                            let call_context = self.call_context()?;

                            for name in arg_names {
                                call_context.args.insert(name, values.remove(0));
                            }

                            self.eval(body)?;
                        }
                        Runnable::External(func) => {
                            let values = self.stack.pop_many(len)?;

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
            }

            return Err(RuntimeError::new(format!(
                "no such function: {}.{}(...)",
                id.module, id.name
            )));
        }

        Err(RuntimeError::new(format!(
            "expected function on stack, not: {}",
            value.get_type().name()
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

                self.stack.push(Value::Array(Rc::new(values)));
            }
            Code::MakeMap(len) => {
                let mut map = HashMap::new();
                let mut key_values = self.stack.pop_many(len * 2)?;

                for _ in 0..*len {
                    let key = key_values.remove(0);
                    let value = key_values.remove(0);

                    map.insert(key, value);
                }

                self.stack.push(Value::Map(Rc::new(map)));
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
            Code::Call(len) => self.eval_call(*len)?,
            Code::Store(id) => {
                let value = self.stack.pop()?;

                self.call_context()?.locals.insert(id.clone(), value);
            }
            Code::StoreMut(id) => {
                let value = self.stack.pop()?;

                self.call_context()?.locals.insert(id.clone(), value);
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

                if let Some(func) = self.func_map.get(&id.name) {
                    self.stack.push(
                        Value::Function(FuncId {
                            name: id.name.clone(),
                            module: func.module_name.clone(),
                        })
                        .into(),
                    );

                    return Ok(None);
                }

                return Err(RuntimeError::new(format!("no such name: {}", id.name)));
            }
            Code::SetLabel(_) => {}
            Code::JumpIfTrue(label) | Code::JumpIfFalse(label) => {
                let value = self.stack.pop()?;
                let bool_val = convert::to_bool(&value)?;
                let expected_val = if let Code::JumpIfTrue(_) = ir.code {
                    true
                } else {
                    false
                };

                self.stack.push(value);

                if bool_val == expected_val {
                    return Ok(Some(label.clone()));
                }
            }
        };

        Ok(None)
    }

    pub fn eval(&mut self, ir_list: Vec<IR>) -> Result<()> {
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
}
