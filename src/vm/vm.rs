use std::collections::HashMap;
use std::rc::Rc;

use crate::compiler::{Code, Func, IR, Module};
use crate::runtime::{FuncId, Value};

use super::result::{Result, RuntimeError};
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
    pub locals: HashMap<String, Value>,
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

    pub fn register_external_fn<F: Fn(Vec<Value>) -> Result<Option<Value>> + 'static>(&mut self, module_name: &str, name: &str, func: F) {
        self.func_map.insert(name.to_string(), FuncDesc {
            module_name: module_name.to_string(),
            run: Runnable::External(Box::new(func)),
        });
    }

    pub fn register(&mut self, module: Module) {
        for (name, func) in module.funcs {
            self.func_map.insert(name, FuncDesc {
                run: Runnable::Func(func),
                module_name: module.name.clone(),
            });
        }
    }

    fn call_context(&mut self) -> Result<&mut CallContext> {
        self.call_stack
            .last_mut()
            .ok_or_else(|| RuntimeError::new("expected call context".to_string()))
    }

    fn eval_call(&mut self, len: usize) -> Result<()> {
        let value = self.stack
            .pop()
            .map_err(|_| RuntimeError::new("expected function on stack".to_string()))?;

        if let Value::Function(id) = value {
            if let Some(desc) = self.func_map.get(&id.name) {
                if desc.module_name == id.module {
                    self.call_stack.push(CallContext {
                        locals: HashMap::new(),
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
                            let arg_names = func.args
                                .iter()
                                .map(|arg| arg.name.clone())
                                .collect::<Vec<_>>();
                            let mut values = self.stack.pop_many(len)?;
                            let call_context = self.call_context()?;

                            for name in arg_names {
                                call_context.locals.insert(
                                    name,
                                    values.remove(0),
                                );
                            }

                            self.eval(body)?;
                        }
                        Runnable::External(func) => {
                            let values = self.stack.pop_many(len)?;

                            if let Some(return_value) = func(values)? {
                                self.stack.push(return_value);
                            }
                        }
                    };

                    self.call_stack.pop();

                    return Ok(());
                }
            }

            return Err(RuntimeError::new(format!("no such function: {}.{}(...)", id.module, id.name)));
        }

        Err(RuntimeError::new(format!("expected function on stack, not: {}", value.get_type().name())))
    }

    fn eval_single(&mut self, ir: IR) -> Result<()> {
        Ok(match &ir.code {
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
            Code::LogicalOr => unreachable!(),
            Code::LogicalAnd => unreachable!(),
            Code::ArithmeticBitOr => unreachable!(),
            Code::ArithmeticBitAnd => unreachable!(),
            Code::ArithmeticAdd => unreachable!(),
            Code::ArithmeticSub => unreachable!(),
            Code::ArithmeticMul => unreachable!(),
            Code::ArithmeticDiv => unreachable!(),
            Code::ComparisonEq => unreachable!(),
            Code::ComparisonNeq => unreachable!(),
            Code::ComparisonGt => unreachable!(),
            Code::ComparisonGte => unreachable!(),
            Code::ComparisonLt => unreachable!(),
            Code::ComparisonLte => unreachable!(),
            Code::Not => unreachable!(),
            Code::Call(len) => return self.eval_call(*len),
            Code::Store(name) => {
                let value = self.stack.pop()?;

                self.call_context()?.locals.insert(
                    name.to_string(),
                    value,
                );
            }
            Code::StoreMut(name) => {
                let value = self.stack.pop()?;

                self.call_context()?.locals.insert(
                    name.to_string(),
                    value,
                );
            }
            Code::Load(name) => {
                if self.call_stack.len() > 0 {
                    let context = self.call_context()?;

                    if let Some(value) = context.locals
                        .get(name)
                        .and_then(|value| Some(value.clone())) {
                        self.stack.push(value);

                        return Ok(());
                    }
                }

                if let Some(func) = self.func_map.get(name) {
                    self.stack.push(Value::Function(FuncId {
                        name: name.clone(),
                        module: func.module_name.clone(),
                    }).into());

                    return Ok(());
                }

                return Err(RuntimeError::new(format!("no such name: {}", name)));
            }
        })
    }

    pub fn eval(&mut self, ir: Vec<IR>) -> Result<()> {
        for ir in ir {
            let pos = ir.pos.clone();

            self.eval_single(ir)
                .map_err(|e| e.with_pos(pos))?;
        }

        Ok(())
    }
}
