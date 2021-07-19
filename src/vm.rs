use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

use crate::ast::Pos;
use crate::compiler::{Code, Func, IR, Module};
use crate::runtime::{FuncId, Value};

#[derive(Debug)]
pub struct RuntimeError {
    pub pos: Pos,
    pub message: String,
    pub module_name: Option<String>,
}

impl RuntimeError {
    pub fn new(message: String, pos: Pos, module_name: Option<String>) -> Self {
        Self {
            message,
            pos,
            module_name,
        }
    }
}

impl Error for RuntimeError {}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(module_name) = &self.module_name {
            return write!(f, "{} at {}..{} in {}", self.message, self.pos.start, self.pos.end, module_name);
        }

        write!(f, "{} at {}..{}", self.message, self.pos.start, self.pos.end)
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;

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
    stack: Vec<Value>,
    call_stack: Vec<CallContext>,
    func_map: HashMap<String, FuncDesc>,
}


impl VM {
    pub fn new() -> Self {
        Self {
            stack: vec![],
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

    fn call_context(&mut self, pos: Pos) -> Result<&mut CallContext> {
        self.call_stack
            .last_mut()
            .ok_or_else(|| RuntimeError::new(
                "expected call context".to_string(),
                pos,
                None,
            ))
    }

    fn pop_value_from_stack(&mut self, pos: Pos) -> Result<Value> {
        self.stack.pop().ok_or_else(||
            RuntimeError::new("no item on stack".to_string(), pos, None))
    }

    fn pop_values_from_stack(&mut self, pos: Pos, mut len: usize) -> Result<Vec<Value>> {
        let mut values = vec![];

        while len > 0 {
            if let Some(value) = self.stack.pop() {
                values.insert(0, value);
                len -= 1;

                continue;
            }

            return Err(RuntimeError::new("no item on stack".to_string(), pos, None));
        }

        Ok(values)
    }

    fn eval_call(&mut self, pos: Pos, len: usize) -> Result<()> {
        let values = self.pop_values_from_stack(pos.clone(), len)?;
        let value = self.stack
            .pop()
            .ok_or_else(|| RuntimeError::new(
                "expected function on stack".to_string(),
                pos.clone(),
                None,
            ))?;

        if let Value::Function(id) = value {
            if let Some(desc) = self.func_map.get(&id.name) {
                if desc.module_name == id.module {
                    self.call_stack.push(CallContext {
                        locals: HashMap::new(),
                    });

                    match &desc.run {
                        Runnable::Func(func) => {
                            if len != func.args.len() {
                                return Err(RuntimeError::new(
                                    format!(
                                        "invalid argument count for function: {}.{}(...) (expected {}, not {})",
                                        id.module,
                                        id.name,
                                        func.args.len(),
                                        len,
                                    ),
                                    pos,
                                    None,
                                ));
                            }

                            let body = func.body.clone();

                            self.eval(body)?;
                        }
                        Runnable::External(func) => {
                            if let Some(return_value) = func(values)? {
                                self.stack.push(return_value);
                            }
                        }
                    };

                    self.call_stack.pop();

                    return Ok(());
                }
            }

            return Err(RuntimeError::new(
                format!("no such function: {}.{}(...)", id.module, id.name),
                pos,
                None,
            ));
        }

        Err(RuntimeError::new(
            format!("expected function on stack, not: {}", value.get_type().name()),
            pos,
            None,
        ))
    }

    fn eval_single(&mut self, ir: IR) -> Result<()> {
        Ok(match &ir.code {
            Code::ConstInt(val) => self.stack.push(Value::Int(*val)),
            Code::ConstBool(val) => self.stack.push(Value::Bool(*val)),
            Code::ConstFloat(val) => self.stack.push(Value::Float(*val)),
            Code::ConstChar(val) => self.stack.push(Value::Char(*val)),
            Code::ConstString(val) => self.stack.push(Value::String(val.clone())),
            Code::MakeArray(len) => {
                let values = self.pop_values_from_stack(ir.pos.clone(), *len)?;

                self.stack.push(Value::Array(Rc::new(values)));
            }
            Code::MakeMap(len) => {
                let mut map = HashMap::new();
                let mut key_values = self.pop_values_from_stack(ir.pos.clone(), len * 2)?;

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
            Code::Call(len) => return self.eval_call(ir.pos.clone(), *len),
            Code::Store(name) => {
                let value = self.pop_value_from_stack(ir.pos.clone())?;

                self.call_context(ir.pos.clone())?.locals.insert(
                    name.to_string(),
                    value,
                );
            }
            Code::StoreMut(name) => {
                let value = self.pop_value_from_stack(ir.pos.clone())?;

                self.call_context(ir.pos.clone())?.locals.insert(
                    name.to_string(),
                    value,
                );
            }
            Code::Load(name) => {
                if self.call_stack.len() > 0 {
                    let context = self.call_context(ir.pos.clone())?;

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

                return Err(RuntimeError::new(
                    format!("no such name: {}", name),
                    ir.pos.clone(),
                    None,
                ));
            }
        })
    }

    pub fn eval(&mut self, ir: Vec<IR>) -> Result<()> {
        for ir in ir {
            self.eval_single(ir)?;
        }

        Ok(())
    }
}
