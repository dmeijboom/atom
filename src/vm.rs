use std::collections::HashMap;

use crate::ast::Pos;
use crate::compiler::{Func, IR, Module};
use crate::runtime::Value;
use std::error::Error;
use std::fmt;

struct FuncDesc {
    pub func: Func,
    pub module_name: String,
}

pub struct VM {
    func_map: HashMap<String, FuncDesc>,
    stack: Vec<Value>,
}

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

impl VM {
    pub fn new() -> Self {
        Self {
            func_map: HashMap::new(),
            stack: vec![],
        }
    }

    pub fn register(&mut self, module: Module) {
        for (name, func) in module.funcs {
            self.func_map.insert(name, FuncDesc {
                func,
                module_name: module.name.clone(),
            });
        }
    }

    fn eval_single(&self, ir: &IR) -> Result<()> {
        unreachable!()
    }

    pub fn eval(&self, ir: Vec<IR>) -> Result<()> {
        for ir in ir.iter() {
            self.eval_single(ir)?;
        }

        Ok(())
    }
}
