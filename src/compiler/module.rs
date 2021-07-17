use std::collections::HashMap;

use crate::compiler::IR;

#[derive(Debug)]
pub struct FuncArg {
    pub mutable: bool,
    pub name: String,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub args: Vec<FuncArg>,
    pub body: Vec<IR>,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub funcs: HashMap<String, Func>,
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            funcs: HashMap::new(),
        }
    }
}
