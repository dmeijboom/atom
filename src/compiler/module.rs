use std::collections::HashMap;

use crate::ast::Pos;
use crate::compiler::IR;
use crate::runtime::IndexedBTreeMap;

#[derive(Debug, Clone)]
pub struct FuncArg {
    pub mutable: bool,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub pos: Pos,
    pub name: String,
    pub body: Vec<IR>,
    pub is_void: bool,
    pub args: Vec<FuncArg>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub mutable: bool,
}

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub funcs: HashMap<String, Func>,
    pub fields: IndexedBTreeMap<String, Field>,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub funcs: HashMap<String, Func>,
    pub classes: HashMap<String, Class>,
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            funcs: HashMap::new(),
            classes: HashMap::new(),
        }
    }
}
