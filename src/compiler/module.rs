use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use indexmap::map::IndexMap;

use crate::ast::Pos;
use crate::compiler::IR;

#[derive(Debug, Clone)]
pub struct FuncArg {
    pub mutable: bool,
    pub name: String,
}

#[derive(Clone)]
pub struct Func {
    pub pos: Pos,
    pub name: String,
    pub body: Vec<IR>,
    pub is_void: bool,
    pub args: Vec<FuncArg>,
}

impl Debug for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Fn {}() -> {} {{\n{}\n}}",
            self.name,
            if self.is_void { "Void" } else { "Any" },
            self.body
                .iter()
                .map(|ir| format!("  {:?}", ir))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub mutable: bool,
}

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub funcs: HashMap<String, Func>,
    pub fields: IndexMap<String, Field>,
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
