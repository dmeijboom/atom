use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use indexmap::map::IndexMap;

use atom_ir::IR;

use crate::ast::Pos;

#[derive(Debug, Clone)]
pub struct FuncArg {
    pub mutable: bool,
    pub name: String,
}

#[derive(Clone)]
pub struct Func {
    pub pos: Pos,
    pub public: bool,
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
    pub public: bool,
}

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub public: bool,
    pub funcs: HashMap<String, Func>,
    pub fields: IndexMap<String, Field>,
}

#[derive(Debug)]
pub struct Interface {
    pub name: String,
    pub public: bool,
    pub functions: Vec<String>,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub imports: Vec<String>,
    pub funcs: HashMap<String, Func>,
    pub classes: HashMap<String, Class>,
    pub interfaces: HashMap<String, Interface>,
}

impl Module {
    pub fn new(name: String) -> Self {
        Self {
            name,
            imports: vec![],
            funcs: HashMap::new(),
            classes: HashMap::new(),
            interfaces: HashMap::new(),
        }
    }
}
