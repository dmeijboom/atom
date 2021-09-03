use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use indexmap::map::IndexMap;

use atom_ir::{Location, IR};

#[derive(Debug, Clone)]
pub struct FuncArg {
    pub mutable: bool,
    pub name: String,
}

#[derive(Clone)]
pub struct Func {
    pub public: bool,
    pub name: String,
    pub body: Vec<IR>,
    pub is_void: bool,
    pub is_extern: bool,
    pub args: Vec<FuncArg>,
    pub location: Location,
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

#[derive(Clone)]
pub struct Closure {
    pub body: Vec<IR>,
    pub is_void: bool,
    pub args: Vec<FuncArg>,
    pub location: Location,
}

impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "|| -> {} {{\n{}\n}}",
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
pub enum TypeKind {
    Fn,
    Class,
    Interface,
}

#[derive(Debug)]
pub struct Type {
    pub name: String,
    pub kind: TypeKind,
    pub module_name: String,
}

impl Type {
    pub fn new(kind: TypeKind, module_name: String, name: String) -> Self {
        Self {
            kind,
            name,
            module_name,
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub filename: Option<String>,
    pub modules: HashMap<String, Module>,
    pub closures: Vec<Closure>,
    pub funcs: IndexMap<String, Func>,
    pub classes: IndexMap<String, Class>,
    pub interfaces: IndexMap<String, Interface>,
    pub globals: IndexMap<String, Type>,
}

impl Module {
    pub fn new() -> Self {
        Self::with_name("main".to_string())
    }

    pub fn with_name(name: String) -> Self {
        Self {
            name,
            filename: None,
            closures: vec![],
            modules: HashMap::new(),
            funcs: IndexMap::new(),
            classes: IndexMap::new(),
            interfaces: IndexMap::new(),
            globals: IndexMap::new(),
        }
    }
}
