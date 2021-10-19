use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use indexmap::map::IndexMap;

use atom_ir::{Location, IR};

use crate::ast::MixinDeclStmt;

use super::types::Type;

#[derive(Debug, Clone)]
pub struct FuncArg {
    pub mutable: bool,
    pub name: String,
}

#[derive(Clone)]
pub struct Func {
    pub name: String,
    pub body: IR,
    pub is_void: bool,
    pub is_extern: bool,
    pub is_closure: bool,
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
                .map(|code| format!("  {:?}", code))
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
    pub methods: HashMap<String, Func>,
    pub fields: IndexMap<String, Field>,
}

#[derive(Debug)]
pub struct Interface {
    pub name: String,
    pub functions: Vec<String>,
}

#[derive(Debug)]
pub struct Import {
    pub known_type: Type,
    pub origin: String,
}

impl Import {
    pub fn new(known_type: Type, origin: String) -> Self {
        Self { known_type, origin }
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub filename: Option<String>,
    pub funcs: Vec<Func>,
    pub mixins: HashMap<String, MixinDeclStmt>,
    pub classes: IndexMap<String, Class>,
    pub interfaces: IndexMap<String, Interface>,
    pub imports: IndexMap<String, Import>,
    pub exports: HashMap<String, Type>,
}

impl Module {
    pub fn new() -> Self {
        Self::with_name("main".to_string())
    }

    pub fn with_name(name: String) -> Self {
        Self {
            name,
            filename: None,
            funcs: vec![],
            mixins: HashMap::new(),
            classes: IndexMap::new(),
            interfaces: IndexMap::new(),
            imports: IndexMap::new(),
            exports: HashMap::new(),
        }
    }

    pub fn get_fn_by_name(&self, name: &str) -> Option<&Func> {
        self.funcs.iter().find(|func| func.name == name)
    }
}
