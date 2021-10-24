use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use indexmap::map::IndexMap;

use atom_ir::{Location, IR};

use crate::ast::MixinDeclStmt;

use super::types::Type;

#[derive(Debug, Clone, Default)]
pub struct FuncArg {
    pub mutable: bool,
    pub name: String,
}

impl FuncArg {
    pub fn new(name: String, mutable: bool) -> Self {
        Self { name, mutable }
    }
}

#[derive(Clone, Default)]
pub struct Func {
    pub name: String,
    pub body: IR,
    pub is_extern: bool,
    pub is_closure: bool,
    pub args: Vec<FuncArg>,
    pub location: Location,
    pub return_type: Option<Type>,
}

impl Debug for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Fn {}() -> {} {{\n{}\n}}",
            self.name,
            self.return_type
                .as_ref()
                .map(|return_type| format!("{}", return_type))
                .unwrap_or_else(|| "Void".to_string()),
            self.body
                .iter()
                .map(|code| format!("  {:?}", code))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone, Default)]
pub struct Field {
    pub mutable: bool,
    pub public: bool,
}

impl Field {
    pub fn new(mutable: bool, public: bool) -> Self {
        Self { mutable, public }
    }
}

#[derive(Debug, Default)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Func>,
    pub fields: IndexMap<String, Field>,
}

#[derive(Debug, Default)]
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
    pub mixins: HashMap<String, MixinDeclStmt>,
    pub classes: IndexMap<String, Class>,
    pub funcs: IndexMap<String, Func>,
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
            mixins: HashMap::new(),
            funcs: IndexMap::new(),
            classes: IndexMap::new(),
            interfaces: IndexMap::new(),
            imports: IndexMap::new(),
            exports: HashMap::new(),
        }
    }
}
