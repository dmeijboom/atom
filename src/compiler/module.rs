use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use enumflags2::{bitflags, BitFlags};
use indexmap::map::IndexMap;

use crate::compiler::ir::{Location, IR};
use crate::syntax::MixinDeclStmt;

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

#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionAttr {
    // Has to match `Modifier::Public`
    Public,
    // Has to match `Modifier::Static`
    Static,
    Extern,
    Closure,
}

#[derive(Clone, Default)]
pub struct Function {
    pub name: String,
    pub body: IR,
    pub attr: BitFlags<FunctionAttr>,
    pub args: Vec<FuncArg>,
    pub location: Location,
}

impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Fn {}() {{\n{}\n}}",
            self.name,
            self.body
                .iter()
                .map(|code| format!("  {}", code.description(Some(&self.body))))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone, Default)]
pub struct Field {
    pub name: String,
    pub mutable: bool,
    pub public: bool,
}

impl Field {
    pub fn new(name: String, mutable: bool, public: bool) -> Self {
        Self {
            name,
            mutable,
            public,
        }
    }
}

#[derive(Debug, Default)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Function>,
    pub fields: IndexMap<String, Field>,
}

#[derive(Debug, Default)]
pub struct Interface {
    pub name: String,
    pub methods: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum ElementKind {
    Fn,
    Class,
    Interface,
}

#[derive(Debug, Clone)]
pub struct Id {
    pub name: String,
    pub module: String,
}

impl Id {
    pub fn new(module: String, name: String) -> Self {
        Self { module, name }
    }
}

#[derive(Debug, Clone)]
pub struct Element {
    pub kind: ElementKind,
    pub id: Id,
}

impl Element {
    pub fn new(kind: ElementKind, id: Id) -> Self {
        Self { kind, id }
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub filename: Option<String>,
    pub mixins: HashMap<String, MixinDeclStmt>,
    pub classes: IndexMap<String, Class>,
    pub functions: IndexMap<String, Function>,
    pub interfaces: IndexMap<String, Interface>,
    pub imports: IndexMap<String, Element>,
    pub exports: HashMap<String, Element>,
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
            functions: IndexMap::new(),
            classes: IndexMap::new(),
            interfaces: IndexMap::new(),
            imports: IndexMap::new(),
            exports: HashMap::new(),
        }
    }
}
