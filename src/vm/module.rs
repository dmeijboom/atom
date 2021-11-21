use indexmap::map::IndexMap;

use crate::runtime::{AtomRef, Class, Fn, Interface, Value};

pub type ModuleId = usize;

pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub filename: Option<String>,
    pub globals: Vec<Value>,
    pub functions: Vec<AtomRef<Fn>>,
    pub interfaces: Vec<AtomRef<Interface>>,
    pub classes: IndexMap<String, AtomRef<Class>>,
}

impl Module {
    pub fn new(id: ModuleId, name: String, filename: Option<String>) -> Self {
        Self {
            id,
            name,
            filename,
            functions: vec![],
            globals: vec![],
            interfaces: vec![],
            classes: IndexMap::new(),
        }
    }
}
