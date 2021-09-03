use indexmap::map::IndexMap;

use atom_runtime::{AtomRef, Class, Fn, Interface, Value};

pub type ModuleId = usize;

pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub filename: Option<String>,
    pub globals: Vec<Value>,
    pub funcs: Vec<AtomRef<Fn>>,
    pub closures: Vec<AtomRef<Fn>>,
    pub interfaces: Vec<AtomRef<Interface>>,
    pub classes: IndexMap<String, AtomRef<Class>>,
}

impl Module {
    pub fn new(id: ModuleId, name: String, filename: Option<String>) -> Self {
        Self {
            id,
            name,
            filename,
            funcs: vec![],
            globals: vec![],
            closures: vec![],
            interfaces: vec![],
            classes: IndexMap::new(),
        }
    }
}
