use indexmap::map::IndexMap;

use atom_runtime::{AtomRef, Class, Fn, Interface, Value};

pub type ModuleId = usize;

pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub location: String,
    pub globals: Vec<Value>,
    pub funcs: Vec<AtomRef<Fn>>,
    pub interfaces: Vec<AtomRef<Interface>>,
    pub classes: IndexMap<String, AtomRef<Class>>,
}

impl Module {
    pub fn new(id: ModuleId, name: String, location: String) -> Self {
        Self {
            id,
            name,
            location,
            globals: vec![],
            funcs: vec![],
            interfaces: vec![],
            classes: IndexMap::new(),
        }
    }
}
