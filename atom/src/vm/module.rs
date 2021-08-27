use std::collections::HashMap;

use wyhash2::WyHash;

use atom_runtime::{AtomRef, Class, Fn, Interface, Value};

pub type ModuleId = usize;

pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub location: String,
    pub globals: HashMap<String, Value, WyHash>,
    pub funcs: HashMap<String, AtomRef<Fn>, WyHash>,
    pub classes: HashMap<String, AtomRef<Class>, WyHash>,
    pub interfaces: HashMap<String, AtomRef<Interface>, WyHash>,
}

impl Module {
    pub fn new(id: ModuleId, name: String, location: String) -> Self {
        Self {
            id,
            name,
            location,
            globals: HashMap::with_hasher(WyHash::default()),
            funcs: HashMap::with_hasher(WyHash::default()),
            classes: HashMap::with_hasher(WyHash::default()),
            interfaces: HashMap::with_hasher(WyHash::default()),
        }
    }
}
