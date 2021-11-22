use indexmap::map::IndexMap;

use crate::runtime::{AtomRef, Class, Fn, Interface, WeakRef};

pub type ModuleId = usize;

pub enum Global {
    Fn(WeakRef<Fn>),
    Class(WeakRef<Class>),
    Interface(WeakRef<Interface>),
}

pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub filename: Option<String>,
    pub globals: Vec<Global>,
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
