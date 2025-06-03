use std::collections::HashMap;

use wyhash2::WyHash;

use crate::gc::{Handle, Trace};

use super::{function::Fn, str::Str, value::Value, Name};

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Name,
    pub init: Option<Handle<Fn>>,
    pub methods: HashMap<Name, Fn, WyHash>,
}

impl Trace for Class {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        for (_, f) in self.methods.iter() {
            f.trace(gc);
        }
    }
}

impl Class {
    pub fn new(name: impl Into<Name>) -> Self {
        Self {
            name: name.into(),
            init: None,
            methods: HashMap::with_hasher(WyHash::default()),
        }
    }

    pub fn is_extern(&self) -> bool {
        self.methods.values().all(|f| f.is_extern())
    }
}

pub struct Object {
    pub class: Handle<Class>,
    pub attrs: HashMap<Handle<Str>, Value, WyHash>,
}

impl Trace for Object {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        self.class.trace(gc);
        gc.mark(self.class.boxed());

        for (key, value) in self.attrs.iter() {
            key.trace(gc);
            value.trace(gc);
            gc.mark(key.boxed());
        }
    }
}

impl Object {
    pub fn new(class: Handle<Class>) -> Self {
        Self {
            class,
            attrs: HashMap::default(),
        }
    }
}
