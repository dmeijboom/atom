use linear_map::LinearMap;

use crate::gc::{Handle, Trace};

use super::{function::Fn, str::Str, value::Value, Name};

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Name,
    pub init: Option<Handle<Fn>>,
    pub methods: LinearMap<Name, Fn>,
}

impl Trace for Class {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        for (_, f) in self.methods.iter() {
            f.trace(gc);
        }

        if let Some(init) = &self.init {
            gc.mark(init);
        }
    }
}

impl Class {
    pub fn new(name: impl Into<Name>) -> Self {
        Self {
            name: name.into(),
            init: None,
            methods: LinearMap::default(),
        }
    }
}

pub struct Object {
    pub class: Handle<Class>,
    pub attrs: LinearMap<Handle<Str>, Value>,
}

impl Trace for Object {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        gc.mark(&self.class);

        for (key, value) in self.attrs.iter() {
            value.trace(gc);
            gc.mark(key);
        }
    }
}

impl Object {
    pub fn new(class: Handle<Class>) -> Self {
        Self {
            class,
            attrs: LinearMap::default(),
        }
    }
}
