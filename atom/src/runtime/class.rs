use std::{collections::HashMap, rc::Rc};

use wyhash2::WyHash;

use crate::gc::{Handle, Trace};

use super::{func::Func, str::Str, value::Value, Name};

#[derive(Debug)]
pub struct Class {
    pub name: Name,
    pub methods: HashMap<Name, Rc<Func>, WyHash>,
}

impl Class {
    pub fn new(name: impl Into<Name>) -> Self {
        Self {
            name: name.into(),
            methods: HashMap::with_hasher(WyHash::default()),
        }
    }

    pub fn is_extern(&self) -> bool {
        self.methods.values().all(|f| f.is_extern())
    }
}

#[derive(Debug)]
pub struct Object {
    pub class: Rc<Class>,
    attrs: HashMap<Handle<Str>, Value, WyHash>,
}

impl Object {
    pub fn new(class: Rc<Class>) -> Self {
        Self {
            class,
            attrs: HashMap::default(),
        }
    }

    pub fn get_attr(&self, key: &Handle<Str>) -> Option<&Value> {
        self.attrs.get(key)
    }

    pub fn set_attr(&mut self, key: Handle<Str>, value: Value) {
        self.attrs.insert(key, value);
    }
}

impl Trace for Object {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        for (key, value) in self.attrs.iter() {
            key.trace(gc);
            value.trace(gc);
            gc.mark(key.boxed());
        }
    }
}
