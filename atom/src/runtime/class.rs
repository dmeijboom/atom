use std::{collections::HashMap, rc::Rc};

use wyhash2::WyHash;

use crate::gc::Trace;

use super::{func::Func, value::Value, Name};

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
pub struct Instance {
    pub class: Rc<Class>,
    pub attrs: HashMap<String, Value, WyHash>,
}

impl Instance {
    pub fn new(class: Rc<Class>) -> Self {
        Self {
            class,
            attrs: HashMap::default(),
        }
    }
}

impl Trace for Instance {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        for value in self.attrs.values() {
            value.trace(gc);
        }
    }
}
