use std::{collections::HashMap, rc::Rc};

use wyhash2::WyHash;

use crate::gc::Trace;

use super::{
    function::{Exec, Func},
    value::Value,
};

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Rc<Func>, WyHash>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: HashMap::with_hasher(WyHash::default()),
        }
    }

    pub fn native(&self) -> bool {
        self.methods
            .iter()
            .all(|(_, func)| matches!(func.exec, Exec::Handler(_)))
    }
}

pub struct ClassBuilder {
    class: Class,
}

impl ClassBuilder {
    pub fn new(name: String) -> Self {
        Self {
            class: Class::new(name),
        }
    }

    pub fn method(mut self, func: impl FnOnce() -> Func) -> Self {
        let func = func();
        self.class.methods.insert(func.name.clone(), Rc::new(func));
        self
    }

    pub fn build(self) -> Class {
        self.class
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
            if let Some(handle) = value.handle() {
                gc.mark(handle);
            }
        }
    }
}
