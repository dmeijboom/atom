use std::rc::Rc;

use bytes::Bytes;

use crate::opcode::Const;

use super::{class::Class, function::Fn};

#[derive(Debug, Default)]
pub struct Module {
    pub body: Bytes,
    pub consts: Vec<Const>,
    pub functions: Vec<Rc<Fn>>,
    pub classes: Vec<Rc<Class>>,
}

#[derive(Default)]
pub struct ModuleBuilder {
    module: Module,
}

impl ModuleBuilder {
    pub fn build(self) -> Module {
        self.module
    }

    pub fn function(mut self, f: impl FnOnce() -> Fn) -> Self {
        self.module.functions.push(Rc::new(f()));
        self
    }

    pub fn class(mut self, class: Class) -> Self {
        self.module.classes.push(Rc::new(class));
        self
    }
}
