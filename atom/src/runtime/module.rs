use std::rc::Rc;

use bytes::Bytes;

use crate::opcode::Const;

use super::{class::Class, func::Func};

#[derive(Debug, Default)]
pub struct Module {
    pub body: Bytes,
    pub consts: Vec<Const>,
    pub funcs: Vec<Rc<Func>>,
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

    pub fn func(mut self, func: impl FnOnce() -> Func) -> Self {
        self.module.funcs.push(Rc::new(func()));
        self
    }

    pub fn class(mut self, class: Class) -> Self {
        self.module.classes.push(Rc::new(class));
        self
    }
}
