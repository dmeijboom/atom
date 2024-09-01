use bytes::Bytes;

use crate::opcode::Const;

use super::{class::Class, function::Fn};

#[derive(Debug, Default)]
pub struct Module {
    pub body: Bytes,
    pub consts: Vec<Const>,
    pub functions: Vec<Fn>,
    pub classes: Vec<Class>,
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
        self.module.functions.push(f());
        self
    }

    pub fn class(mut self, class: Class) -> Self {
        self.module.classes.push(class);
        self
    }
}
