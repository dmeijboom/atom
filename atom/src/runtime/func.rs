use std::fmt::Debug;

use crate::{
    gc::Trace,
    opcode::{Op, Opcode},
};

use super::Name;

#[derive(Default)]
pub struct Func {
    pub name: Name,
    pub method: bool,
    pub arg_count: usize,
    pub body: Vec<Opcode>,
}

impl Func {
    pub fn new(name: impl Into<Name>, arg_count: usize) -> Self {
        Self {
            name: name.into(),
            arg_count,
            method: false,
            body: vec![],
        }
    }

    pub fn with_codes(name: impl Into<Name>, arg_count: usize, codes: Vec<Opcode>) -> Self {
        Self {
            name: name.into(),
            arg_count,
            method: false,
            body: codes,
        }
    }

    pub fn with_method(mut self) -> Self {
        self.method = true;
        self
    }

    pub fn is_extern(&self) -> bool {
        matches!(self.body.first(), Some(c) if c.op() == Op::CallExtern)
    }
}

impl Trace for Func {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Func")
            .field("name", &self.name)
            .field("arg_count", &self.arg_count)
            .finish()
    }
}
