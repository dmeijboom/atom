use std::{fmt::Debug, rc::Rc};

use crate::opcode::{Op, Opcode};

use super::Name;

#[derive(Default)]
pub struct Func {
    pub name: Name,
    pub receiver: bool,
    pub arg_count: usize,
    pub codes: Rc<[Opcode]>,
}

impl Func {
    pub fn new(name: impl Into<Name>, arg_count: usize) -> Self {
        Self {
            name: name.into(),
            arg_count,
            receiver: false,
            codes: Rc::new([]),
        }
    }

    pub fn with_codes(name: impl Into<Name>, arg_count: usize, codes: Vec<Opcode>) -> Self {
        Self {
            name: name.into(),
            arg_count,
            receiver: false,
            codes: codes.into(),
        }
    }

    pub fn with_receiver(mut self) -> Self {
        self.receiver = true;
        self
    }

    pub fn is_extern(&self) -> bool {
        matches!(self.codes.first(), Some(c) if c.op() == Op::CallExtern)
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Func")
            .field("name", &self.name)
            .field("arg_count", &self.arg_count)
            .finish()
    }
}
