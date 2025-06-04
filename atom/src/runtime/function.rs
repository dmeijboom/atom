use std::fmt::Debug;

use bytes::Bytes;

use crate::{
    bytecode::{Bytecode, Op},
    gc::Trace,
};

use super::Name;

#[derive(Debug, Default, Clone)]
pub struct Fn {
    pub name: Name,
    pub body: Bytes,
    pub method: bool,
    pub arg_count: u32,
}

impl Trace for Fn {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Fn {
    pub fn new(name: impl Into<Name>, arg_count: u32) -> Self {
        Self {
            name: name.into(),
            arg_count,
            method: false,
            body: Bytes::default(),
        }
    }

    pub fn with_body(name: impl Into<Name>, arg_count: u32, body: Bytes) -> Self {
        Self {
            name: name.into(),
            arg_count,
            method: false,
            body,
        }
    }

    pub fn with_method(mut self) -> Self {
        self.method = true;
        self
    }

    pub fn is_extern(&self) -> bool {
        matches!(self.body.chunks_exact(8).map(Bytecode::deserialize).next(), Some(c) if c.op == Op::CallExtern)
    }
}
