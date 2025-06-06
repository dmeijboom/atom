use std::fmt::Debug;

use bytes::Bytes;

use crate::gc::Trace;

use super::Name;

#[derive(Debug, Default, Clone)]
pub struct Fn {
    pub name: Name,
    pub body: Bytes,
    pub method: bool,
    pub arg_count: u32,
    pub instance_id: usize,
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
            instance_id: 0,
        }
    }

    pub fn with_body(name: impl Into<Name>, arg_count: u32, body: Bytes) -> Self {
        Self {
            name: name.into(),
            arg_count,
            method: false,
            body,
            instance_id: 0,
        }
    }

    pub fn with_method(mut self) -> Self {
        self.method = true;
        self
    }
}
