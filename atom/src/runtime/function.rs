use std::{borrow::Cow, fmt::Debug};

use bytes::Bytes;

use crate::gc::Trace;

#[derive(Default)]
pub struct FnBuilder {
    name: Cow<'static, str>,
    arg_count: u32,
    public: bool,
    method: bool,
    body: Bytes,
}

impl FnBuilder {
    pub fn name(mut self, name: impl Into<Cow<'static, str>>) -> Self {
        self.name = name.into();
        self
    }

    pub fn arg_count(mut self, count: u32) -> Self {
        self.arg_count = count;
        self
    }

    pub fn public(mut self, public: bool) -> Self {
        self.public = public;
        self
    }

    pub fn method(mut self, is_method: bool) -> Self {
        self.method = is_method;
        self
    }

    pub fn body(mut self, body: Bytes) -> Self {
        self.body = body;
        self
    }

    pub fn build(self) -> Fn {
        Fn {
            name: self.name,
            arg_count: self.arg_count,
            public: self.public,
            method: self.method,
            body: self.body,
            inline: Inline::default(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Inline {
    pub instance: usize,
}

#[derive(Debug, Default, Clone)]
pub struct Fn {
    pub name: Cow<'static, str>,
    pub body: Bytes,
    pub method: bool,
    pub public: bool,
    pub arg_count: u32,
    pub inline: Inline,
}

impl Trace for Fn {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Fn {
    pub fn builder() -> FnBuilder {
        FnBuilder::default()
    }
}
