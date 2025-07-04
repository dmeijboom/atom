use std::{borrow::Cow, fmt::Debug};

use bytes::Bytes;

use crate::gc::{Handle, Trace};

use super::value::Value;

#[derive(Default)]
pub struct FnBuilder {
    name: Cow<'static, str>,
    arg_count: u32,
    public: bool,
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

    pub fn body(mut self, body: Bytes) -> Self {
        self.body = body;
        self
    }

    pub fn build(self) -> Fn {
        Fn {
            name: self.name,
            public: self.public,
            arg_count: self.arg_count,
            body: self.body,
            context: Context::default(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Context {
    pub module: usize,
}

impl Context {
    pub fn with_module(module: usize) -> Self {
        Self { module }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Fn {
    pub name: Cow<'static, str>,
    pub body: Bytes,
    pub public: bool,
    pub arg_count: u32,
    pub context: Context,
}

impl Trace for Fn {
    fn trace(&self, _gc: &mut crate::gc::Gc) {}
}

impl Fn {
    pub fn builder() -> FnBuilder {
        FnBuilder::default()
    }
}

pub struct Method<'gc> {
    pub recv: Value<'gc>,
    pub func: Handle<'gc, Fn>,
}

impl<'gc> Method<'gc> {
    pub fn new(recv: Value<'gc>, func: Handle<'gc, Fn>) -> Self {
        Self { func, recv }
    }
}

impl<'gc> Trace for Method<'gc> {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        gc.mark(&self.func);
        self.recv.trace(gc);
    }
}
