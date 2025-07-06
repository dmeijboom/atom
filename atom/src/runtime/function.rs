use std::{borrow::Cow, fmt::Debug};

use bytes::Bytes;

use crate::gc::{Handle, Trace};

use super::{value::Value, Context};

#[derive(Default)]
pub struct FnBuilder {
    name: Cow<'static, str>,
    arg_count: u32,
    public: bool,
    resumable: bool,
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

    pub fn resumable(mut self, resumable: bool) -> Self {
        self.resumable = resumable;
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
            resumable: self.resumable,
            arg_count: self.arg_count,
            body: self.body,
            context: Context::default(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Fn {
    pub name: Cow<'static, str>,
    pub body: Bytes,
    pub public: bool,
    pub resumable: bool,
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

#[derive(PartialEq)]
pub enum ResumableState {
    Idle,
    Running,
    Completed,
}

pub struct Resumable<'gc> {
    pub offset: usize,
    pub locals: Vec<Value<'gc>>,
    pub func: Handle<'gc, Fn>,
    pub state: ResumableState,
}

impl<'gc> Resumable<'gc> {
    pub fn new(func: Handle<'gc, Fn>, locals: Vec<Value<'gc>>) -> Self {
        Self {
            offset: 0,
            locals,
            func,
            state: ResumableState::Idle,
        }
    }
}

impl<'gc> Trace for Resumable<'gc> {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        gc.mark(&self.func);
        self.locals.iter().for_each(|v| v.trace(gc));
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
