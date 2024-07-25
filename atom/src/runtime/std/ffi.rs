use crate::{
    gc::{Gc, Handle, Trace},
    lexer::Span,
    runtime::{error::RuntimeError, value::Value},
};

use super::{array::Array, str::Str};

pub struct Context<'a> {
    pub gc: &'a mut Gc,
    pub span: Span,
}

impl<'a> Context<'a> {
    #[cfg(test)]
    pub fn new(gc: &'a mut Gc) -> Self {
        Self {
            gc,
            span: Span::default(),
        }
    }

    pub fn with_span(gc: &'a mut Gc, span: Span) -> Self {
        Self { gc, span }
    }
}

pub type FnHandler = dyn Fn(Context<'_>, Vec<Value>) -> Result<Value, RuntimeError>;
pub type FieldHandler = dyn Fn(Context<'_>, Value) -> Result<Value, RuntimeError>;

pub trait Convert<'a, T> {
    fn convert(self, gc: &'a mut Gc) -> T
    where
        T: 'a;
}

impl<'a, T: From<Value>> Convert<'a, T> for T {
    fn convert(self, _gc: &'a mut Gc) -> T
    where
        T: 'a,
    {
        self
    }
}

impl<'a, T: Trace> Convert<'a, &'a mut T> for Handle<T> {
    fn convert(self, gc: &'a mut Gc) -> &'a mut T
    where
        &'a mut T: 'a,
    {
        gc.get_mut(self)
    }
}

impl<'a, T: Trace> Convert<'a, &'a T> for Handle<T> {
    fn convert(self, gc: &'a mut Gc) -> &'a T
    where
        &'a mut T: 'a,
    {
        gc.get(self)
    }
}

impl<'a> Convert<'a, &'a Array<Value>> for Value {
    fn convert(self, gc: &'a mut Gc) -> &'a Array<Value>
    where
        &'a Array<Value>: 'a,
    {
        self.array().convert(gc)
    }
}

impl<'a> Convert<'a, &'a mut Array<Value>> for Value {
    fn convert(self, gc: &'a mut Gc) -> &'a mut Array<Value>
    where
        &'a Array<Value>: 'a,
    {
        self.array().convert(gc)
    }
}

impl<'a> Convert<'a, &'a Str> for Value {
    fn convert(self, gc: &'a mut Gc) -> &'a Str
    where
        &'a Str: 'a,
    {
        self.str().convert(gc)
    }
}
