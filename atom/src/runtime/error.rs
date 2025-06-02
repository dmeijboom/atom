use std::fmt::{Debug, Display};

use crate::{gc::Handle, lexer::Span};

use super::{class::Class, function::Fn, value::Type};

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("type mismatch: {left} != {right}")]
    TypeMismatch { left: Type, right: Type },
    #[error("index out of bounds: {0}")]
    IndexOutOfBounds(usize),
    #[error("unable to use 'self' outside of a method")]
    NoReceiver,
    #[error("cannot call non-function: {0}")]
    NotCallable(Type),
    #[error("invalid argument count on '{}(..)': expected {}, got: {arg_count}", func.name, func.arg_count)]
    ArgCountMismatch { arg_count: usize, func: Handle<Fn> },
    #[error("no such field '{field}' in {ty}")]
    UnknownField { ty: Type, field: String },
    #[error("no such attribute '{attribute}' in {}", class.name)]
    UnknownAttr {
        class: Handle<Class>,
        attribute: String,
    },
    #[error("out of memory")]
    OutOfMemory,
    #[error("invalid memory layout")]
    InvalidMemoryLayout,
    #[error("unsupported operation: {left} {op} {right}")]
    UnsupportedBinaryOp {
        left: Type,
        right: Type,
        op: &'static str,
    },
    #[error("unsupported operation `{op}` on: {ty}")]
    UnsupportedOp { ty: Type, op: &'static str },
}

impl ErrorKind {
    pub fn at(self, span: Span) -> RuntimeError {
        RuntimeError {
            kind: self,
            span,
            trace: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    #[allow(dead_code)]
    pub span: Span,
    pub func: Handle<Fn>,
}

impl Call {
    pub fn new(span: Span, func: Handle<Fn>) -> Self {
        Call { span, func }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(..)", self.func.name)
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: ErrorKind,
    #[allow(dead_code)]
    pub span: Span,
    pub trace: Option<Vec<Call>>,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.kind, f)
    }
}

impl std::error::Error for RuntimeError {}
