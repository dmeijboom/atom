use std::fmt::Display;

use crate::{codes::BinaryOp, lexer::Span};

use super::value::Type;

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("type mismatch: {left} != {right}")]
    TypeMismatch { left: Type, right: Type },
    #[error("segmentation fault: heap value is nil")]
    Segfault,
    #[error("index out of bounds: {0}")]
    IndexOutOfBounds(usize),
    #[error("cannot call non-function: {0}")]
    NotCallable(Type),
    #[error("no such field '{field}' in {ty}")]
    UnknownField { ty: Type, field: String },
    #[error("no such function: {0}")]
    UnknownFunc(String),
    #[error("unsupported operation '{op:?}' for {left} and {right}")]
    UnsupportedOp {
        left: Type,
        right: Type,
        op: BinaryOp,
    },
}

impl ErrorKind {
    pub fn at(self, span: Span) -> Error {
        Error {
            kind: self,
            span,
            trace: None,
        }
    }
}

#[derive(Debug)]
pub struct Call {
    pub span: Span,
    pub name: String,
    pub receiver: Option<Type>,
}

impl Call {
    pub fn new(span: Span, name: String) -> Self {
        Call {
            span,
            name,
            receiver: None,
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
    pub trace: Option<Vec<Call>>,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl std::error::Error for Error {}
