use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{
    codes::{BinaryOp, Func},
    lexer::Span,
};

use super::value::Type;

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("type mismatch: {left} != {right}")]
    TypeMismatch { left: Type, right: Type },
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

#[derive(Default, Clone)]
pub struct Call {
    pub span: Span,
    pub func: Rc<Func>,
    pub receiver: Option<Type>,
}

impl Call {
    pub fn new(span: Span, func: Rc<Func>) -> Self {
        Call {
            span,
            func,
            receiver: None,
        }
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(..)", self.func.name)
    }
}

impl Debug for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Call{{")?;
        std::fmt::Display::fmt(&self, f)?;
        write!(f, "}}")
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
        std::fmt::Display::fmt(&self.kind, f)
    }
}

impl std::error::Error for Error {}
