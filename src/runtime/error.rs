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
    #[error("no such field '{field}' in {ty}")]
    NoSuchField { ty: Type, field: String },
    #[error("unsupported operation '{op:?}' for {left} and {right}")]
    UnsupportedOp {
        left: Type,
        right: Type,
        op: BinaryOp,
    },
}

impl ErrorKind {
    pub fn at(self, span: Span) -> Error {
        Error { kind: self, span }
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl std::error::Error for Error {}
