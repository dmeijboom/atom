use std::fmt::Display;

use crate::{codes::BinaryOp, lexer::Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Int,
    Float,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "{}", "Int"),
            Type::Float => write!(f, "{}", "Float"),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Int(i64),
    Float(f64),
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
        }
    }

    pub fn int(self) -> i64 {
        match self {
            Value::Int(i) => i,
            _ => unimplemented!(),
        }
    }

    pub fn float(self) -> f64 {
        match self {
            Value::Float(f) => f,
            _ => unimplemented!(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(fmt, "{i}"),
            Value::Float(f) => write!(fmt, "{f}"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("invalid const at: {0}")]
    InvalidConst(usize),
    #[error("stack is empty")]
    StackEmpty,
    #[error("binary type mismatch: {left} != {right}")]
    BinaryTypeMismatch { left: Type, right: Type },
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
