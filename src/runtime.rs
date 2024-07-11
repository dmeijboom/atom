use std::fmt::Display;

use crate::{codes::BinaryOp, lexer::Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
        }
    }
}

macro_rules! extract {
    ($value:expr, $name:ident) => {
        match $value {
            Value::$name(value) => value,
            _ => unimplemented!(),
        }
    };
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Bool(_) => Type::Bool,
        }
    }

    pub fn is_number(&self) -> bool {
        matches!(self.ty(), Type::Int | Type::Float)
    }

    pub fn int(self) -> i64 {
        extract!(self, Int)
    }

    pub fn float(self) -> f64 {
        extract!(self, Float)
    }

    pub fn bool(self) -> bool {
        extract!(self, Bool)
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(fmt, "{i}"),
            Value::Float(f) => write!(fmt, "{f}"),
            Value::Bool(b) => write!(fmt, "{b}"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("invalid const at: {0}")]
    InvalidConst(usize),
    #[error("invalid var at: {0}")]
    InvalidVar(usize),
    #[error("stack is empty")]
    StackEmpty,
    #[error("type mismatch: {left} != {right}")]
    TypeMismatch { left: Type, right: Type },
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
