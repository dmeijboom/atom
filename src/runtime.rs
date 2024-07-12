use std::fmt::Display;

use broom::{trace::Trace, Handle};

use crate::{codes::BinaryOp, lexer::Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
    Array,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::Str => write!(f, "Str"),
            Type::Array => write!(f, "Array"),
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

#[derive(Debug)]
pub enum HeapValue {
    Str(String),
    Array(Vec<Value>),
}

impl Display for HeapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapValue::Str(s) => write!(f, "\"{s}\""),
            HeapValue::Array(values) => {
                write!(f, "[")?;

                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}", value)?;
                }

                write!(f, "]")
            }
        }
    }
}

impl Trace<Self> for HeapValue {
    fn trace(&self, tracer: &mut broom::prelude::Tracer<Self>) {
        match self {
            HeapValue::Str(_) => {}
            HeapValue::Array(values) => {
                for value in values {
                    if let Value::Heap(_, value) = value {
                        value.trace(tracer);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Heap(Type, Handle<HeapValue>),
}

impl Value {
    pub fn ty(&self) -> Type {
        match self {
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Bool(_) => Type::Bool,
            Value::Heap(ty, _) => *ty,
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

    pub fn heap(self) -> Handle<HeapValue> {
        match self {
            Value::Heap(_, value) => value,
            _ => unreachable!(),
        }
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(fmt, "{i}"),
            Value::Float(f) => write!(fmt, "{f}"),
            Value::Bool(b) => write!(fmt, "{b}"),
            Value::Heap(..) => write!(fmt, "<heap>"),
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
    #[error("segmentation fault: heap value is nil")]
    Segfault,
    #[error("index out of bounds: {0}")]
    IndexOutOfBounds(usize),
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
