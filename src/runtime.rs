use std::fmt::Display;

use broom::{trace::Trace, Handle};

use crate::{codes::BinaryOp, lexer::Span};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Array,
    Str,
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
            ValueKind::$name(value) => value,
            _ => unimplemented!(),
        }
    };
}

#[derive(Debug)]
pub enum HeapValue {
    Buffer(Vec<u8>),
    Array(Vec<Value>),
}

impl Trace<Self> for HeapValue {
    fn trace(&self, tracer: &mut broom::prelude::Tracer<Self>) {
        match self {
            HeapValue::Buffer(_) => {}
            HeapValue::Array(values) => {
                for value in values {
                    if let ValueKind::Heap(value) = value.kind {
                        value.trace(tracer);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Value {
    ty: Type,
    kind: ValueKind,
}

impl Value {
    pub fn ty(&self) -> Type {
        self.ty
    }

    pub fn kind(self) -> ValueKind {
        self.kind
    }

    pub fn kind_ref(&self) -> &ValueKind {
        &self.kind
    }

    pub fn int(i: i64) -> Self {
        Self {
            ty: Type::Int,
            kind: ValueKind::Int(i),
        }
    }

    pub fn float(f: f64) -> Self {
        Self {
            ty: Type::Float,
            kind: ValueKind::Float(f),
        }
    }

    pub fn bool(b: bool) -> Self {
        Self {
            ty: Type::Bool,
            kind: ValueKind::Bool(b),
        }
    }

    pub fn str(handle: Handle<HeapValue>) -> Self {
        Self {
            ty: Type::Str,
            kind: ValueKind::Heap(handle),
        }
    }

    pub fn array(handle: Handle<HeapValue>) -> Self {
        Self {
            ty: Type::Array,
            kind: ValueKind::Heap(handle),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Heap(Handle<HeapValue>),
}

impl ValueKind {
    pub fn int(self) -> i64 {
        extract!(self, Int)
    }

    pub fn float(self) -> f64 {
        extract!(self, Float)
    }

    pub fn bool(self) -> bool {
        extract!(self, Bool)
    }

    pub fn is_number(&self) -> bool {
        matches!(self, ValueKind::Int(_) | ValueKind::Float(_))
    }

    pub fn heap(self) -> Handle<HeapValue> {
        match self {
            ValueKind::Heap(handle) => handle,
            _ => unreachable!(),
        }
    }
}

impl Display for ValueKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Int(i) => write!(fmt, "{i}"),
            ValueKind::Float(f) => write!(fmt, "{f}"),
            ValueKind::Bool(b) => write!(fmt, "{b}"),
            ValueKind::Heap(..) => write!(fmt, "<heap>"),
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
