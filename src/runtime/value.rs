use std::fmt::Display;

use broom::{trace::Trace, Handle};

use crate::codes::Func;

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Array,
    Fn,
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
            Type::Fn => write!(f, "Fn"),
        }
    }
}

macro_rules! extract {
    ($ty:ident::$name:ident, $value:expr) => {
        match $value {
            $ty::$name(value) => value,
            _ => unimplemented!(),
        }
    };
}

#[derive(Debug)]
pub enum HeapValue {
    Buffer(Vec<u8>),
    Array(Vec<Value>),
}

impl HeapValue {
    pub fn buffer(&self) -> &[u8] {
        extract!(HeapValue::Buffer, self)
    }

    pub fn array(&self) -> &[Value] {
        extract!(HeapValue::Array, self)
    }
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

    pub fn kind(&self) -> &ValueKind {
        &self.kind
    }

    pub fn new_str(handle: Handle<HeapValue>) -> Self {
        Self {
            ty: Type::Str,
            kind: ValueKind::Heap(handle),
        }
    }

    pub fn new_array(handle: Handle<HeapValue>) -> Self {
        Self {
            ty: Type::Array,
            kind: ValueKind::Heap(handle),
        }
    }

    pub fn new_func(func: Func) -> Self {
        Self {
            ty: Type::Fn,
            kind: ValueKind::Func(func),
        }
    }

    pub fn int(self) -> i64 {
        extract!(ValueKind::Int, self.kind)
    }

    pub fn float(self) -> f64 {
        extract!(ValueKind::Float, self.kind)
    }

    pub fn bool(self) -> bool {
        extract!(ValueKind::Bool, self.kind)
    }

    pub fn func(self) -> Func {
        extract!(ValueKind::Func, self.kind)
    }

    pub fn heap(self) -> Handle<HeapValue> {
        extract!(ValueKind::Heap, self.kind)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self {
            ty: Type::Int,
            kind: ValueKind::Int(value),
        }
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self {
            ty: Type::Float,
            kind: ValueKind::Float(value),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self {
            ty: Type::Bool,
            kind: ValueKind::Bool(value),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ValueKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    Func(Func),
    Heap(Handle<HeapValue>),
}

impl Display for ValueKind {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Int(i) => write!(fmt, "{i}"),
            ValueKind::Float(f) => write!(fmt, "{f}"),
            ValueKind::Bool(b) => write!(fmt, "{b}"),
            ValueKind::Heap(..) => write!(fmt, "<heap>"),
            ValueKind::Func(..) => write!(fmt, "<func>"),
        }
    }
}

impl ValueKind {
    pub fn is_number(&self) -> bool {
        matches!(self, ValueKind::Int(_) | ValueKind::Float(_))
    }
}
