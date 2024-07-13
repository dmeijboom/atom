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
    ($value:expr, $name:ident) => {
        match $value.kind {
            ValueKind::$name(value) => value,
            _ => unimplemented!(),
        }
    };
}

macro_rules! extract_heap {
    ($value:expr, $name:ident) => {
        match $value {
            HeapValue::$name(value) => value,
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
        extract_heap!(self, Buffer)
    }

    pub fn array(&self) -> &[Value] {
        extract_heap!(self, Array)
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

    pub fn new_int(i: i64) -> Self {
        Self {
            ty: Type::Int,
            kind: ValueKind::Int(i),
        }
    }

    pub fn new_float(f: f64) -> Self {
        Self {
            ty: Type::Float,
            kind: ValueKind::Float(f),
        }
    }

    pub fn new_bool(b: bool) -> Self {
        Self {
            ty: Type::Bool,
            kind: ValueKind::Bool(b),
        }
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
        extract!(self, Int)
    }

    pub fn float(self) -> f64 {
        extract!(self, Float)
    }

    pub fn bool(self) -> bool {
        extract!(self, Bool)
    }

    pub fn func(self) -> Func {
        extract!(self, Func)
    }

    pub fn heap(self) -> Handle<HeapValue> {
        match self.kind {
            ValueKind::Heap(handle) => handle,
            _ => unreachable!(),
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
