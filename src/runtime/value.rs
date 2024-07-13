use std::{fmt::Display, rc::Rc};

use safe_gc::{Collector, Gc, Trace};

use crate::codes::Func;

#[repr(u64)]
enum Tag {
    Int,
    Float,
    Array,
    Fn,
    Str,
    True,
    False,
}

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

impl Trace for HeapValue {
    fn trace(&self, collector: &mut Collector) {
        match self {
            HeapValue::Buffer(_) => {}
            HeapValue::Array(values) => {
                for value in values {
                    if matches!(value.ty(), Type::Array | Type::Str) {
                        collector.edge(value.heap());
                    }
                }
            }
        }
    }
}

const SIGN_BIT: u64 = 1 << 63;
const QUIET_NAN: u64 = 0x7ff8_0000_0000_0000;
const INT_MASK: u64 = 0xffff_ffff_ffff;
const TAG_MASK: u64 = 0b111 << 48;

fn handle_to_bits(handle: Gc<HeapValue>) -> u64 {
    let (low, hi) = handle.into_raw_parts();
    (hi as u64) << 32 | low as u64
}

#[derive(Debug, Clone, Copy)]
pub struct Value {
    bits: u64,
}

impl Value {
    const FALSE: Self = Self::new_primitive(Tag::False);
    const TRUE: Self = Self::new_primitive(Tag::True);

    pub const fn ty(self) -> Type {
        match (self.bits & TAG_MASK) >> 48 {
            t if t == Tag::Int as u64 => Type::Int,
            t if t == Tag::Float as u64 => Type::Float,
            t if t == Tag::True as u64 || t == Tag::False as u64 => Type::Bool,
            t if t == Tag::Array as u64 => Type::Array,
            t if t == Tag::Fn as u64 => Type::Fn,
            t if t == Tag::Str as u64 => Type::Str,
            _ => unreachable!(),
        }
    }

    pub fn is_number(self) -> bool {
        matches!(self.ty(), Type::Int | Type::Float)
    }

    const fn new_primitive(tag: Tag) -> Self {
        Self {
            bits: (tag as u64) << 48,
        }
    }

    const fn new(tag: Tag, value: u64) -> Self {
        Self {
            bits: (tag as u64) << 48 | QUIET_NAN | value,
        }
    }

    pub fn new_str(handle: Gc<HeapValue>) -> Self {
        Self::new(Tag::Str, handle_to_bits(handle))
    }

    pub fn new_array(handle: Gc<HeapValue>) -> Self {
        Self::new(Tag::Array, handle_to_bits(handle))
    }

    pub fn new_func(func: Rc<Func>) -> Self {
        let value = Rc::into_raw(func);
        Self::new(Tag::Fn, value as u64)
    }

    pub fn int(self) -> i64 {
        match self.ty() {
            Type::Int => {
                let bits = self.bits & INT_MASK;

                if self.bits & SIGN_BIT != 0 {
                    -(bits as i64)
                } else {
                    bits as i64
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn float(self) -> f64 {
        f64::from_bits(self.bits)
    }

    pub fn bool(self) -> bool {
        (self.bits & TAG_MASK) >> 48 == Self::TRUE.bits
    }

    pub fn func(self) -> Rc<Func> {
        unsafe {
            let value = self.bits & INT_MASK;
            let func = value as *const Func;

            Rc::from_raw(func)
        }
    }

    pub fn heap(self) -> Gc<HeapValue> {
        let value = self.bits & INT_MASK;
        let low = value as u32;
        let hi = (value >> 32) as u32;

        Gc::from_raw_parts(low, hi)
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::new_primitive(Tag::False)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::new(Tag::Int, value as u64)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self::new(Tag::Float, value.to_bits())
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        if value {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }
}
