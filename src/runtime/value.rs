use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::gc::{Gc, Handle, Trace};

use super::function::Func;

#[repr(u64)]
enum Tag {
    Int,
    Float,
    Array,
    Fn,
    Str,
    True,
    False,
    Nil,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Array,
    Fn,
    Str,
    Nil,
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
            Type::Nil => write!(f, "Nil"),
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
    fn trace(&self, gc: &mut Gc) {
        match self {
            HeapValue::Buffer(_) => {}
            HeapValue::Array(items) => {
                for item in items.iter() {
                    if matches!(item.ty(), Type::Array | Type::Str) {
                        gc.mark(item.handle());
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

#[derive(Clone, Copy)]
pub struct Value {
    bits: u64,
}

impl Value {
    const FALSE: Self = Self::new_primitive(Tag::False);
    const TRUE: Self = Self::new_primitive(Tag::True);
    const NAN: Self = Self::new_primitive(Tag::Float);
    pub const NIL: Self = Self::new_primitive(Tag::Nil);

    pub const fn is_handle(&self) -> bool {
        let t = (self.bits & TAG_MASK) >> 48;
        t == Tag::Array as u64 || t == Tag::Str as u64
    }

    pub const fn ty(&self) -> Type {
        if self.bits == Self::NAN.bits || (self.bits & QUIET_NAN) != QUIET_NAN {
            return Type::Float;
        }

        match (self.bits & TAG_MASK) >> 48 {
            t if t == Tag::Int as u64 => Type::Int,
            t if t == Tag::Float as u64 => Type::Float,
            t if t == Tag::True as u64 || t == Tag::False as u64 => Type::Bool,
            t if t == Tag::Array as u64 => Type::Array,
            t if t == Tag::Fn as u64 => Type::Fn,
            t if t == Tag::Str as u64 => Type::Str,
            t if t == Tag::Nil as u64 => Type::Nil,
            _ => unreachable!(),
        }
    }

    const fn new_primitive(tag: Tag) -> Self {
        Self {
            bits: (tag as u64) << 48 | QUIET_NAN,
        }
    }

    const fn new(tag: Tag, value: u64) -> Self {
        Self {
            bits: (tag as u64) << 48 | QUIET_NAN | value,
        }
    }

    pub fn new_str(handle: Handle<HeapValue>) -> Self {
        Self::new(Tag::Str, handle.addr() as u64)
    }

    pub fn new_array(handle: Handle<HeapValue>) -> Self {
        Self::new(Tag::Array, handle.addr() as u64)
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
        (self.bits & TAG_MASK) >> 48 == Tag::True as u64
    }

    pub fn func(self) -> Rc<Func> {
        unsafe {
            let value = self.bits & INT_MASK;
            let func = value as *const Func;

            Rc::from_raw(func)
        }
    }

    pub fn handle(self) -> Handle<HeapValue> {
        let addr = self.bits & INT_MASK;
        Handle::from_addr(addr as usize).unwrap()
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::NIL
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty() {
            Type::Int => write!(f, "{}", self.int()),
            Type::Float => write!(f, "{}", self.float()),
            Type::Bool => write!(f, "{}", self.bool()),
            Type::Array => write!(f, "Array"),
            Type::Fn => write!(f, "Fn"),
            Type::Str => write!(f, "Str"),
            Type::Nil => write!(f, "Nil"),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Value{{")?;
        std::fmt::Display::fmt(&self, f)?;
        write!(f, "}}")
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        if value.unsigned_abs() > INT_MASK {
            panic!("integer overflow");
        }

        Self::new(
            Tag::Int,
            if value < 0 {
                SIGN_BIT | value.unsigned_abs()
            } else {
                value as u64
            },
        )
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Self {
            bits: value.to_bits(),
        }
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

#[cfg(test)]
mod tests {
    use crate::gc::Gc;

    use super::*;

    #[test]
    fn test_int() {
        for i in -1000000..1000000 {
            let value = Value::from(i);
            assert_eq!(value.int(), i);
        }
    }

    #[test]
    fn test_float() {
        let mut i = -10000.0;

        while i < 10000.0 {
            let value = Value::from(i);
            assert_eq!(value.float(), i);

            i += 0.1;
        }
    }

    #[test]
    fn test_bool() {
        let value = Value::from(true);
        assert_eq!(value.bool(), true);

        let value = Value::from(false);
        assert_eq!(value.bool(), false);
    }

    #[test]
    fn test_string() {
        let mut gc = Gc::default();

        let handle = gc.alloc(HeapValue::Buffer(b"hello".to_vec()));
        let value = Value::new_str(handle);

        assert_eq!(value.ty(), Type::Str);

        let handle = value.handle();
        let heap_value = gc.get(handle);

        match heap_value {
            HeapValue::Buffer(buffer) => {
                assert_eq!(buffer, b"hello");
            }
            HeapValue::Array(_) => unreachable!(),
        }
    }
}
