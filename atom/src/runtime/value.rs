use std::{
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::gc::{AnyHandle, Gc, Handle, Trace};

use super::{
    class::{Class, Instance},
    func::Func,
    std::{array::Array, str::Str},
};

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
    Class,
    Instance,
}

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Array,
    Fn,
    Str,
    Nil,
    Class,
    Instance,
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Int => "Int",
            Type::Float => "Float",
            Type::Bool => "Bool",
            Type::Array => "Array",
            Type::Fn => "Fn",
            Type::Str => "Str",
            Type::Nil => "Nil",
            Type::Class => "Class",
            Type::Instance => "Instance",
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

const SIGN_BIT: u64 = 1 << 63;
const QUIET_NAN: u64 = 0x7ff0_0000_0000_0000;
const INT_MASK: u64 = 0xffff_ffff_ffff;
const TAG_MASK: u64 = 0b1111 << 48;

#[derive(Clone, Copy)]
pub struct Value {
    bits: u64,
}

impl Value {
    const FALSE: Self = Self::new_primitive(Tag::False);
    const TRUE: Self = Self::new_primitive(Tag::True);
    const NAN: Self = Self::new_primitive(Tag::Float);
    pub const NIL: Self = Self::new_primitive(Tag::Nil);

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
            t if t == Tag::Class as u64 => Type::Class,
            t if t == Tag::Instance as u64 => Type::Instance,
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

    pub fn handle(&self) -> Option<Box<dyn AnyHandle>> {
        match self.ty() {
            Type::Array => Some(Box::new(self.array())),
            Type::Str => Some(Box::new(self.str())),
            Type::Instance => Some(Box::new(self.instance())),
            Type::Int | Type::Float | Type::Bool | Type::Fn | Type::Class | Type::Nil => None,
        }
    }

    fn into_rc<T>(self) -> Rc<T> {
        unsafe {
            let value = self.bits & INT_MASK;
            let ptr = value as *const T;

            Rc::from_raw(ptr)
        }
    }

    pub fn class(self) -> Rc<Class> {
        self.into_rc()
    }

    pub fn func(self) -> Rc<Func> {
        self.into_rc()
    }

    fn into_handle<T: Trace>(self) -> Handle<T> {
        let addr = self.bits & INT_MASK;
        Handle::from_addr(addr as usize).unwrap()
    }

    pub fn instance(self) -> Handle<Instance> {
        self.into_handle()
    }

    pub fn str(self) -> Handle<Str> {
        self.into_handle()
    }

    pub fn array(self) -> Handle<Array<Value>> {
        self.into_handle()
    }
}

impl Trace for Value {
    fn trace(&self, gc: &mut Gc) {
        if let Some(handle) = self.handle() {
            handle.trace(gc);
        }
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
            Type::Class => write!(f, "Class"),
            Type::Instance => write!(f, "Instance"),
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

impl From<usize> for Value {
    fn from(value: usize) -> Self {
        (value as i64).into()
    }
}

impl From<Handle<Instance>> for Value {
    fn from(instance: Handle<Instance>) -> Self {
        Self::new(Tag::Instance, instance.addr() as u64)
    }
}

impl From<Rc<Class>> for Value {
    fn from(class: Rc<Class>) -> Self {
        let value = Rc::into_raw(class);
        Self::new(Tag::Class, value as u64)
    }
}

impl From<Handle<Str>> for Value {
    fn from(str: Handle<Str>) -> Self {
        Self::new(Tag::Str, str.addr() as u64)
    }
}

impl From<Handle<Array<Value>>> for Value {
    fn from(array: Handle<Array<Value>>) -> Self {
        Self::new(Tag::Array, array.addr() as u64)
    }
}

impl From<Rc<Func>> for Value {
    fn from(func: Rc<Func>) -> Self {
        let value = Rc::into_raw(func);
        Self::new(Tag::Fn, value as u64)
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Value::NIL
    }
}

#[cfg(test)]
mod tests {
    use crate::gc::Gc;

    use super::*;

    #[test]
    fn test_tags() {
        assert_eq!(Value::new_primitive(Tag::Int).ty(), Type::Int);
        assert_eq!(Value::new_primitive(Tag::Array).ty(), Type::Array);
        assert_eq!(Value::new_primitive(Tag::Fn).ty(), Type::Fn);
        assert_eq!(Value::new_primitive(Tag::Str).ty(), Type::Str);
        assert_eq!(Value::new_primitive(Tag::True).ty(), Type::Bool);
        assert_eq!(Value::new_primitive(Tag::False).ty(), Type::Bool);
        assert_eq!(Value::new_primitive(Tag::Nil).ty(), Type::Nil);
        assert_eq!(Value::new_primitive(Tag::Class).ty(), Type::Class);
        assert_eq!(Value::new_primitive(Tag::Instance).ty(), Type::Instance);
    }

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

        let str = Str::from_string(&mut gc, "hello".to_string());
        let handle = gc.alloc(str).unwrap();
        let value = Value::from(handle);

        assert_eq!(value.ty(), Type::Str);

        let str = gc.get(value.str());
        assert_eq!(str.as_str(), "hello");
    }
}
