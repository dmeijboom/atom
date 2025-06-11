use std::{
    fmt::{Debug, Display},
    marker::PhantomData,
};

use crate::gc::{Gc, Handle, Trace};

use super::{
    array::Array,
    class::{Class, Object},
    error::RuntimeError,
    function::{Fn, Method},
    str::Str,
};

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tag {
    SmallInt,
    Int,
    Float,
    Array,
    Fn,
    Method,
    Str,
    True,
    False,
    Nil,
    Class,
    Object,
}

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Array,
    Fn,
    Method,
    Str,
    Nil,
    Class,
    Object,
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Int => "Int",
            Type::Float => "Float",
            Type::Bool => "Bool",
            Type::Array => "Array",
            Type::Fn => "Fn",
            Type::Method => "Method",
            Type::Str => "Str",
            Type::Nil => "Nil",
            Type::Class => "Class",
            Type::Object => "Object",
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

const SIGN_BIT: u64 = 1 << 63;
const SIG_NAN: u64 = 0x7ff0_0000_0000_0000;
const TAG_MASK: u64 = 0b1111 << 48;
pub const INT_MASK: u64 = 0xffff_ffff_ffff;

#[derive(Clone, Copy)]
pub struct Value<'gc> {
    bits: u64,
    _phantom: PhantomData<&'gc ()>,
}

impl<'gc> Value<'gc> {
    pub const FALSE: Self = Self::new_primitive(Tag::False);
    pub const TRUE: Self = Self::new_primitive(Tag::True);
    const NAN: Self = Self::new_primitive(Tag::Float);
    pub const NIL: Self = Self::new_primitive(Tag::Nil);

    pub const fn is_float(&self) -> bool {
        self.bits == Self::NAN.bits || (self.bits & SIG_NAN) != SIG_NAN
    }

    pub const fn is_int(&self) -> bool {
        let tag = (self.bits & TAG_MASK) >> 48;
        tag == Tag::SmallInt as u64 || tag == Tag::Int as u64
    }

    pub const fn is_object(&self) -> bool {
        (self.bits & TAG_MASK) >> 48 == Tag::Object as u64
    }

    pub const fn tag(&self) -> Tag {
        if self.is_float() {
            return Tag::Float;
        }

        match (self.bits & TAG_MASK) >> 48 {
            t if t == Tag::SmallInt as u64 => Tag::SmallInt,
            t if t == Tag::Int as u64 => Tag::Int,
            t if t == Tag::Float as u64 => Tag::Float,
            t if t == Tag::True as u64 => Tag::True,
            t if t == Tag::False as u64 => Tag::False,
            t if t == Tag::Array as u64 => Tag::Array,
            t if t == Tag::Fn as u64 => Tag::Fn,
            t if t == Tag::Method as u64 => Tag::Method,
            t if t == Tag::Str as u64 => Tag::Str,
            t if t == Tag::Nil as u64 => Tag::Nil,
            t if t == Tag::Class as u64 => Tag::Class,
            t if t == Tag::Object as u64 => Tag::Object,
            _ => unreachable!(),
        }
    }

    pub const fn ty(&self) -> Type {
        match self.tag() {
            Tag::SmallInt | Tag::Int => Type::Int,
            Tag::Float => Type::Float,
            Tag::Array => Type::Array,
            Tag::Fn => Type::Fn,
            Tag::Method => Type::Method,
            Tag::Str => Type::Str,
            Tag::True | Tag::False => Type::Bool,
            Tag::Nil => Type::Nil,
            Tag::Class => Type::Class,
            Tag::Object => Type::Object,
        }
    }

    const fn new_primitive(tag: Tag) -> Self {
        Self {
            bits: (tag as u64) << 48 | SIG_NAN,
            _phantom: PhantomData,
        }
    }

    const fn new(tag: Tag, value: u64) -> Self {
        Self {
            bits: (tag as u64) << 48 | SIG_NAN | value,
            _phantom: PhantomData,
        }
    }

    pub const fn new_smallint(value: i64) -> Self {
        Self::new(
            Tag::SmallInt,
            if value < 0 {
                SIGN_BIT | value.unsigned_abs()
            } else {
                value as u64
            },
        )
    }

    pub fn int(self) -> i64 {
        match self.tag() {
            Tag::SmallInt => {
                let bits = self.bits & INT_MASK;

                if self.bits & SIGN_BIT != 0 {
                    return -(bits as i64);
                }

                bits as i64
            }
            Tag::Int => *self.into_handle(),
            _ => unreachable!(),
        }
    }

    pub fn float(self) -> f64 {
        f64::from_bits(self.bits)
    }

    pub fn bool(self) -> bool {
        self == Value::TRUE
    }

    pub fn class(self) -> Handle<'gc, Class<'gc>> {
        self.into_handle()
    }

    pub fn func(self) -> Handle<'gc, Fn> {
        self.into_handle()
    }

    pub fn method(self) -> Handle<'gc, Method<'gc>> {
        self.into_handle()
    }

    fn into_handle<T: Trace>(self) -> Handle<'gc, T> {
        let addr = self.bits & INT_MASK;
        Handle::from_addr(addr as usize).unwrap()
    }

    pub fn object(self) -> Handle<'gc, Object<'gc>> {
        self.into_handle()
    }

    pub fn str(self) -> Handle<'gc, Str<'gc>> {
        self.into_handle()
    }

    pub fn array(self) -> Handle<'gc, Array<'gc, Value<'gc>>> {
        self.into_handle()
    }
}

impl<'gc> Trace for Value<'gc> {
    fn trace(&self, gc: &mut Gc) {
        match self.tag() {
            Tag::Array => gc.mark(&self.array()),
            Tag::Str => gc.mark(&self.str()),
            Tag::Int => gc.mark(&self.into_handle::<i64>()),
            Tag::Object => gc.mark(&self.object()),
            Tag::Fn => gc.mark(&self.func()),
            Tag::Method => gc.mark(&self.method()),
            Tag::Class => gc.mark(&self.class()),
            Tag::SmallInt | Tag::Float | Tag::True | Tag::False | Tag::Nil => {}
        }
    }
}

impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.bits == other.bits
    }
}

impl<'gc> Default for Value<'gc> {
    fn default() -> Self {
        Self::NIL
    }
}

impl<'gc> Display for Value<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty() {
            Type::Int => write!(f, "{}", self.int()),
            Type::Float => write!(f, "{}", self.float()),
            Type::Bool => write!(f, "{}", self.bool()),
            Type::Array => write!(
                f,
                "[{}]",
                self.array()
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ty => write!(f, "{ty}"),
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("integer overflow")]
pub struct IntOverflowError;

impl<'gc> TryFrom<i64> for Value<'gc> {
    type Error = IntOverflowError;

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        if value.unsigned_abs() > INT_MASK {
            return Err(IntOverflowError);
        }

        Ok(Self::new_smallint(value))
    }
}

impl<'gc> From<f64> for Value<'gc> {
    fn from(value: f64) -> Self {
        Self {
            bits: value.to_bits(),
            _phantom: PhantomData,
        }
    }
}

impl<'gc> From<bool> for Value<'gc> {
    fn from(value: bool) -> Self {
        if value {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }
}

impl<'gc> TryFrom<usize> for Value<'gc> {
    type Error = IntOverflowError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        (value as i64).try_into()
    }
}

impl<'gc> From<Handle<'gc, Object<'gc>>> for Value<'gc> {
    fn from(object: Handle<'gc, Object>) -> Self {
        Self::new(Tag::Object, object.addr() as u64)
    }
}

impl<'gc> From<Handle<'gc, Class<'gc>>> for Value<'gc> {
    fn from(handle: Handle<'gc, Class<'gc>>) -> Self {
        Self::new(Tag::Class, handle.addr() as u64)
    }
}

impl<'gc> From<Handle<'gc, i64>> for Value<'gc> {
    fn from(handle: Handle<'gc, i64>) -> Self {
        Self::new(Tag::Int, handle.addr() as u64)
    }
}

impl<'gc> From<Handle<'gc, Str<'gc>>> for Value<'gc> {
    fn from(handle: Handle<'gc, Str<'gc>>) -> Self {
        Self::new(Tag::Str, handle.addr() as u64)
    }
}

impl<'gc> From<Handle<'gc, Array<'gc, Value<'gc>>>> for Value<'gc> {
    fn from(handle: Handle<'gc, Array<'gc, Value<'gc>>>) -> Self {
        Self::new(Tag::Array, handle.addr() as u64)
    }
}

impl<'gc> From<Handle<'gc, Fn>> for Value<'gc> {
    fn from(handle: Handle<'gc, Fn>) -> Self {
        Self::new(Tag::Fn, handle.addr() as u64)
    }
}

impl<'gc> From<Handle<'gc, Method<'gc>>> for Value<'gc> {
    fn from(handle: Handle<'gc, Method<'gc>>) -> Self {
        Self::new(Tag::Method, handle.addr() as u64)
    }
}

impl<'gc> From<()> for Value<'gc> {
    fn from(_: ()) -> Self {
        Value::NIL
    }
}

impl<'gc> From<Value<'gc>> for Handle<'gc, Str<'gc>> {
    fn from(value: Value<'gc>) -> Self {
        value.str()
    }
}

impl<'gc> From<Value<'gc>> for Handle<'gc, Object<'gc>> {
    fn from(value: Value<'gc>) -> Self {
        value.object()
    }
}

impl<'gc> From<Value<'gc>> for Handle<'gc, Array<'gc, Value<'gc>>> {
    fn from(value: Value<'gc>) -> Self {
        value.array()
    }
}

pub trait TryIntoValue<'gc> {
    fn try_into_val(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError>;
}

impl<'gc, T: TryIntoValue<'gc>> TryIntoValue<'gc> for Vec<T> {
    fn try_into_val(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        let data = self
            .into_iter()
            .map(|value| value.try_into_val(gc))
            .collect::<Result<Vec<_>, _>>()?;
        let array = Array::from_vec(gc, data);
        let handle = gc.alloc(array)?;

        Ok(handle.into())
    }
}

impl<'gc, T: Into<Value<'gc>>> TryIntoValue<'gc> for T {
    fn try_into_val(self, _gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        Ok(self.into())
    }
}

impl<'gc> TryIntoValue<'gc> for String {
    fn try_into_val(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        let bytes = self.into_bytes();
        let array = Array::from_vec(gc, bytes);
        Ok(gc.alloc(Str(array))?.into())
    }
}

impl<'gc> TryIntoValue<'gc> for usize {
    fn try_into_val(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        (self as i64).try_into_val(gc)
    }
}

impl<'gc> TryIntoValue<'gc> for i64 {
    fn try_into_val(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        match Value::try_from(self) {
            Ok(value) => Ok(value),
            Err(IntOverflowError) => {
                let handle = gc.alloc(self)?;
                Ok(handle.into())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::gc::Gc;

    use super::*;

    #[test]
    fn test_tags() {
        assert_eq!(Value::new_primitive(Tag::SmallInt).ty(), Type::Int);
        assert_eq!(Value::new_primitive(Tag::Int).ty(), Type::Int);
        assert_eq!(Value::new_primitive(Tag::Array).ty(), Type::Array);
        assert_eq!(Value::new_primitive(Tag::Fn).ty(), Type::Fn);
        assert_eq!(Value::new_primitive(Tag::Str).ty(), Type::Str);
        assert_eq!(Value::new_primitive(Tag::True).ty(), Type::Bool);
        assert_eq!(Value::new_primitive(Tag::False).ty(), Type::Bool);
        assert_eq!(Value::new_primitive(Tag::Nil).ty(), Type::Nil);
        assert_eq!(Value::new_primitive(Tag::Class).ty(), Type::Class);
        assert_eq!(Value::new_primitive(Tag::Object).ty(), Type::Object);
    }

    #[test]
    fn test_small_int() {
        for i in -1000000..1000000 {
            let value = Value::try_from(i).unwrap();
            assert_eq!(value.int(), i);
        }
    }

    #[test]
    fn test_big_int() {
        let mut gc = Gc::default();

        for i in i64::MAX - 1000..i64::MAX {
            let value = i.try_into_val(&mut gc).unwrap();
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

        let str = value.str();
        assert_eq!(str.as_str(), "hello");
    }
}
