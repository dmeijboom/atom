use std::{
    fmt::{Debug, Display},
    marker::PhantomData,
    ops::Deref,
};

use num_enum::{FromPrimitive, IntoPrimitive};

use crate::gc::{Gc, Handle, Trace};

use super::{
    array::Array,
    bigint::BigInt,
    class::{Class, Object},
    error::RuntimeError,
    function::{Fn, Method},
    str::Str,
};

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromPrimitive, IntoPrimitive)]
pub enum Tag {
    Int,
    BigInt,
    Float,
    Array,
    Fn,
    Method,
    Str,
    #[default]
    Atom,
    Class,
    Object,
}

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, FromPrimitive, IntoPrimitive)]
pub enum Type {
    Int,
    BigInt,
    Float,
    Array,
    Fn,
    Method,
    Str,
    #[default]
    Atom,
    Class,
    Object,
}

impl Type {
    pub fn name(&self) -> &str {
        match self {
            Type::Int => "Int",
            Type::BigInt => "BigInt",
            Type::Float => "Float",
            Type::Array => "Array",
            Type::Fn => "Fn",
            Type::Method => "Method",
            Type::Str => "Str",
            Type::Atom => "Atom",
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

// In theory, we can store any handle in a value, but we only allow a limited set of types
trait ValueHandle {
    fn tag() -> Tag;
}

impl ValueHandle for Str<'_> {
    fn tag() -> Tag {
        Tag::Str
    }
}

impl ValueHandle for Class<'_> {
    fn tag() -> Tag {
        Tag::Class
    }
}

impl ValueHandle for Object<'_> {
    fn tag() -> Tag {
        Tag::Object
    }
}

impl ValueHandle for Fn {
    fn tag() -> Tag {
        Tag::Fn
    }
}

impl ValueHandle for BigInt {
    fn tag() -> Tag {
        Tag::BigInt
    }
}

impl ValueHandle for Method<'_> {
    fn tag() -> Tag {
        Tag::Method
    }
}

impl ValueHandle for Array<'_, Value<'_>> {
    fn tag() -> Tag {
        Tag::Array
    }
}

pub const SIGN_BIT: u64 = 1 << 63;
pub const INT_MASK: u64 = 0xffff_ffff_ffff;
pub const FALSE: u32 = ((Tag::Atom as u64) << 48 | SIG_NAN) as u32;
pub const TRUE: u32 = ((Tag::Atom as u64) << 48 | SIG_NAN | 1) as u32;
pub const NIL: u32 = ((Tag::Atom as u64) << 48 | SIG_NAN | 2) as u32;
pub const INSTANCE: u32 = ((Tag::Atom as u64) << 48 | SIG_NAN | 3) as u32;

const SIG_NAN: u64 = 0x7ff0_0000_0000_0000;
const TAG_MASK: u64 = 0b1111 << 48;
const NAN: u64 = (Tag::Float as u64) << 48 | SIG_NAN;

#[derive(Clone)]
pub struct Value<'gc> {
    bits: u64,
    _phantom: PhantomData<&'gc ()>,
}

impl<'gc> Value<'gc> {
    #[inline(always)]
    pub const fn new(tag: Tag, value: u64) -> Self {
        Self {
            bits: (tag as u64) << 48 | SIG_NAN | value,
            _phantom: PhantomData,
        }
    }

    pub fn into_bits(self) -> u64 {
        self.bits
    }

    #[inline]
    pub fn new_int(i: i64) -> Self {
        Self::new(
            Tag::Int,
            if i < 0 {
                SIGN_BIT | i.unsigned_abs()
            } else {
                i as u64
            },
        )
    }

    pub fn from_bits(bits: u64) -> Self {
        Self {
            bits,
            _phantom: PhantomData,
        }
    }

    #[inline(always)]
    fn from_ptr(tag: Tag, ptr: *mut u8) -> Self {
        Self::new(tag, ptr as u64)
    }

    pub const fn is_float(&self) -> bool {
        self.bits == NAN || (self.bits & SIG_NAN) != SIG_NAN
    }

    pub fn is_object(&self) -> bool {
        ((self.bits & TAG_MASK) >> 48) == Tag::Object as u64
    }

    pub fn is_int(&self) -> bool {
        ((self.bits & TAG_MASK) >> 48) == Tag::Int as u64
    }

    pub fn tag(&self) -> Tag {
        if self.is_float() {
            return Tag::Float;
        }

        ((self.bits & TAG_MASK) >> 48).into()
    }

    pub fn ty(&self) -> Type {
        match self.tag() {
            Tag::Int => Type::Int,
            Tag::BigInt => Type::BigInt,
            Tag::Float => Type::Float,
            Tag::Array => Type::Array,
            Tag::Fn => Type::Fn,
            Tag::Method => Type::Method,
            Tag::Str => Type::Str,
            Tag::Atom => Type::Atom,
            Tag::Class => Type::Class,
            Tag::Object => Type::Object,
        }
    }

    pub const fn as_int(&self) -> i64 {
        let bits = self.bits & INT_MASK;

        if self.bits & SIGN_BIT != 0 {
            return -(bits as i64);
        }

        bits as i64
    }

    pub fn as_bigint(&self) -> BigInt {
        match self.tag() {
            Tag::Int => BigInt::from(self.as_int()),
            Tag::BigInt => self.as_handle::<BigInt>().deref().clone(),
            _ => unreachable!(),
        }
    }

    #[inline]
    pub fn as_float(&self) -> f64 {
        f64::from_bits(self.bits)
    }

    pub fn as_ptr<T>(&self) -> *mut T {
        (self.bits & INT_MASK) as _
    }

    pub fn as_raw_ptr(&self) -> *mut u8 {
        (self.bits & INT_MASK) as _
    }

    #[inline]
    fn as_handle<T: Trace + ValueHandle>(&self) -> Handle<'gc, T> {
        Handle::from_ptr(self.as_ptr::<T>())
    }

    #[inline]
    pub fn as_class(&self) -> Handle<'gc, Class<'gc>> {
        self.as_handle()
    }

    #[inline]
    pub fn as_fn(&self) -> Handle<'gc, Fn> {
        self.as_handle()
    }

    #[inline]
    pub fn as_method(&self) -> Handle<'gc, Method<'gc>> {
        self.as_handle()
    }

    #[inline]
    pub fn as_object(&self) -> Handle<'gc, Object<'gc>> {
        self.as_handle()
    }

    #[inline]
    pub fn as_str(&self) -> Handle<'gc, Str<'gc>> {
        self.as_handle()
    }

    #[inline]
    pub fn as_array(&self) -> Handle<'gc, Array<'gc, Value<'gc>>> {
        self.as_handle()
    }

    #[inline]
    pub fn as_atom(&self) -> u32 {
        (self.bits & INT_MASK) as u32
    }
}

impl<'gc> Trace for Value<'gc> {
    fn trace(&self, gc: &mut Gc) {
        match self.tag() {
            Tag::Array => gc.mark(&self.as_array()),
            Tag::Str => gc.mark(&self.as_str()),
            Tag::Object => gc.mark(&self.as_object()),
            Tag::Fn => gc.mark(&self.as_fn()),
            Tag::Method => gc.mark(&self.as_method()),
            Tag::Class => gc.mark(&self.as_class()),
            Tag::Int | Tag::BigInt | Tag::Float | Tag::Atom => {}
        }
    }
}

impl<'gc> PartialEq for Value<'gc> {
    fn eq(&self, other: &Self) -> bool {
        if self.bits == other.bits {
            return true;
        }

        match (self.tag(), other.tag()) {
            (Tag::BigInt, Tag::BigInt) => self.as_bigint() == other.as_bigint(),
            (Tag::Str, Tag::Str) => self.as_str().as_str() == other.as_str().as_str(),
            _ => false,
        }
    }
}

impl<'gc> Default for Value<'gc> {
    fn default() -> Self {
        Value::new(Tag::Atom, NIL as u64)
    }
}

impl<'gc> Display for Value<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty() {
            Type::Int => write!(f, "{}", self.as_bigint()),
            Type::Float => write!(f, "{}", self.as_float()),
            Type::Array => write!(
                f,
                "[{}]",
                self.as_array()
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ty => write!(f, "{ty}"),
        }
    }
}

impl<'gc> From<Value<'gc>> for i32 {
    fn from(value: Value<'gc>) -> Self {
        match value.tag() {
            Tag::Int => value.as_int() as i32,
            Tag::BigInt => value.as_bigint().as_i64() as i32,
            _ => unreachable!(),
        }
    }
}

impl<'gc> From<Value<'gc>> for i64 {
    fn from(value: Value<'gc>) -> Self {
        match value.tag() {
            Tag::Int => value.as_int(),
            Tag::BigInt => value.as_bigint().as_i64(),
            _ => unreachable!(),
        }
    }
}

impl<'gc> From<Value<'gc>> for usize {
    fn from(value: Value<'gc>) -> Self {
        match value.tag() {
            Tag::Int => value.as_int() as usize,
            Tag::BigInt => value.as_bigint().as_usize(),
            _ => unreachable!(),
        }
    }
}

impl From<f64> for Value<'_> {
    fn from(value: f64) -> Self {
        Self {
            bits: value.to_bits(),
            _phantom: PhantomData,
        }
    }
}

macro_rules! impl_from_small_int {
    ($($ty:ty),+) => {
        $(impl From<$ty> for Value<'_> {
            fn from(value: $ty) -> Self {
                Value::new_int(value as i64)
            }
        })+
    };
}

impl_from_small_int!(i8, i16, i32, u8, u16, u32);

impl From<bool> for Value<'_> {
    fn from(value: bool) -> Self {
        if value { TRUE } else { FALSE }.into()
    }
}

impl<'gc, T: Trace + ValueHandle> From<Handle<'gc, T>> for Value<'gc> {
    fn from(handle: Handle<'gc, T>) -> Self {
        Value::from_ptr(T::tag(), handle.as_ptr().cast())
    }
}

pub trait IntoAtom<'gc> {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError>;
}

impl<'gc, T> IntoAtom<'gc> for T
where
    Value<'gc>: From<T>,
{
    #[inline]
    fn into_atom(self, _gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        Ok(self.into())
    }
}

impl<'gc> IntoAtom<'gc> for BigInt {
    #[inline]
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        if self.is_small() {
            let (signed, abs) = self.as_unsigned_abs();

            if abs <= INT_MASK {
                return Ok(Value::new(
                    Tag::Int,
                    if signed { SIGN_BIT | abs } else { abs },
                ));
            }
        }

        gc.alloc(self)?.into_atom(gc)
    }
}

impl<'gc> IntoAtom<'gc> for () {
    fn into_atom(self, _gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        Ok(Value::default())
    }
}

impl<'gc> IntoAtom<'gc> for i64 {
    #[inline]
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        if self.unsigned_abs() <= INT_MASK {
            return Ok(Value::new_int(self));
        }

        BigInt::from(self).into_atom(gc)
    }
}

impl<'gc> IntoAtom<'gc> for usize {
    #[inline]
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        BigInt::from(self).into_atom(gc)
    }
}

impl<'gc> IntoAtom<'gc> for String {
    #[inline]
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        let str = Str::copy_from_str(gc, &self)?;
        let handle = gc.alloc(str)?;

        Ok(handle.into())
    }
}

impl<'gc, T: IntoAtom<'gc>> IntoAtom<'gc> for Vec<T> {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        let data = self
            .into_iter()
            .map(|value| value.into_atom(gc))
            .collect::<Result<Vec<_>, _>>()?;
        let array = Array::copy_from_slice(gc, &data)?;
        let handle = gc.alloc(array)?;

        Ok(handle.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::gc::Gc;

    use super::*;

    #[test]
    fn test_tags() {
        assert_eq!(Value::new(Tag::Int, 0).ty(), Type::Int);
        assert_eq!(Value::new(Tag::Array, 0).ty(), Type::Array);
        assert_eq!(Value::new(Tag::Fn, 0).ty(), Type::Fn);
        assert_eq!(Value::new(Tag::Str, 0).ty(), Type::Str);
        assert_eq!(Value::new(Tag::Atom, 0).ty(), Type::Atom);
        assert_eq!(Value::new(Tag::Class, 0).ty(), Type::Class);
        assert_eq!(Value::new(Tag::Object, 0).ty(), Type::Object);
    }

    #[test]
    #[cfg_attr(miri, ignore)] // Works on miri, but takes too long
    fn test_small_int() {
        let mut gc = Gc::default();

        for i in -1000000..1000000 {
            let value = i.into_atom(&mut gc).unwrap();
            assert_eq!(value.as_bigint().as_i64(), i);
        }
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_big_int() {
        let mut gc = Gc::default();

        for i in i64::MAX - 1000..i64::MAX {
            let value = i.into_atom(&mut gc).unwrap();
            assert_eq!(value.as_bigint().as_i64(), i);
        }
    }

    #[test]
    fn test_float() {
        let mut i = -10000.0;

        while i < 10000.0 {
            let value = Value::from(i);
            assert_eq!(value.as_float(), i);

            i += 0.1;
        }
    }

    #[test]
    fn test_bool() {
        let value = Value::from(true);
        assert_eq!(value.as_atom(), TRUE);

        let value = Value::from(false);
        assert_eq!(value.as_atom(), FALSE);
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_string() {
        let mut gc = Gc::default();

        let str = Str::copy_from_str(&mut gc, "hello").unwrap();
        let handle = gc.alloc(str).unwrap();
        let value = Value::from(handle);

        assert_eq!(value.ty(), Type::Str);

        let str = value.as_str();
        assert_eq!(str.as_str(), "hello");
    }
}
