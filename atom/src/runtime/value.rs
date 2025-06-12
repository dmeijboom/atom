use std::{
    fmt::{Debug, Display},
    marker::PhantomData,
};

use num_enum::{FromPrimitive, IntoPrimitive};

use crate::gc::{Gc, Handle, Trace};

use super::{
    array::Array,
    class::{Class, Object},
    error::RuntimeError,
    function::{Fn, Method},
    str::Str,
};

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, FromPrimitive, IntoPrimitive)]
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
    #[default]
    Nil,
    Class,
    Object,
}

#[repr(u64)]
#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq, FromPrimitive, IntoPrimitive)]
pub enum Type {
    Int,
    Float,
    Bool,
    Array,
    Fn,
    Method,
    Str,
    #[default]
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

    pub fn tag(&self) -> Tag {
        if self.is_float() {
            return Tag::Float;
        }

        ((self.bits & TAG_MASK) >> 48).into()
    }

    pub fn ty(&self) -> Type {
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

    pub const fn new_float(value: f64) -> Self {
        Self {
            bits: value.to_bits(),
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

    pub const fn new_bool(value: bool) -> Self {
        if value {
            Self::TRUE
        } else {
            Self::FALSE
        }
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

    fn into_handle<T: Trace>(self) -> Handle<'gc, T> {
        let addr = self.bits & INT_MASK;
        Handle::from_addr(addr as usize).unwrap()
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

macro_rules! impl_from {
    ($($fn:ident => $ty:ty),+) => {
        $(impl<'gc> From<Value<'gc>> for $ty {
            fn from(value: Value<'gc>) -> Self {
                value.$fn() as $ty
            }
        })+
    };
}

macro_rules! impl_from_handle {
    ($($tag:ident => $ty:ty),+) => {
        $(impl<'gc> From<Handle<'gc, $ty>> for Value<'gc> {
            fn from(handle: Handle<'gc, $ty>) -> Self {
                let addr = unsafe { handle.as_ptr() as u64 };
                Value::new(Tag::$tag, addr)
            }
        })+
    };
}

impl_from_handle!(
    Int => i64,
    Class => Class<'gc>,
    Fn => Fn,
    Method => Method<'gc>,
    Object => Object<'gc>,
    Str => Str<'gc>,
    Array => Array<'gc, Value<'gc>>
);

impl_from!(
    int => i8,
    int => i16,
    int => i32,
    int => i64,
    int => isize,
    int => u8,
    int => u16,
    int => u32,
    int => u64,
    int => usize,
    float => f32,
    float => f64,
    bool => bool,
    str => Handle<'gc, Str<'gc>>
);

impl<'gc> From<Value<'gc>> for () {
    fn from(_value: Value<'gc>) -> Self {}
}

impl<'gc> From<Value<'gc>> for Vec<Value<'gc>> {
    fn from(value: Value<'gc>) -> Self {
        value.array().iter().cloned().collect::<Vec<_>>()
    }
}

impl<'gc> From<Value<'gc>> for String {
    fn from(value: Value<'gc>) -> Self {
        value.str().to_string()
    }
}

#[derive(Debug, thiserror::Error)]
#[error("integer overflow")]
pub struct IntOverflowError;

macro_rules! impl_into_atom {
    ($($fn:ident: $lty:ty => $rty:ty),+) => {
        $(
            impl From<$rty> for Value<'_> {
                fn from(value: $rty) -> Self {
                    Value::$fn(value as $lty)
                }
            }

            impl<'gc> IntoAtom<'gc> for $rty {
                fn into_atom(self, _gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
                    Ok(self.into())
                }
            }
        )+
    };
}

impl_into_atom!(
    new_smallint: i64 => i8,
    new_smallint: i64 => i16,
    new_smallint: i64 => i32,
    new_smallint: i64 => u8,
    new_smallint: i64 => u16,
    new_smallint: i64 => u32,
    new_float: f64 => f32,
    new_float: f64 => f64,
    new_bool: bool => bool
);

pub trait IntoAtom<'gc> {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError>;
}

impl<'gc> IntoAtom<'gc> for i64 {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        if self.unsigned_abs() > INT_MASK {
            let handle = gc.alloc(self)?;
            return Ok(handle.into());
        }

        Ok(Value::new_smallint(self))
    }
}

impl<'gc> IntoAtom<'gc> for () {
    fn into_atom(self, _gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        Ok(Value::NIL)
    }
}

impl<'gc> IntoAtom<'gc> for isize {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        (self as i64).into_atom(gc)
    }
}

impl<'gc> IntoAtom<'gc> for usize {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        (self as i64).into_atom(gc)
    }
}

impl<'gc> IntoAtom<'gc> for String {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        let str = Str::from_string(gc, self);
        let handle = gc.alloc(str)?;

        Ok(handle.into())
    }
}

impl<'gc> IntoAtom<'gc> for Value<'gc> {
    fn into_atom(self, _gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        Ok(self)
    }
}

impl<'gc, T: IntoAtom<'gc>> IntoAtom<'gc> for Vec<T> {
    fn into_atom(self, gc: &mut Gc<'gc>) -> Result<Value<'gc>, RuntimeError> {
        let data = self
            .into_iter()
            .map(|value| value.into_atom(gc))
            .collect::<Result<Vec<_>, _>>()?;
        let array = Array::from_vec(gc, data);
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
        let mut gc = Gc::default();

        for i in -1000000..1000000 {
            let value = i.into_atom(&mut gc).unwrap();
            assert_eq!(value.int(), i);
        }
    }

    #[test]
    fn test_big_int() {
        let mut gc = Gc::default();

        for i in i64::MAX - 1000..i64::MAX {
            let value = i.into_atom(&mut gc).unwrap();
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
