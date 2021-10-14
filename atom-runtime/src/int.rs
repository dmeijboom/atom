use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Shl, Shr, Sub};

use super::result::RuntimeError;
use super::value::Value;

macro_rules! cast {
    ($value:expr, $typename:ident) => {
        match $value {
            Int::Int128(val) => val as $typename,
            Int::Uint128(val) => val as $typename,
            Int::Int64(val) => val as $typename,
            Int::Uint64(val) => val as $typename,
            Int::Int32(val) => val as $typename,
            Int::Uint32(val) => val as $typename,
            Int::Int16(val) => val as $typename,
            Int::Uint16(val) => val as $typename,
            Int::Int8(val) => val as $typename,
            Int::Uint8(val) => val as $typename,
        }
    };
}

macro_rules! impl_op {
    ($name:ident, $opname:ident) => {
        impl $name for Int {
            type Output = Int;

            fn $opname(self, rhs: Self) -> Self::Output {
                match self {
                    Int::Int128(left) => (left.$opname(cast!(rhs, i128))).into(),
                    Int::Uint128(left) => (left.$opname(cast!(rhs, u128))).into(),
                    Int::Int64(left) => (left.$opname(cast!(rhs, i64))).into(),
                    Int::Uint64(left) => (left.$opname(cast!(rhs, u64))).into(),
                    Int::Int32(left) => (left.$opname(cast!(rhs, i32))).into(),
                    Int::Uint32(left) => (left.$opname(cast!(rhs, u32))).into(),
                    Int::Int16(left) => (left.$opname(cast!(rhs, i16))).into(),
                    Int::Uint16(left) => (left.$opname(cast!(rhs, u16))).into(),
                    Int::Int8(left) => (left.$opname(cast!(rhs, i8))).into(),
                    Int::Uint8(left) => (left.$opname(cast!(rhs, u8))).into(),
                }
            }
        }
    };
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Int {
    Int128(i128),
    Uint128(u128),
    Int64(i64),
    Uint64(u64),
    Int32(i32),
    Uint32(u32),
    Int16(i16),
    Uint16(u16),
    Int8(i8),
    Uint8(u8),
}

impl Int {
    pub fn to_float(&self) -> f64 {
        cast!(*self, f64)
    }

    pub fn to_byte(&self) -> u8 {
        cast!(*self, u8)
    }

    pub fn pow(&self, exp: Int) -> Self {
        match self {
            Int::Int128(val) => val.pow(cast!(exp, u32)).into(),
            Int::Uint128(val) => val.pow(cast!(exp, u32)).into(),
            Int::Int64(val) => val.pow(cast!(exp, u32)).into(),
            Int::Uint64(val) => val.pow(cast!(exp, u32)).into(),
            Int::Int32(val) => val.pow(cast!(exp, u32)).into(),
            Int::Uint32(val) => val.pow(cast!(exp, u32)).into(),
            Int::Int16(val) => val.pow(cast!(exp, u32)).into(),
            Int::Uint16(val) => val.pow(cast!(exp, u32)).into(),
            Int::Int8(val) => val.pow(cast!(exp, u32)).into(),
            Int::Uint8(val) => val.pow(cast!(exp, u32)).into(),
        }
    }
}

impl From<u8> for Int {
    fn from(val: u8) -> Self {
        Int::Uint8(val)
    }
}

impl From<i8> for Int {
    fn from(val: i8) -> Self {
        Int::Int8(val)
    }
}

impl From<u16> for Int {
    fn from(val: u16) -> Self {
        Int::Uint16(val)
    }
}

impl From<i16> for Int {
    fn from(val: i16) -> Self {
        Int::Int16(val)
    }
}

impl From<u32> for Int {
    fn from(val: u32) -> Self {
        Int::Uint32(val)
    }
}

impl From<i32> for Int {
    fn from(val: i32) -> Self {
        Int::Int32(val)
    }
}

impl From<u64> for Int {
    fn from(val: u64) -> Self {
        Int::Uint64(val)
    }
}

impl From<i64> for Int {
    fn from(val: i64) -> Self {
        Int::Int64(val)
    }
}

impl From<u128> for Int {
    fn from(val: u128) -> Self {
        Int::Uint128(val)
    }
}

impl From<i128> for Int {
    fn from(val: i128) -> Self {
        Int::Int128(val)
    }
}

impl From<usize> for Int {
    fn from(size: usize) -> Self {
        Int::Uint64(size as u64)
    }
}

impl_op!(Add, add);
impl_op!(Sub, sub);
impl_op!(Mul, mul);
impl_op!(Div, div);
impl_op!(BitAnd, bitand);
impl_op!(BitOr, bitor);
impl_op!(BitXor, bitxor);
impl_op!(Shl, shl);
impl_op!(Shr, shr);

impl TryFrom<Value> for usize {
    type Error = RuntimeError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        let int: Int = value.try_into()?;

        Ok(cast!(int, usize))
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Int::Int128(val) => val.fmt(f),
            Int::Uint128(val) => val.fmt(f),
            Int::Int64(val) => val.fmt(f),
            Int::Uint64(val) => val.fmt(f),
            Int::Int32(val) => val.fmt(f),
            Int::Uint32(val) => val.fmt(f),
            Int::Int16(val) => val.fmt(f),
            Int::Uint16(val) => val.fmt(f),
            Int::Int8(val) => val.fmt(f),
            Int::Uint8(val) => val.fmt(f),
        }
    }
}
