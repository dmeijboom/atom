use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

use super::error::Result;
use super::value::{Convert, Value};

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

fn upper_bound(lhs: Int, rhs: Int) -> (bool, u8) {
    let lsize = lhs.size();
    let rsize = rhs.size();

    if lsize >= rsize {
        (lhs.signed(), lsize)
    } else {
        (rhs.signed(), rsize)
    }
}

macro_rules! call_upper_bound {
    ($left:expr, borrow: $right:expr, $function:ident) => {{
        let (signed, size) = upper_bound($left, $right);

        match size {
            8 if !signed => cast!($left, u8).$function(&cast!($right, u8)).into(),
            8 if signed => cast!($left, i8).$function(&cast!($right, i8)).into(),
            16 if !signed => cast!($left, u16).$function(&cast!($right, u16)).into(),
            16 if signed => cast!($left, i16).$function(&cast!($right, i16)).into(),
            32 if !signed => cast!($left, u32).$function(&cast!($right, u32)).into(),
            32 if signed => cast!($left, i32).$function(&cast!($right, i32)).into(),
            64 if !signed => cast!($left, u64).$function(&cast!($right, u64)).into(),
            64 if signed => cast!($left, i64).$function(&cast!($right, i64)).into(),
            128 if !signed => cast!($left, u128).$function(&cast!($right, u128)).into(),
            128 if signed => cast!($left, i128).$function(&cast!($right, i128)).into(),
            _ => unreachable!(),
        }
    }};

    ($left:expr, $right:expr, $function:ident) => {{
        let (signed, size) = upper_bound($left, $right);

        match size {
            8 if !signed => cast!($left, u8).$function(cast!($right, u8)).into(),
            8 if signed => cast!($left, i8).$function(cast!($right, i8)).into(),
            16 if !signed => cast!($left, u16).$function(cast!($right, u16)).into(),
            16 if signed => cast!($left, i16).$function(cast!($right, i16)).into(),
            32 if !signed => cast!($left, u32).$function(cast!($right, u32)).into(),
            32 if signed => cast!($left, i32).$function(cast!($right, i32)).into(),
            64 if !signed => cast!($left, u64).$function(cast!($right, u64)).into(),
            64 if signed => cast!($left, i64).$function(cast!($right, i64)).into(),
            128 if !signed => cast!($left, u128).$function(cast!($right, u128)).into(),
            128 if signed => cast!($left, i128).$function(cast!($right, i128)).into(),
            _ => unreachable!(),
        }
    }};
}

macro_rules! impl_op {
    ($($function:ident, $opname:ident) +) => {
        $(impl $function for Int {
            type Output = Int;

            fn $opname(self, rhs: Self) -> Self::Output {
                call_upper_bound!(self, rhs, $opname)
            }
        }) +
    };
}

macro_rules! impl_conv {
    ($function:ident, $rust_type:ident) => {
        impl From<$rust_type> for Int {
            fn from(val: $rust_type) -> Self {
                Self::$function(val)
            }
        }

        impl From<Int> for $rust_type {
            fn from(int: Int) -> Self {
                cast!(int, $rust_type)
            }
        }
    };
}

#[derive(Debug, Eq, Clone, Copy)]
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
    pub const fn to_float(self) -> f64 {
        cast!(self, f64)
    }

    pub const fn to_byte(self) -> u8 {
        cast!(self, u8)
    }

    pub const fn signed(&self) -> bool {
        matches!(
            self,
            Int::Int8(_) | Int::Int16(_) | Int::Int32(_) | Int::Int64(_) | Int::Int128(_)
        )
    }

    pub const fn size(&self) -> u8 {
        match self {
            Int::Int8(_) | Int::Uint8(_) => 8,
            Int::Int16(_) | Int::Uint16(_) => 16,
            Int::Int32(_) | Int::Uint32(_) => 32,
            Int::Int64(_) | Int::Uint64(_) => 64,
            Int::Int128(_) | Int::Uint128(_) => 128,
        }
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

impl PartialEq for Int {
    fn eq(&self, other: &Self) -> bool {
        call_upper_bound!(*self, borrow: *other, eq)
    }
}

impl PartialOrd for Int {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        call_upper_bound!(*self, borrow: *other, partial_cmp)
    }
}

impl From<usize> for Int {
    fn from(size: usize) -> Self {
        Int::Uint64(size as u64)
    }
}

impl_conv!(Int128, i128);
impl_conv!(Uint128, u128);
impl_conv!(Int64, i64);
impl_conv!(Uint64, u64);
impl_conv!(Int32, i32);
impl_conv!(Uint32, u32);
impl_conv!(Int16, i16);
impl_conv!(Uint16, u16);
impl_conv!(Int8, i8);
impl_conv!(Uint8, u8);

impl_op!(
    Add, add
    Sub, sub
    Mul, mul
    Div, div
    BitAnd, bitand
    BitOr, bitor
    BitXor, bitxor
    Shl, shl
    Shr, shr
    Rem, rem
);

impl Convert<usize> for Value {
    fn convert(self) -> Result<usize> {
        let int: Int = self.convert()?;

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
