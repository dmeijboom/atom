use std::{
    fmt::Display,
    ops::{Add, BitAnd, BitOr, BitXor, Deref, Div, Mul, Rem, Shl, Shr, Sub},
};

use rug::{Complete, Integer};

use crate::gc::{Gc, Handle, Trace};

use super::value::{self, IntoAtom, Value};

pub enum RawInt {
    Inline(i64),
    Big(Integer),
}

impl<'gc> IntoAtom<'gc> for RawInt {
    fn into_atom(
        self,
        gc: &mut Gc<'gc>,
    ) -> Result<super::value::Value<'gc>, super::error::RuntimeError> {
        match self {
            RawInt::Inline(i) => {
                if i.unsigned_abs() > value::INT_MASK {
                    let handle = gc.alloc(Integer::from(i))?;
                    return Ok(handle.into());
                }

                Ok(Value::new_smallint(i))
            }
            RawInt::Big(integer) => {
                let handle = gc.alloc(integer)?;
                Ok(handle.into())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Int<'gc> {
    Inline(i64),
    Big(Handle<'gc, Integer>),
}

impl<'gc> Int<'gc> {
    pub fn to_usize(&self) -> usize {
        match self {
            Int::Inline(i) => *i as usize,
            Int::Big(_) => todo!(),
        }
    }

    pub fn to_i64(&self) -> i64 {
        match self {
            Int::Inline(i) => *i,
            Int::Big(i) => i.to_i64_wrapping(),
        }
    }
}

impl Trace for Int<'_> {
    fn trace(&self, gc: &mut Gc) {
        match self {
            Int::Inline(_) => {}
            Int::Big(big) => gc.mark(big),
        }
    }
}

impl Trace for Integer {
    fn trace(&self, _gc: &mut Gc<'_>) {}
}

impl<'gc> PartialEq for Int<'gc> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Int::Inline(lhs), Int::Inline(rhs)) => lhs == rhs,
            (Int::Big(lhs), Int::Big(rhs)) => lhs.deref() == rhs.deref(),
            (Int::Inline(lhs), Int::Big(rhs)) => lhs == rhs.deref(),
            (Int::Big(lhs), Int::Inline(rhs)) => lhs.deref() == rhs,
        }
    }
}

impl<'gc> PartialOrd for Int<'gc> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Int::Inline(lhs), Int::Inline(rhs)) => lhs.partial_cmp(rhs),
            (Int::Big(lhs), Int::Big(rhs)) => lhs.deref().partial_cmp(rhs.deref()),
            (Int::Inline(lhs), Int::Big(rhs)) => lhs.partial_cmp(rhs.deref()),
            (Int::Big(lhs), Int::Inline(rhs)) => lhs.deref().partial_cmp(rhs),
        }
    }
}

impl<'gc> Display for Int<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Int::Inline(value) => write!(f, "{value}"),
            Int::Big(big) => write!(f, "{}", big.deref()),
        }
    }
}

macro_rules! impl_from {
    ($($name:ident $fn:ident $op:tt),+) => {
        $(impl $name for Int<'_> {
            type Output = RawInt;

            fn $fn(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (Int::Inline(lhs), Int::Inline(rhs)) => RawInt::Inline(lhs $op rhs),
                    (Int::Big(lhs), Int::Big(rhs)) => RawInt::Big((lhs.deref() $op rhs.deref()).complete()),
                    (Int::Inline(lhs), Int::Big(rhs)) => RawInt::Big((lhs $op rhs.deref()).complete()),
                    (Int::Big(lhs), Int::Inline(rhs)) => RawInt::Big((lhs.deref() $op rhs).complete()),
                }
            }
        })+
    };
}

impl_from!(
    Add add +,
    Sub sub -,
    Mul mul *,
    Div div /,
    Rem rem %,
    BitAnd bitand &,
    BitOr bitor |,
    BitXor bitxor ^
);

impl Shl for Int<'_> {
    type Output = RawInt;

    fn shl(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Inline(lhs), Int::Inline(rhs)) => RawInt::Inline(lhs << rhs),
            (Int::Big(lhs), Int::Big(rhs)) => {
                RawInt::Big((lhs.deref() << rhs.to_usize_wrapping()).complete())
            }
            (Int::Inline(lhs), Int::Big(rhs)) => {
                RawInt::Big((&Integer::from(lhs) << rhs.to_usize_wrapping()).complete())
            }
            (Int::Big(lhs), Int::Inline(rhs)) => {
                RawInt::Big((lhs.deref() << rhs as usize).complete())
            }
        }
    }
}

impl Shr for Int<'_> {
    type Output = RawInt;

    fn shr(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Inline(lhs), Int::Inline(rhs)) => RawInt::Inline(lhs >> rhs),
            (Int::Big(lhs), Int::Big(rhs)) => {
                RawInt::Big((lhs.deref() >> rhs.to_usize_wrapping()).complete())
            }
            (Int::Inline(lhs), Int::Big(rhs)) => {
                RawInt::Big((&Integer::from(lhs) >> rhs.to_usize_wrapping()).complete())
            }
            (Int::Big(lhs), Int::Inline(rhs)) => {
                RawInt::Big((lhs.deref() >> rhs as usize).complete())
            }
        }
    }
}

// @TODO: Refactor to handle overflow
impl From<usize> for Int<'_> {
    fn from(value: usize) -> Self {
        Int::Inline(value as i64)
    }
}

impl From<i64> for Int<'_> {
    fn from(value: i64) -> Self {
        Int::Inline(value)
    }
}
