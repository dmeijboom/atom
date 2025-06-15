use std::{
    fmt::Display,
    ops::{Add, BitAnd, BitOr, BitXor, Deref, Div, Mul, Rem, Shl, Shr, Sub},
};

use rug::Integer;

use crate::gc::{Gc, Handle, Trace};

#[derive(Clone, Debug)]
pub enum Int<'gc> {
    Small(i64),
    Big(Handle<'gc, Integer>),
}

impl<'gc> Int<'gc> {
    pub fn to_usize(&self) -> usize {
        match self {
            Int::Small(i) => *i as usize,
            Int::Big(_) => todo!(),
        }
    }

    pub fn to_i64(&self) -> i64 {
        match self {
            Int::Small(i) => *i,
            Int::Big(i) => i.to_i64_wrapping(),
        }
    }
}

impl Trace for Int<'_> {
    fn trace(&self, gc: &mut Gc) {
        match self {
            Int::Small(_) => {}
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
            (Int::Small(lhs), Int::Small(rhs)) => lhs == rhs,
            (Int::Big(lhs), Int::Big(rhs)) => lhs.deref() == rhs.deref(),
            (Int::Small(lhs), Int::Big(rhs)) => lhs == rhs.deref(),
            (Int::Big(lhs), Int::Small(rhs)) => lhs.deref() == rhs,
        }
    }
}

impl<'gc> PartialOrd for Int<'gc> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Int::Small(lhs), Int::Small(rhs)) => lhs.partial_cmp(rhs),
            (Int::Big(lhs), Int::Big(rhs)) => lhs.deref().partial_cmp(rhs.deref()),
            (Int::Small(lhs), Int::Big(rhs)) => lhs.partial_cmp(rhs.deref()),
            (Int::Big(lhs), Int::Small(rhs)) => lhs.deref().partial_cmp(rhs),
        }
    }
}

impl<'gc> Display for Int<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Int::Small(value) => write!(f, "{value}"),
            Int::Big(big) => write!(f, "{}", big.deref()),
        }
    }
}

impl<'gc> Shl for Int<'gc> {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs << rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> Shr for Int<'gc> {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs >> rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> BitOr for Int<'gc> {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs | rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> BitAnd for Int<'gc> {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs & rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> Rem for Int<'gc> {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs % rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> Div for Int<'gc> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs / rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> Mul for Int<'gc> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs * rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> Sub for Int<'gc> {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs - rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> Add for Int<'gc> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs + rhs),
            _ => unreachable!(),
        }
    }
}

impl<'gc> BitXor for Int<'gc> {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Int::Small(lhs), Int::Small(rhs)) => Int::Small(lhs ^ rhs),
            _ => unreachable!(),
        }
    }
}

// @TODO: Refactor to handle overflow
impl From<usize> for Int<'_> {
    fn from(value: usize) -> Self {
        Int::Small(value as i64)
    }
}

impl From<i64> for Int<'_> {
    fn from(value: i64) -> Self {
        Int::Small(value)
    }
}
