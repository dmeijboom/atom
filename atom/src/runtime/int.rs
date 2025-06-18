use std::{
    cmp::Ordering,
    ffi::{c_char, CStr, CString},
    fmt::Display,
    mem::MaybeUninit,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub},
    ptr::{self, NonNull},
    str::FromStr,
};

use gmp_mpfr_sys::gmp::{
    self, limb_t, mpz_cmp, mpz_fits_slong_p, mpz_get_si, mpz_get_str, mpz_get_ui, mpz_set_str,
    mpz_set_ui, mpz_t,
};
use serde::Serialize;

use crate::gc::{Gc, Trace};

#[derive(Debug, thiserror::Error)]
pub enum ParseIntError {
    #[error("string contains null byte")]
    InvalidNullByte,
    #[error("invalid format for int")]
    InvalidFormat,
}

#[derive(Debug, Clone, Copy)]
pub struct Int {
    limbs: [MaybeUninit<limb_t>; 1],
    inline: bool,
    inner: mpz_t,
}

impl Trace for Int {
    fn trace(&self, _gc: &mut Gc<'_>) {}
}

impl Int {
    pub fn new() -> Self {
        let inner = unsafe {
            let mut z = MaybeUninit::uninit();
            gmp::mpz_init(z.as_mut_ptr());
            z.assume_init()
        };

        Self {
            inline: false,
            limbs: [MaybeUninit::uninit()],
            inner,
        }
    }

    pub fn to_usize(self) -> usize {
        unsafe { mpz_get_ui(&self.get()) as usize }
    }

    pub fn to_i64(self) -> i64 {
        unsafe { mpz_get_si(&self.get()) }
    }

    pub fn fits_in_i64(&self) -> bool {
        unsafe { mpz_fits_slong_p(&self.get()) != 0 }
    }

    pub fn is_small(&self) -> bool {
        self.inline
    }

    fn get(&self) -> mpz_t {
        if self.inline {
            return mpz_t {
                alloc: self.inner.alloc,
                size: self.inner.size,
                d: NonNull::from(&self.limbs[..]).cast(),
            };
        }

        self.inner
    }

    pub fn parse(s: &str) -> Result<Self, ParseIntError> {
        let mut int = Int::new();
        let cstr = CString::from_str(s).map_err(|_| ParseIntError::InvalidNullByte)?;

        unsafe {
            if mpz_set_str(&mut int.inner, cstr.into_raw(), 10) == -1 {
                return Err(ParseIntError::InvalidFormat);
            }
        }

        Ok(int)
    }
}

impl Serialize for Int {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl PartialOrd for Int {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let result = unsafe { mpz_cmp(&self.get(), &other.get()) };

        if result < 0 {
            Some(Ordering::Less)
        } else if result > 0 {
            Some(Ordering::Greater)
        } else {
            Some(Ordering::Equal)
        }
    }
}

impl PartialEq for Int {
    fn eq(&self, other: &Self) -> bool {
        unsafe { mpz_cmp(&self.get(), &other.get()) == 0 }
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = unsafe { mpz_get_str(ptr::null::<c_char>().cast_mut(), 10, &self.get()) };
        let cstr = unsafe { CStr::from_ptr(c) };
        let s = cstr.to_string_lossy();

        write!(f, "{s}")
    }
}

impl Default for Int {
    fn default() -> Self {
        Self {
            inline: true,
            limbs: [MaybeUninit::uninit()],
            inner: mpz_t {
                alloc: 1,
                size: 0,
                d: NonNull::dangling(),
            },
        }
    }
}

impl From<usize> for Int {
    fn from(value: usize) -> Self {
        if value < i64::MAX as usize {
            return Int::from(value as i64);
        }

        let mut int = Int::new();
        unsafe {
            mpz_set_ui(&mut int.inner, value as u64);
        }

        int
    }
}

impl From<i64> for Int {
    fn from(value: i64) -> Self {
        if value == 0 {
            return Self::default();
        }

        let limb = MaybeUninit::new(value.unsigned_abs() as limb_t);

        Self {
            inline: true,
            limbs: [limb],
            inner: mpz_t {
                alloc: 1,
                size: if value < 0 { -1 } else { 1 },
                d: NonNull::dangling(),
            },
        }
    }
}

macro_rules! impl_from_small {
    ($($ty:ty),+) => {
        $(impl From<$ty> for Int {
            fn from(value: $ty) -> Self {
                Self::from(value as i64)
            }
        })+
    };
}

macro_rules! impl_op {
    ($($name:ident $fn:ident $mpz_fn:ident $op:tt),+) => {
        $(impl<'gc> $name for Int {
            type Output = Int;

            fn $fn(self, rhs: Self) -> Self::Output {
                let mut result = Int::new();
                unsafe {
                    gmp::$mpz_fn(&mut result.inner, &self.get(), &rhs.get());
                }

                result
            }
        })+
    };
}

macro_rules! impl_shift_op {
    ($($name:ident $fn:ident $mpz_fn:ident $op:tt),+) => {
        $(impl<'gc> $name for Int {
            type Output = Int;

            fn $fn(self, rhs: Self) -> Self::Output {
                let mut result = Int::new();
                unsafe {
                    gmp::$mpz_fn(&mut result.inner, &self.get(), rhs.to_usize() as u64);
                }

                result
            }
        })+
    };
}

impl_from_small!(i8, i16, i32, u8, u16, u32);

impl_op!(
    Add add mpz_add +,
    Sub sub mpz_sub -,
    Mul mul mpz_mul *,
    Div div mpz_tdiv_q /,
    Rem rem mpz_mod %,
    BitAnd bitand mpz_and &,
    BitOr bitor mpz_ior |,
    BitXor bitxor mpz_xor ^
);

impl_shift_op!(
    Shl shl mpz_mul_2exp <<,
    Shr shr mpz_tdiv_q_2exp >>
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_i64_negative() {
        let u = Int::from(-114748364i64);

        assert!(u.is_small());
        assert_eq!(-114748364i64, u.to_i64());
    }

    #[test]
    fn test_from_i64() {
        let u = Int::from(2147483648i64);

        assert!(u.is_small());
        assert_eq!(2147483648i64, u.to_i64());
    }

    #[test]
    fn test_from_i32() {
        let u = Int::from(10i32);

        assert!(u.is_small());
        assert_eq!(10, u.to_i64());
    }

    #[test]
    fn test_from_usize() {
        let u = Int::from(9223372036854775808usize);

        assert!(!u.is_small());
        assert_eq!(9223372036854775808usize, u.to_usize());
    }

    #[test]
    fn test_parse() {
        let s = "9223372036854775810";
        let int = Int::parse(s).expect("failed to parse int");

        assert!(!int.is_small());
        assert_eq!(s.to_string(), int.to_string());
    }
}
