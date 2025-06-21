use std::{
    cmp::Ordering,
    ffi::{c_char, CStr, CString},
    fmt::Display,
    mem::MaybeUninit,
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

#[derive(Debug, Clone)]
pub enum BigInt {
    Small {
        size: i32,
        limbs: [MaybeUninit<limb_t>; 1],
    },
    Big(mpz_t),
}

impl Trace for BigInt {
    fn trace(&self, _gc: &mut Gc<'_>) {}
}

impl BigInt {
    pub fn as_usize(&self) -> usize {
        unsafe { mpz_get_ui(&self.get()) as usize }
    }

    pub fn as_i64(&self) -> i64 {
        match self {
            BigInt::Small { size, limbs } => {
                let abs: u64 = unsafe { limbs[0].assume_init() };

                match size.cmp(&0) {
                    Ordering::Less if abs - 1 == i64::MAX as u64 => i64::MIN,
                    Ordering::Less => -(abs as i64),
                    Ordering::Equal => 0,
                    Ordering::Greater => abs as i64,
                }
            }
            BigInt::Big(mpz_t) => unsafe { mpz_get_si(mpz_t) },
        }
    }

    pub fn fits_in_i64(&self) -> bool {
        unsafe { mpz_fits_slong_p(&self.get()) != 0 }
    }

    pub fn as_unsigned_abs(&self) -> (bool, u64) {
        match self {
            BigInt::Small { size, limbs } => (*size < 0, unsafe { limbs[0].assume_init() }),
            BigInt::Big(mpz_t) => (mpz_t.size < 0, unsafe { mpz_get_ui(mpz_t) }),
        }
    }

    pub fn is_small(&self) -> bool {
        matches!(self, BigInt::Small { .. })
    }

    pub fn replace_with(&mut self, other: Self) {
        if let BigInt::Big(mpz_t) = self {
            unsafe {
                gmp::mpz_clear(mpz_t);
            }
        }

        *self = other;
    }

    fn get(&self) -> mpz_t {
        match self {
            BigInt::Small { size, limbs } => mpz_t {
                alloc: 1,
                size: *size,
                d: NonNull::from(&limbs[0]).cast(),
            },
            BigInt::Big(mpz_t) => *mpz_t,
        }
    }

    fn get_mut(&mut self) -> &mut mpz_t {
        match self {
            BigInt::Small { .. } => unreachable!(),
            BigInt::Big(mpz_t) => mpz_t,
        }
    }

    pub fn parse(s: &str) -> Result<Self, ParseIntError> {
        let mut int = BigInt::default();
        let cstr = CString::from_str(s).map_err(|_| ParseIntError::InvalidNullByte)?;

        unsafe {
            if let BigInt::Big(mpz_t) = &mut int {
                if mpz_set_str(mpz_t, cstr.into_raw(), 10) == -1 {
                    return Err(ParseIntError::InvalidFormat);
                }
            }
        }

        Ok(int)
    }
}

impl Serialize for BigInt {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl PartialOrd for BigInt {
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

impl PartialEq for BigInt {
    fn eq(&self, other: &Self) -> bool {
        unsafe { mpz_cmp(&self.get(), &other.get()) == 0 }
    }
}

impl Display for BigInt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c = unsafe { mpz_get_str(ptr::null::<c_char>().cast_mut(), 10, &self.get()) };
        let cstr = unsafe { CStr::from_ptr(c) };
        let s = cstr.to_string_lossy();

        write!(f, "{s}")
    }
}

impl Default for BigInt {
    fn default() -> Self {
        let mpz_t = unsafe {
            let mut z = MaybeUninit::uninit();
            gmp::mpz_init(z.as_mut_ptr());
            z.assume_init()
        };

        Self::Big(mpz_t)
    }
}

impl From<usize> for BigInt {
    fn from(value: usize) -> Self {
        if value < i64::MAX as usize {
            return BigInt::from(value as i64);
        }

        let mut int = BigInt::default();
        unsafe {
            mpz_set_ui(int.get_mut(), value as u64);
        }

        int
    }
}

impl From<i64> for BigInt {
    #[inline]
    fn from(value: i64) -> Self {
        Self::Small {
            limbs: [MaybeUninit::new(value.unsigned_abs() as limb_t)],
            size: match value.cmp(&0) {
                Ordering::Less => -1,
                Ordering::Equal => 0,
                Ordering::Greater => 1,
            },
        }
    }
}

macro_rules! impl_from_small {
    ($($ty:ty),+) => {
        $(impl From<$ty> for BigInt {
            fn from(value: $ty) -> Self {
                Self::from(value as i64)
            }
        })+
    };
}

macro_rules! impl_op {
    ($($fn:ident $mpz_fn:ident),+) => {
        impl BigInt {
            $(
                #[inline]
                pub fn $fn(&self, rhs: &Self, result: &mut BigInt) {
                    unsafe {
                        gmp::$mpz_fn(result.get_mut(), &self.get(), &rhs.get());
                    }
                }
            )+
        }
    };
}

macro_rules! impl_shift_op {
    ($($fn:ident $mpz_fn:ident),+) => {
        impl BigInt {
            $(
                #[inline]
                pub fn $fn(&self, rhs: &Self, result: &mut BigInt) {
                    unsafe {
                        gmp::$mpz_fn(result.get_mut(), &self.get(), rhs.as_usize() as u64);
                    }
                }
            )+
        }
    };
}

impl_from_small!(i8, i16, i32, u8, u16, u32);

impl_op!(
    add mpz_add,
    sub mpz_sub,
    mul mpz_mul,
    div mpz_tdiv_q,
    rem mpz_mod,
    bitand mpz_and,
    bitor mpz_ior,
    bitxor mpz_xor
);

impl_shift_op!(
    shl mpz_mul_2exp,
    shr mpz_tdiv_q_2exp
);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_partial_eq() {
        let a = BigInt::from(10);
        let b = BigInt::from(5);
        let c = BigInt::from(5);
        let mut r = BigInt::default();

        b.add(&c, &mut r);

        assert_eq!(a, r);
        assert_eq!(b, c);
    }

    #[test]
    fn test_unsigned_abs_small() {
        let i = BigInt::from(-10000);

        assert!(i.is_small());
        assert_eq!((true, 10000), i.as_unsigned_abs());
    }

    #[test]
    fn test_from_i64_negative() {
        let u = BigInt::from(-114748364i64);

        assert!(u.is_small());
        assert_eq!(-114748364i64, u.as_i64());
    }

    #[test]
    fn test_from_i64() {
        let u = BigInt::from(2147483648i64);

        assert!(u.is_small());
        assert_eq!(2147483648i64, u.as_i64());
    }

    #[test]
    fn test_from_i32() {
        let u = BigInt::from(10i32);

        assert!(u.is_small());
        assert_eq!(10, u.as_i64());
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_from_usize() {
        let u = BigInt::from(9223372036854775808usize);

        assert!(!u.is_small());
        assert_eq!(9223372036854775808usize, u.as_usize());
    }

    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_parse() {
        let s = "9223372036854775810";
        let int = BigInt::parse(s).expect("failed to parse int");

        assert!(!int.is_small());
        assert_eq!(s.to_string(), int.to_string());
    }
}
