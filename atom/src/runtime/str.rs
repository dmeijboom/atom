use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::gc::{Gc, Trace};

use super::array::Array;
use super::error::RuntimeError;

pub struct Str<'gc>(pub Array<'gc, u8>);

impl<'gc> Str<'gc> {
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }

    pub fn copy_from_str(gc: &mut Gc<'gc>, s: &str) -> Result<Self, RuntimeError> {
        Ok(Self(Array::copy_from_slice(gc, s.as_bytes())?))
    }

    pub unsafe fn as_static_str(&self) -> &'static str {
        let bytes: &'static [u8] = std::mem::transmute(self.0.as_slice());
        std::str::from_utf8_unchecked(bytes)
    }
}

impl<'gc> Hash for Str<'gc> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl<'gc> Eq for Str<'gc> {}

impl<'gc> PartialEq for Str<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<'gc> Debug for Str<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Str(\"{}\")", self.as_str())
    }
}

impl<'gc> Display for Str<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl<'gc> Trace for Str<'gc> {
    fn trace(&self, gc: &mut Gc) {
        self.0.trace(gc);
    }
}

impl<'gc> AsRef<str> for Str<'gc> {
    fn as_ref(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.0.as_slice()) }
    }
}
