use std::fmt::{Debug, Display};
use std::hash::Hash;

use crate::gc::{Gc, Trace};

use super::array::Array;

pub struct Str(pub Array<u8>);

impl Str {
    pub fn as_str(&self) -> &str {
        self.as_ref()
    }

    pub fn from_string(gc: &mut Gc, s: String) -> Self {
        Self(Array::from_vec(gc, s.into_bytes()))
    }

    pub unsafe fn as_static_str(&self) -> &'static str {
        let bytes: &'static [u8] = std::mem::transmute(self.0.as_slice());
        std::str::from_utf8_unchecked(bytes)
    }
}

impl Hash for Str {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl Eq for Str {}

impl PartialEq for Str {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Debug for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Str(\"{}\")", self.as_str())
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl Trace for Str {
    fn trace(&self, gc: &mut Gc) {
        self.0.trace(gc);
    }
}

impl AsRef<str> for Str {
    fn as_ref(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.0.as_slice()) }
    }
}
