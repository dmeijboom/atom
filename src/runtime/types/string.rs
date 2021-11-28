use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::{fmt, mem};

use crate::runtime::AtomArray;

/// AtomString implements a UTF-8 encoded immutable string
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AtomString(AtomArray<u8>);

impl AtomString {
    pub fn new(s: String) -> Self {
        Self(AtomArray::from(s.into_bytes()))
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        // This should be safe, because the AtomString is guaranteed to be valid UTF-8
        unsafe { mem::transmute(self.0.as_ref()) }
    }
}

impl From<AtomArray<u8>> for AtomString {
    fn from(array: AtomArray<u8>) -> Self {
        AtomString(array)
    }
}

impl From<&str> for AtomString {
    fn from(buff: &str) -> Self {
        AtomString::from(AtomArray::from(buff.as_bytes()))
    }
}

impl Clone for AtomString {
    fn clone(&self) -> Self {
        AtomString(AtomArray::clone(&self.0))
    }
}

impl Display for AtomString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl PartialEq<str> for AtomString {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<AtomString> for str {
    fn eq(&self, other: &AtomString) -> bool {
        other.as_str() == self
    }
}

impl Deref for AtomString {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
