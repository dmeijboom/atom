use std::fmt::{Debug, Formatter};
use std::ops::Deref;

/// Symbols are unique names in atom encoded as a byte slice or constant value
#[derive(Clone)]
pub struct Symbol {
    pub name: Box<[u8]>,
}

impl Symbol {
    pub fn new(name: impl AsRef<str>) -> Self {
        Self {
            name: name.as_ref().as_bytes().into(),
        }
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        let length = self.name.len();

        if length != other.name.len() {
            return false;
        }

        // This is fine as we already verified the length of the byte array
        unsafe {
            for i in 0..length {
                if self.name.get_unchecked(i) != other.name.get_unchecked(i) {
                    return false;
                }
            }
        }

        true
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl PartialEq<str> for Symbol {
    fn eq(&self, other: &str) -> bool {
        self.as_ref() == other
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        // Use of unsafe is fine as we use a String to create the symbol
        unsafe { std::str::from_utf8_unchecked(&self.name) }
    }
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
