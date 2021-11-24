use std::ops::Deref;

use crate::runtime::AtomString;

/// Symbols are unique names in atom encoded as a byte slice or constant value
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: AtomString,
}

impl Symbol {
    pub fn new(name: String) -> Self {
        Self {
            name: AtomString::from(name.into_bytes()),
        }
    }
}

impl From<&str> for Symbol {
    fn from(name: &str) -> Self {
        Self {
            name: AtomString::copy_from_slice(name.as_bytes()),
        }
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        // Use of unsafe is fine as we use a String to create the symbol
        unsafe { std::str::from_utf8_unchecked(self.name.as_ref()) }
    }
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
