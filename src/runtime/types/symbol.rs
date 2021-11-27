use std::ops::Deref;

use crate::runtime::{atom_string_to_str, AtomString};

/// Symbols are unique names in atom encoded as a byte slice or constant value
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: AtomString,
}

impl Symbol {
    pub fn new(name: AtomString) -> Self {
        Self { name }
    }
}

impl From<&str> for Symbol {
    fn from(name: &str) -> Self {
        Self {
            name: AtomString::from(name.as_bytes()),
        }
    }
}

impl AsRef<str> for Symbol {
    fn as_ref(&self) -> &str {
        atom_string_to_str(&self.name)
    }
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}
