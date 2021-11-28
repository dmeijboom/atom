use std::ops::Deref;

use crate::runtime::types::string::AtomString;

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
            name: AtomString::from(name),
        }
    }
}

impl Deref for Symbol {
    type Target = str;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.name.deref()
    }
}
