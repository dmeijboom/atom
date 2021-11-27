use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use wyhash2::WyHash;

use crate::runtime::{AtomRef, Origin};

use super::r#fn::Fn;

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub mutable: bool,
    pub public: bool,
}

impl Field {
    pub fn new(name: String, mutable: bool, public: bool) -> Self {
        Self {
            name,
            mutable,
            public,
        }
    }
}

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub origin: Origin,
    pub fields: Vec<Field>,
    pub methods: HashMap<String, AtomRef<Fn>, WyHash>,
    pub static_methods: HashMap<String, AtomRef<Fn>, WyHash>,
}

impl Class {
    pub fn get_field(&self, name: &str) -> Option<(usize, &Field)> {
        for (i, field) in self.fields.iter().enumerate() {
            if field.name == name {
                return Some((i, field));
            }
        }

        None
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.origin.module_name, self.name)
    }
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.origin == other.origin
    }
}
