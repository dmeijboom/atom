use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use indexmap::IndexMap;
use wyhash2::WyHash;

use super::atom_ref::AtomRef;
use super::origin::Origin;
use super::r#fn::Fn;

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub id: usize,
    pub name: String,
    pub mutable: bool,
    pub public: bool,
}

impl Field {
    pub fn new(id: usize, name: String, mutable: bool, public: bool) -> Self {
        Self {
            id,
            name,
            mutable,
            public,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub origin: Origin,
    pub fields: IndexMap<String, Field, WyHash>,
    pub methods: HashMap<String, AtomRef<Fn>, WyHash>,
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
