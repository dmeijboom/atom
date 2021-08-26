use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

use wyhash2::WyHash;

use crate::origin::Origin;
use crate::AtomRef;

use super::r#fn::Fn;

#[derive(Debug, PartialEq, Hash)]
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

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub public: bool,
    pub origin: Origin,
    pub fields: HashMap<String, Field, WyHash>,
    pub methods: HashMap<String, AtomRef<Fn>, WyHash>,
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.origin.module_name, self.name)
    }
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.origin.hash(state);
    }
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.origin == other.origin
    }
}
