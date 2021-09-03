use std::hash::{Hash, Hasher};

use crate::Value;

use super::atom_ref::AtomRef;
use super::r#fn::Fn;

#[derive(Debug)]
pub struct Closure {
    pub func: AtomRef<Fn>,
    pub values: Vec<Value>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        if self.func != other.func {
            return false;
        }

        self.values == other.values
    }
}

impl Hash for Closure {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.func.hash(state);
    }
}
