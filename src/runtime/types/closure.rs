use crate::runtime::{AtomRef, Value};

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
