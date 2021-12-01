use crate::runtime::types::{AtomRef, Fn, Value};

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
