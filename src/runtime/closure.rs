use super::atom_ref::AtomRef;
use super::r#fn::Fn;
use super::value::Value;

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