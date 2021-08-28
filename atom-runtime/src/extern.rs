use std::any::Any;
use std::hash::{Hash, Hasher};

use super::atom_ref::AtomRef;

#[derive(Debug)]
pub struct Extern(pub AtomRef<Box<dyn Any>>);

impl PartialEq for Extern {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl Hash for Extern {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}
