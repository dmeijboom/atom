use std::any::Any;

use super::atom_ref::AtomRef;

#[derive(Debug)]
pub struct Extern(pub AtomRef<Box<dyn Any>>);

impl PartialEq for Extern {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
