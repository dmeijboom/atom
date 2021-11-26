use crate::runtime::{AtomRef, AtomRefMut, Value};

use super::class::Class;

#[derive(Debug)]
pub struct Object {
    pub class: AtomRef<Class>,
    pub fields: AtomRefMut<[Value]>,
}

impl Object {
    pub fn new(class: AtomRef<Class>, fields: AtomRefMut<[Value]>) -> Self {
        Self { class, fields }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.class.as_ref() == other.class.as_ref() && self.fields == other.fields
    }
}

impl Clone for Object {
    fn clone(&self) -> Self {
        Self {
            class: AtomRef::clone(&self.class),
            fields: AtomRefMut::clone(&self.fields),
        }
    }
}
