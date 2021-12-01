use crate::runtime::types::{AtomRef, AtomRefMut, Class, Value};

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
