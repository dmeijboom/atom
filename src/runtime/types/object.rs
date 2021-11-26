use std::ops::Deref;

use crate::runtime::{AtomRef, AtomRefMut, Value};

use super::class::Class;

#[derive(Debug)]
pub struct Object {
    pub class: AtomRef<Class>,
    fields: AtomRefMut<[Value]>,
}

impl Object {
    pub fn new(class: AtomRef<Class>, fields: AtomRefMut<[Value]>) -> Self {
        Self { class, fields }
    }

    pub fn get_fields(&self) -> &[Value] {
        self.fields.deref()
    }

    pub fn get_field(&self, index: usize) -> Option<&Value> {
        self.fields.get(index)
    }

    pub fn get_field_mut(&mut self, index: usize) -> Option<&mut Value> {
        self.fields.as_mut().get_mut(index)
    }

    pub fn set_field_value(&mut self, index: usize, value: Value) {
        self.fields.as_mut()[index] = value;
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
