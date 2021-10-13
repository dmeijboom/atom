use crate::class::Class;
use crate::{AtomRef, Value};

#[derive(Debug)]
pub struct Object {
    pub class: AtomRef<Class>,
    fields: Box<[Value]>,
}

impl Object {
    pub fn new(class: AtomRef<Class>, fields: Vec<Value>) -> Self {
        Self {
            class,
            fields: fields.into_boxed_slice(),
        }
    }

    pub fn get_field(&self, index: usize) -> Option<&Value> {
        self.fields.get(index)
    }

    pub fn get_field_mut(&mut self, index: usize) -> Option<&mut Value> {
        self.fields.get_mut(index)
    }

    pub fn set_field_value(&mut self, index: usize, value: Value) -> bool {
        if let Some(field) = self.get_field_mut(index) {
            *field = value;

            return true;
        }

        false
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.class.as_ref() == other.class.as_ref() && self.fields == other.fields
    }
}
