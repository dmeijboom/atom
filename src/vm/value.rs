use std::cell::{Ref, RefCell};
use std::ops::Deref;
use std::rc::Rc;

use crate::runtime::{Value, AtomRef};

#[derive(PartialEq)]
pub enum VmValue {
    Value(Value),
    Ref(AtomRef),
}

impl VmValue {
    pub fn borrow(&self) -> VmValueBorrowed<'_> {
        VmValueBorrowed::new(self)
    }

    pub fn into_value(self) -> Value {
        match self {
            Self::Value(value) => value,
            Self::Ref(value_ref) => value_ref.clone_inner_or_unwrap(),
        }
    }

    pub fn into_runtime_value(self) -> Value {
        match self {
            Self::Value(value) => value,
            Self::Ref(value_ref) => Value::Ref(value_ref),
        }
    }

    pub fn into_ref(self) -> Rc<RefCell<Value>> {
        match self {
            // This is useless as this is a reference of a copied value but other languages
            // seem to support code like this as well: `[0, 1][0] = 1;`
            Self::Value(value) => Rc::new(RefCell::new(value)),

            Self::Ref(value_ref) => value_ref,
        }
    }
}

impl From<Value> for VmValue {
    fn from(value: Value) -> Self {
        match value {
            // Make sure all reference types are created by reference
            Value::Object(_) | Value::Map(_) | Value::Array(_) | Value::String(_) => {
                VmValue::Ref(AtomRef::new(value))
            }

            _ => VmValue::Value(value),
        }
    }
}

impl Clone for VmValue {
    fn clone(&self) -> Self {
        match self {
            VmValue::Value(value) => VmValue::Value(value.clone()),
            VmValue::Ref(value_ref) => VmValue::Ref(AtomRef::clone(value_ref)),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone();
    }
}

pub struct VmValueBorrowed<'s> {
    inner: &'s Value,
}

impl<'s> VmValueBorrowed<'s> {
    pub fn new(stacked: &'s VmValue) -> Self {
        match &stacked {
            VmValue::Value(inner) => Self { inner },
            VmValue::Ref(value_ref) => Self {
                inner: value_ref.as_ref(),
            },
        }
    }
}

impl Deref for VmValueBorrowed<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}
