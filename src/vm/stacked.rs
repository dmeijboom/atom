use std::cell::{Ref, RefCell};
use std::ops::Deref;
use std::rc::Rc;

use crate::runtime::Value;

#[derive(PartialEq)]
pub enum Stacked {
    ByValue(Value),
    ByRef(Rc<RefCell<Value>>),
}

impl Stacked {
    pub fn borrow(&self) -> StackedBorrowed<'_> {
        StackedBorrowed::new(self)
    }

    pub fn into_value(self) -> Value {
        match self {
            Self::ByValue(value) => value,
            Self::ByRef(value_ref) => match Rc::try_unwrap(value_ref) {
                Ok(value_cell) => value_cell.into_inner(),
                Err(value_ref) => value_ref.borrow().clone(),
            },
        }
    }

    pub fn into_ref(self) -> Rc<RefCell<Value>> {
        match self {
            // This is useless as this is a reference of a copied value but other languages
            // seem to support code like this as well: `[0, 1][0] = 1;`
            Self::ByValue(value) => Rc::new(RefCell::new(value)),

            Self::ByRef(value_ref) => value_ref,
        }
    }
}

impl Clone for Stacked {
    fn clone(&self) -> Self {
        match self {
            Stacked::ByValue(value) => Stacked::ByValue(value.clone()),
            Stacked::ByRef(value_ref) => Stacked::ByRef(Rc::clone(value_ref)),
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone();
    }
}

enum ValueOrRef<'v> {
    Value(&'v Value),
    ValueRef(Ref<'v, Value>),
}

pub struct StackedBorrowed<'s> {
    data: ValueOrRef<'s>,
}

impl<'s> StackedBorrowed<'s> {
    pub fn new(stacked: &'s Stacked) -> Self {
        match &stacked {
            Stacked::ByValue(value) => Self {
                data: ValueOrRef::Value(value),
            },
            Stacked::ByRef(value_ref) => Self {
                data: ValueOrRef::ValueRef(value_ref.borrow()),
            },
        }
    }
}

impl Deref for StackedBorrowed<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match &self.data {
            ValueOrRef::Value(value) => value,
            ValueOrRef::ValueRef(value_ref) => value_ref,
        }
    }
}
