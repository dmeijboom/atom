use std::cell::{Ref, RefCell, RefMut};
use std::ops::{Deref, DerefMut};
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

    pub fn borrow_mut(&mut self) -> StackedBorrowedMut<'_> {
        StackedBorrowedMut::new(self)
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
            ValueOrRef::ValueRef(value_ref) => &value_ref,
        }
    }
}

enum ValueOrRefMut<'v> {
    Value(&'v mut Value),
    ValueRef(RefMut<'v, Value>),
}

pub struct StackedBorrowedMut<'s> {
    data: ValueOrRefMut<'s>,
}

impl<'s> StackedBorrowedMut<'s> {
    pub fn new(stacked: &'s mut Stacked) -> Self {
        match stacked {
            Stacked::ByValue(value) => Self {
                data: ValueOrRefMut::Value(value),
            },
            Stacked::ByRef(value_ref) => Self {
                data: ValueOrRefMut::ValueRef(value_ref.borrow_mut()),
            },
        }
    }
}

impl Deref for StackedBorrowedMut<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match &self.data {
            ValueOrRefMut::Value(value) => value,
            ValueOrRefMut::ValueRef(value_ref) => value_ref,
        }
    }
}

impl DerefMut for StackedBorrowedMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match &mut self.data {
            ValueOrRefMut::Value(value) => value,
            ValueOrRefMut::ValueRef(value_ref) => value_ref,
        }
    }
}
