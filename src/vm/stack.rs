use std::cell::{Ref, RefCell};
use std::ops::Deref;
use std::rc::Rc;

use crate::runtime::Value;
use crate::runtime::{Result, RuntimeError};

#[derive(PartialEq)]
pub enum Stacked {
    ByValue(Value),
    ByRef(Rc<RefCell<Value>>),
}

impl Stacked {
    pub fn borrow(&self) -> StackedBorrowed<'_> {
        StackedBorrowed::new(self)
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

pub struct Stack {
    data: Vec<Stacked>,
}

impl Stack {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    pub fn push(&mut self, value: Value) {
        self.data.push(Stacked::ByValue(value));
    }

    pub fn push_ref(&mut self, value: Rc<RefCell<Value>>) {
        self.data.push(Stacked::ByRef(value));
    }

    pub fn delete(&mut self) -> Result<()> {
        self.data.pop().ok_or_else(|| {
            RuntimeError::new("expected element on stack (in discard)".to_string())
        })?;

        Ok(())
    }

    pub fn pop_stacked(&mut self) -> Result<Stacked> {
        if let Some(value) = self.data.pop() {
            return Ok(value);
        }

        Err(RuntimeError::new("expecting element on stack".to_string()))
    }

    pub fn pop(&mut self) -> Result<Value> {
        Ok(match self.pop_stacked()? {
            Stacked::ByValue(value) => value,
            Stacked::ByRef(value_ref) => value_ref.borrow().clone(),
        })
    }

    pub fn pop_ref(&mut self) -> Result<Rc<RefCell<Value>>> {
        Ok(match self.pop_stacked()? {
            // This is useless as this is a reference of a copied value but other languages
            // seem to support code like this as well: `[0, 1][0] = 1;`
            Stacked::ByValue(value) => Rc::new(RefCell::new(value)),

            Stacked::ByRef(value_ref) => value_ref,
        })
    }

    pub fn pop_many(&mut self, mut len: usize) -> Result<Vec<Value>> {
        let mut values = Vec::with_capacity(len);

        while len > 0 {
            values.push(self.pop()?);
            len -= 1;
        }

        values.reverse();

        Ok(values)
    }
}
