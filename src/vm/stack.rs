use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::Value;
use crate::runtime::{Result, RuntimeError};

#[derive(PartialEq)]
pub enum Stacked {
    ByValue(Value),
    ByRef(Rc<RefCell<Value>>),
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
            if value == Stacked::ByValue(Value::Invalid) {
                return Err(RuntimeError::new(
                    "unable to use void Fn as a value".to_string(),
                ));
            }

            return Ok(value);
        }

        Err(RuntimeError::new("expecting element on stack".to_string()))
    }

    pub fn pop(&mut self) -> Result<Value> {
        let stacked = self.pop_stacked()?;

        Ok(match stacked {
            Stacked::ByValue(value) => value,
            Stacked::ByRef(value_ref) => value_ref.borrow().clone(),
        })
    }

    pub fn pop_ref(&mut self) -> Result<Rc<RefCell<Value>>> {
        let stacked = self.pop_stacked()?;

        Ok(match stacked {
            // This is useless as this is a reference of a copied value but other languages
            // seem to support code like this as well: `[0, 1][0] = 1;`
            Stacked::ByValue(value) => Rc::new(RefCell::new(value)),

            Stacked::ByRef(value_ref) => value_ref,
        })
    }

    pub fn pop_many(&mut self, mut len: usize) -> Result<Vec<Value>> {
        let mut values = vec![];

        while len > 0 {
            values.insert(0, self.pop()?);
            len -= 1;
        }

        Ok(values)
    }
}
