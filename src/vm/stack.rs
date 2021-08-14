use std::cell::RefCell;
use std::rc::Rc;

use smallvec::SmallVec;

use crate::runtime::Value;
use crate::runtime::{Result, RuntimeError};

use super::stacked::Stacked;

pub struct Stack {
    data: Vec<Stacked>,
}

impl Stack {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    pub fn push_stacked(&mut self, stacked: Stacked) {
        self.data.push(stacked);
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

    pub fn pop_many(&mut self, len: usize) -> Result<SmallVec<[Value; 2]>> {
        let data_len = self.data.len();

        if data_len < len {
            return Err(RuntimeError::new(format!(
                "expected {} elements on stack",
                len
            )));
        }

        // Resort to a single (or double) .pop() when pop_many was called with a single element
        if len == 1 {
            return Ok(SmallVec::<[Value; 2]>::from_buf_and_len(
                [self.pop()?, Value::Void],
                1,
            ));
        }

        let values = self
            .data
            .drain((data_len - len)..)
            .into_iter()
            .map(|stacked| match stacked {
                Stacked::ByValue(value) => value,
                Stacked::ByRef(value_ref) => value_ref.borrow().clone(),
            })
            .collect();

        Ok(values)
    }
}
