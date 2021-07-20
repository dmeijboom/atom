use crate::runtime::Value;

use super::result::{Result, RuntimeError};

pub(crate) struct Stack {
    data: Vec<Value>,
}

impl Stack {
    pub(crate) fn new() -> Self {
        Self {
            data: vec![],
        }
    }

    pub(crate) fn push(&mut self, value: Value) {
        self.data.push(value);
    }

    pub(crate) fn pop(&mut self) -> Result<Value> {
        if let Some(value) = self.data.pop() {
            return Ok(value);
        }

        Err(RuntimeError::new("expecting element on stack".to_string()))
    }

    pub(crate) fn pop_many(&mut self, mut len: usize) -> Result<Vec<Value>> {
        let mut values = vec![];

        while len > 0 {
            values.insert(0, self.pop()?);
            len -= 1;
        }

        Ok(values)
    }
}

