use crate::runtime::Value;
use crate::runtime::{Result, RuntimeError};

pub struct Stack {
    data: Vec<Value>,
}

impl Stack {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    pub fn push(&mut self, value: Value) {
        self.data.push(value);
    }

    pub fn delete(&mut self) -> Result<()> {
        self.data.pop().ok_or_else(|| {
            RuntimeError::new("expected element on stack (in discard)".to_string())
        })?;

        Ok(())
    }

    #[cfg(test)]
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn pop(&mut self) -> Value {
        if let Some(value) = self.data.pop() {
            return value;
        }

        unreachable!("expecting element on stack".to_string())
    }

    pub fn pop_many_t<T>(&mut self, len: usize, mut map: impl FnMut(Value) -> T) -> Result<Vec<T>> {
        let data_len = self.data.len();

        if data_len < len {
            return Err(RuntimeError::new(format!(
                "expected {} elements on stack",
                len
            )));
        }

        Ok(self
            .data
            .drain((data_len - len)..)
            .into_iter()
            .map(|vm_value| map(vm_value))
            .collect())
    }

    pub fn pop_many(&mut self, len: usize) -> Result<Vec<Value>> {
        let data_len = self.data.len();

        if len == 0 {
            return Ok(vec![]);
        }

        if data_len < len {
            return Err(RuntimeError::new(format!(
                "expected {} elements on stack",
                len
            )));
        }

        Ok(self.data.split_off(data_len - len))
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::{Int, Value};

    use super::Stack;

    #[test]
    fn test_pop_many() {
        let mut stack = Stack::new();

        stack.push(Value::Int(Int::Uint8(10)));
        stack.push(Value::Int(Int::Uint8(20)));
        stack.push(Value::Int(Int::Uint8(30)));
        stack.push(Value::Int(Int::Uint8(40)));
        stack.push(Value::Int(Int::Uint8(50)));

        assert_eq!(
            stack.pop_many(3),
            Ok(vec![
                Value::Int(Int::Uint8(30)),
                Value::Int(Int::Uint8(40)),
                Value::Int(Int::Uint8(50))
            ])
        );
    }
}
