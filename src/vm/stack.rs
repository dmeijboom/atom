use crate::runtime::types::Value;
use crate::runtime::{ErrorKind, Result, RuntimeError};

#[derive(Debug)]
pub struct Stack {
    data: Vec<Value>,
}

impl Stack {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    #[inline]
    pub fn push(&mut self, value: Value) {
        self.data.push(value);
    }

    #[inline]
    pub fn delete(&mut self) -> Result<()> {
        self.data.pop().ok_or_else(|| {
            RuntimeError::new(
                ErrorKind::FatalError,
                "expected element on stack (in discard)".to_string(),
            )
        })?;

        Ok(())
    }

    pub fn try_pop(&mut self) -> Option<Value> {
        self.data.pop()
    }

    pub fn pop(&mut self) -> Value {
        debug_assert!(!self.data.is_empty(), "expected element on stack (in pop)");

        self.data.remove(self.data.len() - 1)
    }

    pub fn pop_many(&mut self, len: usize, buffer: &mut Vec<Value>) -> Result<()> {
        if len == 0 {
            return Ok(());
        }

        if self.data.len() < len {
            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!("expected {} elements on stack", len),
            ));
        }

        for value in self.data.drain(self.data.len() - len..) {
            buffer.push(value);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::types::Value;

    use super::Stack;

    #[test]
    fn test_pop_many() {
        let mut stack = Stack::new();

        stack.push(Value::Int(10));
        stack.push(Value::Int(20));
        stack.push(Value::Int(30));
        stack.push(Value::Int(40));
        stack.push(Value::Int(50));

        let mut buffer = vec![];

        stack.pop_many(3, &mut buffer).unwrap();

        assert_eq!(buffer, vec![Value::Int(30), Value::Int(40), Value::Int(50)]);
    }
}
