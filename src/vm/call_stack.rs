use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Pos;
use crate::runtime::{Result, RuntimeError, Trace, TypeId, Value};

pub struct CallContext {
    pub pos: Pos,
    pub id: TypeId,
    pub finished: bool,
    pub return_value: Option<Value>,
    pub locals: HashMap<usize, Rc<RefCell<Value>>>,
    pub named_locals: HashMap<String, Rc<RefCell<Value>>>,
}

impl CallContext {
    pub fn new(pos: Pos, id: TypeId) -> Self {
        Self {
            id,
            pos,
            finished: false,
            return_value: None,
            locals: HashMap::new(),
            named_locals: HashMap::new(),
        }
    }
}

pub struct CallStack {
    data: Vec<CallContext>,
}

impl CallStack {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    pub fn push(&mut self, context: CallContext) {
        self.data.push(context);
    }

    pub fn pop(&mut self) -> Option<CallContext> {
        self.data.pop()
    }

    pub fn current(&mut self) -> Result<&CallContext> {
        self.data
            .last()
            .ok_or_else(|| RuntimeError::new("expected call context".to_string()))
    }

    pub fn current_mut(&mut self) -> Result<&mut CallContext> {
        self.data
            .last_mut()
            .ok_or_else(|| RuntimeError::new("expected call context".to_string()))
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn last(&self) -> Option<&CallContext> {
        self.data.last()
    }

    pub fn rewind(&mut self) -> Vec<Trace> {
        let mut stack_trace = vec![];

        while !self.data.is_empty() {
            let call_context = self.data.remove(0);

            stack_trace.push(Trace {
                pos: call_context.pos.clone(),
                func: call_context.id,
            });
        }

        stack_trace
    }
}
