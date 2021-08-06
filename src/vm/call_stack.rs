use std::collections::HashMap;

use crate::ast::Pos;
use crate::compiler::LocalId;
use crate::runtime::{Result, RuntimeError, Trace, TypeId, Value};

pub struct CallContext {
    pub pos: Pos,
    pub id: TypeId,
    pub return_value: Option<Value>,
    pub args: HashMap<String, Value>,
    pub locals: HashMap<LocalId, Value>,
}

impl CallContext {
    pub(crate) fn new(pos: Pos, id: TypeId) -> Self {
        Self {
            id,
            pos,
            return_value: None,
            args: HashMap::new(),
            locals: HashMap::new(),
        }
    }
}

pub struct CallStack {
    data: Vec<CallContext>,
}

impl CallStack {
    pub(crate) fn new() -> Self {
        Self { data: vec![] }
    }

    pub(crate) fn push(&mut self, context: CallContext) {
        self.data.push(context);
    }

    pub(crate) fn pop(&mut self) -> Option<CallContext> {
        self.data.pop()
    }

    pub(crate) fn current(&self) -> Result<&CallContext> {
        self.data
            .last()
            .ok_or_else(|| RuntimeError::new("expected call context".to_string()))
    }

    pub(crate) fn current_mut(&mut self) -> Result<&mut CallContext> {
        self.data
            .last_mut()
            .ok_or_else(|| RuntimeError::new("expected call context".to_string()))
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub(crate) fn last(&self) -> Option<&CallContext> {
        self.data.last()
    }

    pub(crate) fn rewind(&mut self) -> Vec<Trace> {
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
