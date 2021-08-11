use std::collections::HashMap;

use crate::ast::Pos;
use crate::runtime::{Result, RuntimeError, Trace, TypeId, Value};
use crate::vm::stacked::Stacked;
use crate::vm::ModuleCache;

pub struct CallContext {
    pub pos: Pos,
    pub finished: bool,
    pub target: TypeId,
    pub return_value: Option<Value>,
    pub locals: Vec<Stacked>,
    pub named_locals: HashMap<String, Stacked>,
}

impl CallContext {
    pub fn new(pos: Pos, target: TypeId) -> Self {
        Self {
            pos,
            target,
            locals: vec![],
            finished: false,
            return_value: None,
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

    pub fn rewind(&mut self, module_cache: &ModuleCache) -> Vec<Trace> {
        let mut stack_trace = vec![];

        while !self.data.is_empty() {
            let call_context = self.data.remove(0);

            stack_trace.push(Trace {
                pos: call_context.pos.clone(),
                target: match call_context.target.class {
                    Some(_) => module_cache.fmt_class(&call_context.target),
                    None => module_cache.fmt_func(&call_context.target),
                },
            });
        }

        stack_trace
    }
}
