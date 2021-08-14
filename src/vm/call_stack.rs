use std::collections::HashMap;

use smallvec::SmallVec;

use crate::ast::Pos;
use crate::runtime::{Result, RuntimeError, Trace, TypeId, Value};
use crate::vm::stacked::Stacked;
use crate::vm::ModuleCache;

pub struct CallContext {
    pub pos: Pos,
    pub target: TypeId,
    pub return_value: Option<Value>,
    pub locals: SmallVec<[Stacked; 2]>,
    pub named_locals: HashMap<String, Stacked>,
}

impl CallContext {
    pub fn new_with_locals(pos: Pos, target: TypeId, capacity: usize) -> Self {
        Self {
            pos,
            target,
            return_value: None,
            named_locals: HashMap::new(),
            locals: SmallVec::with_capacity(capacity),
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
                    Some(_) => module_cache.fmt_method(&call_context.target),
                    None => module_cache.fmt_func(&call_context.target),
                },
            });
        }

        stack_trace
    }
}
