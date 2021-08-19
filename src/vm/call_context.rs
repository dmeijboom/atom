use std::collections::HashMap;

use wyhash2::WyHash;

use crate::ast::Pos;
use crate::runtime::{Result, RuntimeError, Trace, TypeId, Value};
use crate::vm::ModuleCache;

#[derive(Clone)]
pub struct Target {
    pub type_id: TypeId,
    pub module_id: usize,
    pub method_id: Option<usize>,
}

pub struct CallContext {
    pub pos: Pos,
    pub target: Target,
    pub locals: Vec<Value>,
    pub this: Option<Value>,
    pub named_locals: HashMap<String, Value, WyHash>,
}

impl CallContext {
    pub fn new(pos: Pos, target: Target, this: Option<Value>) -> Self {
        Self {
            pos,
            this,
            target,
            named_locals: HashMap::with_hasher(WyHash::with_seed(0)),
            locals: vec![],
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

    pub fn current(&self) -> Result<&CallContext> {
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
                target: match call_context.target.method_id {
                    Some(id) => {
                        if let Ok(class) = module_cache
                            .lookup_type_by_id(call_context.target.type_id)
                            .and_then(|t| t.try_as_class())
                        {
                            if let Some((method_name, _)) = class.methods.get_index(id) {
                                format!(
                                    "{}.{}",
                                    module_cache.fmt_type(call_context.target.type_id),
                                    method_name
                                )
                            } else {
                                "!".to_string()
                            }
                        } else {
                            "!".to_string()
                        }
                    }
                    None => module_cache.fmt_type(call_context.target.type_id),
                },
            });
        }

        stack_trace
    }
}
