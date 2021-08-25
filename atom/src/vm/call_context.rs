use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use wyhash2::WyHash;

use atom_runtime::{AtomRef, Fn, Method, Origin, Result, RuntimeError, Trace, Value};

pub enum Target {
    Fn(AtomRef<Fn>),
    Method(AtomRef<Method>),
}

impl Target {
    pub fn origin(&self) -> &Origin {
        match self {
            Target::Fn(func) => &func.origin,
            Target::Method(method) => &method.func.origin,
        }
    }
}

impl Display for Target {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Target::Fn(func) => write!(f, "{}", func.as_ref()),
            Target::Method(method) => write!(f, "{}", method.as_ref()),
        }
    }
}

impl Clone for Target {
    fn clone(&self) -> Self {
        match self {
            Target::Fn(func) => Target::Fn(AtomRef::clone(func)),
            Target::Method(method) => Target::Method(AtomRef::clone(method)),
        }
    }
}

pub struct CallContext {
    pub target: Target,
    pub receiver: Option<Value>,
    pub locals: Vec<Value>,
    pub named_locals: HashMap<String, Value, WyHash>,
}

impl CallContext {
    pub fn new(target: Target, receiver: Option<Value>, locals: Vec<Value>) -> Self {
        Self {
            target,
            receiver,
            locals,
            named_locals: HashMap::with_hasher(WyHash::with_seed(0)),
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

    pub fn rewind(&mut self) -> Vec<Trace> {
        let mut stack_trace = vec![];

        while !self.data.is_empty() {
            let call_context = self.data.remove(0);

            stack_trace.push(Trace {
                origin: call_context.target.origin().clone(),
                target: format!("{}", call_context.target),
            });
        }

        stack_trace
    }
}
