use std::fmt::{Display, Formatter};

use crate::runtime::{AtomRef, Closure, Fn, Method, Origin, Receiver, Trace, Value};

#[derive(Debug)]
pub enum Target {
    Fn(AtomRef<Fn>),
    Method(AtomRef<Method>),
    Closure(AtomRef<Closure>),
}

impl Target {
    pub fn origin(&self) -> &Origin {
        match self {
            Self::Fn(func) => &func.origin,
            Self::Closure(closure) => &closure.func.origin,
            Self::Method(method) => &method.func.origin,
        }
    }

    pub fn function(&self) -> &AtomRef<Fn> {
        match &self {
            Target::Fn(func) => func,
            Target::Method(method) => &method.func,
            Target::Closure(closure) => &closure.func,
        }
    }
}

impl Display for Target {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fn(func) => write!(f, "{}", func.as_ref()),
            Self::Closure(closure) => write!(f, "{}", closure.func.as_ref()),
            Self::Method(method) => write!(f, "{}", method.as_ref()),
        }
    }
}

impl Clone for Target {
    fn clone(&self) -> Self {
        match self {
            Self::Fn(func) => Target::Fn(AtomRef::clone(func)),
            Self::Closure(closure) => Target::Closure(AtomRef::clone(closure)),
            Self::Method(method) => Target::Method(AtomRef::clone(method)),
        }
    }
}

#[derive(Debug)]
pub struct StackFrame {
    pub target: Target,
    pub function: AtomRef<Fn>,
    pub module_id: usize,
    pub locals: Vec<Value>,
    // Contains a stack of addresses to return to after finishing this call (used for tail calls)
    pub return_addr: Vec<usize>,
    // Controls the current position in the execution flow
    pub position: usize,
    // Determines if the result should be stored on the stack or not
    pub store_return_value: bool,
}

impl StackFrame {
    pub fn new(target: Target, store_return_value: bool, locals: Vec<Value>) -> Self {
        let function = AtomRef::clone(target.function());

        Self {
            module_id: function.origin.module_id,
            function,
            target,
            position: 0,
            store_return_value,
            locals,
            return_addr: vec![],
        }
    }

    pub fn get_receiver(&self) -> Option<&Value> {
        if let Target::Method(method) = &self.target {
            if let Receiver::Bound(receiver) = &method.receiver {
                return Some(receiver);
            }
        }

        None
    }
}

pub struct CallStack {
    // Should always default to `data.len() - 1` but it's faster to store it
    data: Vec<StackFrame>,
}

impl CallStack {
    pub fn new() -> Self {
        Self { data: vec![] }
    }

    pub fn push(&mut self, frame: StackFrame) {
        self.data.push(frame);
    }

    pub fn pop(&mut self) -> Option<StackFrame> {
        self.data.pop()
    }

    pub fn current_id(&self) -> usize {
        self.data.len() - 1
    }

    pub fn current(&self) -> &StackFrame {
        let index = self.data.len() - 1;

        &self.data[index]
    }

    pub fn current_mut(&mut self) -> &mut StackFrame {
        let index = self.data.len() - 1;

        &mut self.data[index]
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn last(&self) -> Option<&StackFrame> {
        self.data.last()
    }

    pub fn rewind(&mut self) -> Vec<Trace> {
        let mut stack_trace = vec![];

        while !self.data.is_empty() {
            let frame = self.data.remove(0);

            stack_trace.push(Trace {
                origin: frame.target.origin().clone(),
                target: format!("{}", frame.target),
            });
        }

        stack_trace
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }
}
