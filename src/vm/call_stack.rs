use std::fmt::{Display, Formatter};

use crate::compiler::ir::Code;
use crate::runtime::{
    AtomRef, Closure, ErrorKind, Fn, FnPtr, Method, Origin, Receiver, Result, RuntimeError, Trace,
    Value,
};

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
        Self {
            target,
            position: 0,
            store_return_value,
            locals,
            return_addr: vec![],
        }
    }

    pub fn get_function(&self) -> &AtomRef<Fn> {
        match &self.target {
            Target::Fn(func) => func,
            Target::Method(method) => &method.func,
            Target::Closure(closure) => &closure.func,
        }
    }

    pub fn get_current_instruction(&self) -> Option<&Code> {
        match &self.get_function().ptr {
            FnPtr::Native(ir) => ir.get(self.position),
            _ => unreachable!(),
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

    pub fn current_id(&self) -> Option<usize> {
        if self.data.is_empty() {
            return None;
        }

        Some(self.data.len() - 1)
    }

    pub fn current(&self) -> Result<&StackFrame> {
        self.data.last().ok_or_else(|| {
            RuntimeError::new(ErrorKind::FatalError, "expected call context".to_string())
        })
    }

    pub fn current_mut(&mut self) -> Result<&mut StackFrame> {
        self.data.last_mut().ok_or_else(|| {
            RuntimeError::new(ErrorKind::FatalError, "expected call context".to_string())
        })
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
            let call_context = self.data.remove(0);

            stack_trace.push(Trace {
                origin: call_context.target.origin().clone(),
                target: format!("{}", call_context.target),
            });
        }

        stack_trace
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }
}
