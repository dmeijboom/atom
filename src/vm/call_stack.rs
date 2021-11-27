use std::fmt::{Display, Formatter};

use crate::recycle_vec::RecycleVec;
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

#[derive(Debug)]
pub struct StackFrame {
    pub target: Target,
    pub function: AtomRef<Fn>,
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

    pub fn reset(&mut self, target: Target, store_return_value: bool) {
        self.position = 0;
        self.function = AtomRef::clone(target.function());
        self.target = target;
        self.return_addr.truncate(0);
        self.store_return_value = store_return_value;
        self.locals.truncate(0);
    }
}

pub struct CallStack {
    data: RecycleVec<StackFrame>,
}

impl CallStack {
    pub fn new() -> Self {
        Self {
            data: RecycleVec::new(),
        }
    }

    pub fn recycle(&mut self) -> Option<StackFrame> {
        self.data.recycle()
    }

    #[inline]
    pub fn push(&mut self, frame: StackFrame) {
        self.data.push(frame);
    }

    #[inline]
    pub fn pop(&mut self) {
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

    #[inline]
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
}
