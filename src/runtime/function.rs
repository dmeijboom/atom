use std::{fmt::Debug, rc::Rc};

use safe_gc::Heap;
use tinyvec::TinyVec;

use crate::codes::Code;

use super::{error::Error, std::FnHandler, value::Value};

pub enum Exec {
    Vm(Rc<[Code]>),
    Handler(Box<FnHandler>),
}

impl Default for Exec {
    fn default() -> Self {
        Self::Vm(Rc::new([]))
    }
}

#[derive(Default)]
pub struct Func {
    pub name: String,
    pub exec: Exec,
    pub arg_count: usize,
}

impl Func {
    pub fn new(name: String, arg_count: usize) -> Self {
        Self {
            name,
            arg_count,
            exec: Exec::default(),
        }
    }

    pub fn with_codes(name: String, arg_count: usize, codes: Vec<Code>) -> Self {
        Self {
            name,
            arg_count,
            exec: Exec::Vm(codes.into()),
        }
    }

    pub fn with_handler<F>(name: String, arg_count: usize, handler: F) -> Self
    where
        F: Fn(&mut Heap, TinyVec<[Value; 8]>) -> Result<Value, Error> + 'static,
    {
        Self {
            name,
            arg_count,
            exec: Exec::Handler(Box::new(handler)),
        }
    }
}

impl Debug for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Func")
            .field("name", &self.name)
            .field("arg_count", &self.arg_count)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Cursor {
    pub pos: usize,
    codes: Rc<[Code]>,
}

impl Cursor {
    pub fn new(codes: Rc<[Code]>) -> Self {
        Self { pos: 0, codes }
    }

    pub fn next(&mut self) -> Option<&Code> {
        let code = self.codes.get(self.pos);
        self.pos += 1;
        code
    }

    pub fn cur(&self) -> Option<&Code> {
        self.codes.get(self.pos)
    }

    pub fn goto(&mut self, n: usize) {
        self.pos = n;
    }

    pub fn goto_end(&mut self) {
        self.pos = self.codes.len();
    }
}
