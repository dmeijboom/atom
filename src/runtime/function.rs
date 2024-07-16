use std::{fmt::Debug, rc::Rc};

use safe_gc::Heap;

use crate::opcode::Opcode;

use super::{error::Error, std::FnHandler, value::Value};

pub enum Exec {
    Vm(Rc<[Opcode]>),
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

    pub fn with_handler<F>(name: String, arg_count: usize, handler: F) -> Self
    where
        F: Fn(&mut Heap, Vec<Value>) -> Result<Value, Error> + 'static,
    {
        Self {
            name,
            arg_count,
            exec: Exec::Handler(Box::new(handler)),
        }
    }

    pub fn codes(&self) -> Rc<[Opcode]> {
        match &self.exec {
            Exec::Vm(codes) => Rc::clone(codes),
            Exec::Handler(_) => Rc::new([]),
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
