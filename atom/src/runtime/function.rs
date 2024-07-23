use std::{fmt::Debug, rc::Rc};

use crate::opcode::Opcode;

use super::{
    error::Error,
    std::{Context, FnHandler},
    value::{Type, Value},
};

pub enum Exec {
    Handler(Box<FnHandler>),
    Vm(Rc<[Opcode]>),
}

impl Default for Exec {
    fn default() -> Self {
        Self::Vm(Rc::new([]))
    }
}

pub enum Receiver {
    Type(Type),
    Class,
}

#[derive(Default)]
pub struct Func {
    pub name: String,
    pub exec: Exec,
    pub arg_count: usize,
    pub receiver: Option<Receiver>,
}

impl Func {
    pub fn new(name: String, arg_count: usize) -> Self {
        Self {
            name,
            arg_count,
            exec: Exec::default(),
            receiver: None,
        }
    }

    pub fn with_handler<F>(name: String, arg_count: usize, handler: F) -> Self
    where
        F: Fn(Context<'_>, Vec<Value>) -> Result<Value, Error> + 'static,
    {
        Self {
            name,
            arg_count,
            receiver: None,
            exec: Exec::Handler(Box::new(handler)),
        }
    }

    pub fn with_receiver(mut self, receiver: Receiver) -> Self {
        self.receiver = Some(receiver);
        self
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
