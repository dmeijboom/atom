use ::std::{borrow::Cow, collections::HashMap, rc::Rc};

use bytes::Bytes;
use wyhash2::WyHash;

use class::Class;
use error::{ErrorKind, RuntimeError};
use value::Value;

use crate::{
    gc::Gc,
    lexer::Span,
    opcode::Const,
    vm::{BoxedFn, DynamicLinker},
};

pub mod array;
pub mod class;
pub mod core;
pub mod error;
pub mod function;
pub mod str;
pub mod value;

pub type Name = Cow<'static, str>;

#[derive(Debug, Default)]
pub struct Module {
    pub body: Bytes,
    pub consts: Vec<Const>,
    pub functions: Vec<function::Fn>,
    pub classes: Vec<Class>,
}

pub struct Api<'a> {
    gc: &'a mut Gc,
    span: Span,
    receiver: Option<Value>,
}

impl<'a> Api<'a> {
    pub fn new(gc: &'a mut Gc) -> Self {
        Self {
            gc,
            span: Span::default(),
            receiver: None,
        }
    }

    pub fn gc(&mut self) -> &mut Gc {
        self.gc
    }

    pub fn receiver(&self) -> Result<Value, RuntimeError> {
        self.receiver
            .ok_or_else(|| ErrorKind::NoReceiver.at(self.span))
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_receiver(mut self, receiver: Value) -> Self {
        self.receiver = Some(receiver);
        self
    }
}

#[derive(Default)]
pub struct Lib {
    class_name: Option<&'static str>,
    funcs: HashMap<String, BoxedFn, WyHash>,
}

impl Lib {
    pub fn with(self, f: impl FnOnce(Self) -> Self) -> Self {
        f(self)
    }

    pub fn set<F>(mut self, name: &str, func: F) -> Self
    where
        F: Fn(Api, Vec<Value>) -> Result<Value, RuntimeError> + 'static,
    {
        let name = match self.class_name {
            Some(class_name) => format!("{}.{}", class_name, name),
            None => name.to_string(),
        };

        self.funcs.insert(name, Rc::new(Box::new(func)));
        self
    }

    pub fn class(mut self, class_name: &'static str, f: impl FnOnce(Self) -> Self) -> Self {
        self.class_name = Some(class_name);
        let mut lib = f(self);
        lib.class_name = None;
        lib
    }
}

impl DynamicLinker for Lib {
    fn resolve(&self, name: &str) -> Option<BoxedFn> {
        self.funcs.get(name).cloned()
    }
}

pub fn linker() -> Lib {
    Lib::default()
        .set("println", core::atom_println)
        .set("repr", core::atom_repr)
        .with(array::register)
        .with(str::register)
}
