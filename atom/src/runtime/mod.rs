use ::std::{borrow::Cow, collections::HashMap, rc::Rc};

use error::{ErrorKind, RuntimeError};
use value::Value;
use wyhash2::WyHash;

use crate::{
    gc::{Gc, Handle, Trace},
    lexer::Span,
    vm::{BoxedFn, DynamicLinker},
};

pub mod array;
pub mod class;
pub mod core;
pub mod error;
pub mod function;
pub mod module;
pub mod str;
pub mod value;

pub type Name = Cow<'static, str>;

pub struct Atom<'a> {
    gc: &'a mut Gc,
    span: Span,
    receiver: Option<Value>,
}

impl<'a> Atom<'a> {
    pub fn new(gc: &'a mut Gc) -> Self {
        Self {
            gc,
            span: Span::default(),
            receiver: None,
        }
    }

    pub fn span(&self) -> Span {
        self.span
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

    pub fn alloc<T: Trace + 'static>(&mut self, value: T) -> Result<Handle<T>, RuntimeError> {
        self.gc.alloc(value)
    }

    pub fn alloc_array<T: Trace + 'static>(
        &mut self,
        cap: usize,
    ) -> Result<Handle<T>, RuntimeError> {
        self.gc.alloc_array(cap)
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
        F: Fn(Atom, Vec<Value>) -> Result<Value, RuntimeError> + 'static,
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
