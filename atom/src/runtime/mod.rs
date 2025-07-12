use crate::frontend::Span;

mod builtins;
pub mod errors;
mod gc;
mod module;
pub mod ops;
mod types;
mod vm;

pub use builtins::{BuiltinFunction, Fn0, Fn1, Fn2, Fn3, Fn4};
pub use gc::{DynHandle, Gc, Handle, Trace};
pub use module::{Metadata, Module};
pub use types::*;
pub use vm::{Builtins, Error, Vm};

pub type Result<T> = std::result::Result<T, errors::RuntimeError>;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Context {
    pub module: usize,
}

impl Context {
    pub fn with_module(module: usize) -> Self {
        Self { module }
    }
}

pub trait Runtime<'gc> {
    fn span(&self) -> Span;
    fn get_atom(&self, idx: u32) -> &str;
    fn get_module(&self, idx: usize) -> &Metadata;
}
