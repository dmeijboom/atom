use error::RuntimeError;

use crate::{frame::Frame, module::Metadata};

pub mod array;
pub mod bigint;
pub mod class;
pub mod error;
pub mod function;
pub mod object;
pub mod str;
pub mod value;

pub use array::Array;
pub use bigint::BigInt;
pub use class::Class;
pub use function::{Fn, Method};
pub use object::Object;
pub use str::Str;
pub use value::{IntoAtom, Value};

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, Default, Clone)]
pub struct Context {
    pub module: usize,
}

impl Context {
    pub fn with_module(module: usize) -> Self {
        Self { module }
    }
}

pub trait Runtime {
    fn frame(&self) -> &Frame;
    fn get_atom(&self, idx: u32) -> &str;
    fn get_module(&self, idx: usize) -> &Metadata;
}
