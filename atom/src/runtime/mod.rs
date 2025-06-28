use error::RuntimeError;

use crate::{frame::Frame, module::Metadata};

pub mod array;
pub mod bigint;
pub mod class;
pub mod error;
pub mod function;
pub mod str;
pub mod value;

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub trait Runtime {
    fn frame(&self) -> &Frame;
    fn get_atom(&self, idx: u32) -> &str;
    fn get_module(&self, idx: usize) -> &Metadata;
}
