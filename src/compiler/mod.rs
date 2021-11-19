pub use compiler::Compiler;
pub use line_number_offset::LineNumberOffset;
pub use module::{Class, Element, ElementKind, FuncArg, Function, FunctionAttr, Interface, Module};
pub use error::{CompileError, Result};

mod codegen;
mod compiler;
mod filesystem;
mod frontend;
pub mod ir;
mod line_number_offset;
mod mir;
mod module;
mod optimizers;
mod error;
mod slugs;
