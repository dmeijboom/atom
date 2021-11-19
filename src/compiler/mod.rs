pub use compiler::Compiler;
pub use error::{CompileError, Result};
pub use line_number_offset::LineNumberOffset;
pub use module::{Class, Element, ElementKind, FuncArg, Function, FunctionAttr, Interface, Module};

mod codegen;
mod compiler;
mod error;
mod filesystem;
mod frontend;
pub mod ir;
mod line_number_offset;
mod mir;
mod module;
mod optimizers;
mod slugs;
