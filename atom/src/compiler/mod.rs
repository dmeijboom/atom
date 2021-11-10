pub use compiler::Compiler;
pub use line_number_offset::LineNumberOffset;
pub use module::{Class, Element, ElementKind, FuncArg, Function, Interface, Module};
pub use result::{CompileError, Result};

mod codegen;
mod compiler;
mod filesystem;
mod frontend;
mod line_number_offset;
mod mir;
mod module;
mod optimizers;
mod result;
mod slugs;
