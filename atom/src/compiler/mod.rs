pub use compiler::Compiler;
pub use line_number_offset::LineNumberOffset;
pub use module::{Class, Func, FuncArg, Interface, Module};
pub use result::{CompileError, Result};
pub use types::Type;

//mod backend;
mod compiler;
mod filesystem;
mod frontend;
mod line_number_offset;
mod mir;
mod module;
mod optimizers;
mod result;
mod types;
