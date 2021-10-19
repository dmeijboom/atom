pub use compiler::{parse_line_numbers_offset, Compiler};
pub use module::{Class, Func, FuncArg, Interface, Module};
pub use result::{CompileError, Result};
pub use types::Type;

mod compiler;
mod filesystem;
mod module;
mod optimizers;
mod result;
mod scope;
mod type_checker;
mod types;
