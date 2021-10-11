pub use compiler::{parse_line_numbers_offset, Compiler};
pub use module::{Class, Func, FuncArg, Interface, Module, Type, TypeKind};
pub use result::{CompileError, Result};

mod compiler;
mod filesystem;
mod module;
mod optimizers;
mod result;
mod scope;
