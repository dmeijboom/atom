pub use module::{Class, Func, FuncArg, Interface, Module, Type, TypeKind};
pub use result::{CompileError, Result};
pub use utils::parse_line_numbers_offset;

//mod compiler;
pub mod compiler_v2;
mod module;
mod optimizers;
mod filesystem;
mod pre_processor;
mod result;
mod scope;
mod utils;
