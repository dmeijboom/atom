pub use compiler::Compiler;
pub use module::{Class, Func, FuncArg, Interface, Module};
pub use result::{CompileError, Result};

mod compiler;
mod module;
mod optimizers;
mod result;
mod scope;
