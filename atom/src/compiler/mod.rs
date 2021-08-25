pub use compiler::{CompileError, Compiler};
pub use module::{Class, Func, FuncArg, Interface, Module};

mod compiler;
mod module;
mod optimizers;
mod scope;
