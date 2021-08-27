pub use compiler::Compiler;
pub use module::{Class, Func, FuncArg, Interface, Module, Type, TypeKind};
pub use result::{CompileError, Result};

mod compiler;
mod filesystem;
mod module;
mod optimizers;
mod path_finder;
mod result;
mod scope;
