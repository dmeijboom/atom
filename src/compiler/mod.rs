pub use compiler::{CompileError, Compiler};
pub use ir::{Code, LocalId, IR};
pub use module::{Class, Func, FuncArg, Interface, Module};

mod compiler;
mod ir;
mod module;
mod scope;
