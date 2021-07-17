pub use compiler::{CompileError, Compiler};
pub use ir::IR;
pub use module::{Func, FuncArg, Module};

mod compiler;
mod ir;
mod module;
mod scope;

