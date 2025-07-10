use std::{fs, path::Path};

mod bytecode;
mod compiler;

pub use bytecode::{Bytecode, Const, Op, Serializable, BYTECODE_SIZE};
pub use compiler::{CompileError, Compiler, GlobalContext, Package};

use crate::frontend;

pub fn compile(
    source: impl AsRef<Path>,
    ctx: &mut GlobalContext,
) -> Result<Package, crate::error::Error> {
    let source = fs::read_to_string(source)?;
    let program = frontend::parse(&source)?;
    let compiler = Compiler::default();

    Ok(compiler.compile(ctx, program)?)
}
