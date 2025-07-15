use std::{fs, path::Path};

mod bytecode;
mod compiler;
pub mod compiler2;

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

pub fn compile2(
    ctx: &mut GlobalContext,
    source: impl AsRef<Path>,
) -> Result<Package, crate::error::Error> {
    let source = fs::read_to_string(source)?;
    let program = frontend::compile(ctx, &source)?;
    let compiler = compiler2::Compiler::new(ctx);

    Ok(compiler.compile(program)?)
}
