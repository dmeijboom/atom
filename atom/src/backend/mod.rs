use std::{fs, path::Path};

mod bytecode;
mod compiler;

pub use bytecode::{Bytecode, Const, Op, Serializable, BYTECODE_SIZE};
pub use compiler::{Compiler, GlobalContext, Package};

use crate::frontend;

pub fn compile(
    ctx: &mut GlobalContext,
    source: impl AsRef<Path>,
) -> Result<Package, crate::error::Error> {
    let source = fs::read_to_string(source)?;
    let program = frontend::compile(ctx, &source)?;
    let compiler = Compiler::new(ctx);

    Ok(compiler.compile(program))
}
