use peg::error::ParseError;
use peg::str::LineCol;

use crate::ast::Pos;
use crate::compiler::{CompileError, Compiler, Module};
use crate::parser;
use crate::runtime::RuntimeError;

#[derive(Debug, PartialEq)]
pub enum Error {
    Compile(CompileError),
    Runtime(RuntimeError),
    ParseError(ParseError<LineCol>),
}

impl From<CompileError> for Error {
    fn from(e: CompileError) -> Self {
        Self::Compile(e)
    }
}

impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}

pub fn compile_module(source: &str) -> Result<Module, Error> {
    let tree = parser::parse(source).map_err(|e| Error::ParseError(e))?;
    let compiler = Compiler::new(tree);
    let module = compiler.compile()?;

    Ok(module)
}

pub fn parse_line_column(source: &str, pos: &Pos) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;
    let mut chars = source.chars();

    for _ in 0..pos.start {
        match chars.next() {
            Some('\n') => {
                line += 1;
                column = 1;
            }
            _ => column += 1,
        }
    }

    (line, column)
}
