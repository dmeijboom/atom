use std::{fs, path::Path};

use crate::{ast::Stmt, compiler::Compiler, lexer::Lexer, parser, runtime::Package, Error};

const PRELUDE_SOURCE: &str = include_str!("../std/prelude.atom");

fn parse(source: &str) -> Result<Vec<Stmt>, Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;
    let parser = parser::Parser::new(tokens);

    Ok(parser.parse()?)
}

pub fn compile(source: impl AsRef<Path>, optimize: bool) -> Result<Package, Error> {
    let mut program = parse(PRELUDE_SOURCE)?;
    let source = fs::read_to_string(source)?;
    program.extend(parse(&source)?);
    let compiler = Compiler::default().with_optimize(optimize);

    Ok(compiler.compile(program)?)
}
