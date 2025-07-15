pub mod ast;
mod ir;
mod lexer;
mod parser;

pub use ir::{
    Block, IRClass, IRError, IRFn, IRNode, IRValue, Loop, NodeKind, Program, VariableKind, IR,
};
pub use lexer::{Lexer, Span, Spanned, TokenError};
pub use parser::{ParseError, Parser};

use ast::Stmt;

use crate::backend::GlobalContext;

pub fn parse(source: &str) -> Result<Vec<Stmt>, crate::error::Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;
    let parser = Parser::new(tokens);

    Ok(parser.parse()?)
}

pub fn compile(ctx: &mut GlobalContext, source: &str) -> Result<Program, crate::error::Error> {
    let tree = parse(source)?;
    let program = IR::new(ctx).compile(tree)?;

    Ok(program)
}
