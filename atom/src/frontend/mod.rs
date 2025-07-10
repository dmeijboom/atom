pub mod ast;
mod lexer;
mod parser;

use ast::Stmt;
pub use lexer::{Lexer, Span, Spanned, TokenError};
pub use parser::{ParseError, Parser};

pub fn parse(source: &str) -> Result<Vec<Stmt>, crate::error::Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;
    let parser = Parser::new(tokens);

    Ok(parser.parse()?)
}
