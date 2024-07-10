use std::fs;

use lexer::Lexer;
use parser::Parser;

mod ast;
mod lexer;
mod parser;

fn main() {
    let source = fs::read_to_string("main.atom").expect("failed to open main.atom");
    let chars = source.chars().collect::<Vec<_>>();

    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex().expect("lexer failed");

    let parser = Parser::new(tokens);
    let stmts = parser.parse().expect("parser failed");

    println!("{:#?}", stmts);
}
