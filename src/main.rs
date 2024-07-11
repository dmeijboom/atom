use std::fs;

use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;
use vm::Vm;

mod ast;
mod codes;
mod compiler;
mod lexer;
mod parser;
mod runtime;
mod vm;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("I/O error: {0}")]
    IO(#[from] std::io::Error),
    #[error("ParseError: {0}")]
    Parse(#[from] parser::Error),
    #[error("LexError: {0}")]
    Lex(#[from] lexer::Error),
    #[error("RuntimeError: {0}")]
    Runtime(#[from] runtime::Error),
}

fn main() -> Result<(), Error> {
    let source = fs::read_to_string("main.atom")?;
    let chars = source.chars().collect::<Vec<_>>();

    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;

    let parser = Parser::new(tokens);
    let stmts = parser.parse()?;

    let compiler = Compiler::new(stmts);
    let module = compiler.compile();

    for code in module.codes.iter() {
        println!("{}: {:?}", code.span.offset, code.op);
    }

    let vm = Vm::new(module);
    let value = vm.run()?;

    if let Some(value) = value {
        println!("\n{} ({})", value, value.ty());
    }

    Ok(())
}
