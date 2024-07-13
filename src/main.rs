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
    #[error("TokenError: {0}")]
    Lex(#[from] lexer::Error),
    #[error("CompileError: {0}")]
    Compile(#[from] compiler::Error),
    #[error("{0}")]
    Runtime(#[from] vm::Error),
}

fn main() -> Result<(), Error> {
    let source = fs::read_to_string("main.atom")?;
    let chars = source.chars().collect::<Vec<_>>();

    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;

    let parser = Parser::new(tokens);
    let stmts = parser.parse()?;

    let compiler = Compiler::new(stmts);
    let module = compiler.compile()?;

    for (name, func) in module.funcs.iter() {
        println!("{}:", name);

        for code in func.codes.iter() {
            println!("{}: {:?}", code.span.offset, code.op);
        }
    }

    if !module.funcs.is_empty() {
        println!();
    }

    for code in module.codes.iter() {
        println!("{}: {:?}", code.span.offset, code.op);
    }

    let mut vm = Vm::new(module);
    let value = vm.run()?;

    if let Some(value) = value {
        println!("\n{} ({})", vm.repr(&value)?, value.ty());
    }

    Ok(())
}
