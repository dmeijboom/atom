use std::{
    fs,
    path::{Path, PathBuf},
};

use clap::Parser;
use runtime::{
    function::Exec,
    std::{stdlib, StdLib},
};
#[cfg(feature = "tracing")]
use tracing_subscriber::EnvFilter;

use compiler::{Compiler, Module};
use lexer::Lexer;
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

#[derive(Parser)]
struct Opts {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Parser)]
enum Cmd {
    Run {
        source: PathBuf,
    },
    Compile {
        source: PathBuf,

        #[clap(short, long)]
        verbose: bool,
    },
}

fn compile(std: &StdLib, source: impl AsRef<Path>) -> Result<Module, Error> {
    let source = fs::read_to_string(source)?;
    let chars = source.chars().collect::<Vec<_>>();

    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;

    let parser = parser::Parser::new(tokens);
    let stmts = parser.parse()?;

    let compiler = Compiler::new(std);
    Ok(compiler.compile(stmts)?)
}

fn print_module(module: &Module) {
    for (i, func) in module.funcs.iter().enumerate() {
        if i > 0 {
            println!();
        }

        println!("{}:", func.name);

        match &func.exec {
            Exec::Vm(codes) => {
                for code in codes.iter() {
                    println!("{}: {:?}", code.span.offset, code.op);
                }
            }
            Exec::Handler(_) => println!("<native>"),
        }
    }

    if !module.funcs.is_empty() {
        println!();
    }

    for code in module.codes.iter() {
        println!("{}: {:?}", code.span.offset, code.op);
    }
}

fn main() -> Result<(), Error> {
    #[cfg(feature = "tracing")]
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let opts = Opts::parse();
    let std = stdlib();

    match opts.cmd {
        Cmd::Run { source } => {
            let module = compile(&std, source)?;
            let mut vm = Vm::new(&std, module);
            vm.run()?;
        }
        Cmd::Compile { source, verbose } => {
            let module = compile(&std, source)?;

            if verbose {
                print_module(&module);
            }
        }
    }

    Ok(())
}
