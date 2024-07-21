use std::{
    fs,
    path::{Path, PathBuf},
};

use clap::Parser;
use opcode::{Op, Opcode};
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
mod collections;
mod compiler;
mod gc;
mod lexer;
mod opcode;
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

fn print_opcode(i: usize, opcode: &Opcode) {
    let mut prefix = format!("{i}:");

    while prefix.len() < 5 {
        prefix.push(' ');
    }

    match opcode.op() {
        Op::Store
        | Op::Load
        | Op::LoadFunc
        | Op::LoadNativeFunc
        | Op::LoadConst
        | Op::Jump
        | Op::JumpIfFalse
        | Op::PushJumpIfFalse
        | Op::PushJumpIfTrue
        | Op::MakeArray
        | Op::Call
        | Op::TailCall
        | Op::LoadElement
        | Op::LoadMember
        | Op::LoadArg => println!("{prefix} {:?} {}", opcode.op(), opcode.code()),
        Op::DirectCall => {
            let (hi, low) = opcode.code2();
            println!("{prefix} {:?} {} {}", opcode.op(), hi, low,)
        }
        _ => println!("{prefix} {:?}", opcode.op(),),
    }
}

fn print_module(module: &Module) {
    for (i, func) in module.funcs.iter().enumerate() {
        if i > 0 {
            println!();
        }

        println!("{}:", func.name);

        match &func.exec {
            Exec::Vm(codes) => {
                for (i, opcode) in codes.iter().enumerate() {
                    print_opcode(i, opcode);
                }
            }
            Exec::Handler(_) => println!("<native>"),
        }
    }

    if !module.funcs.is_empty() {
        println!();
    }

    for (i, opcode) in module.codes.iter().enumerate() {
        print_opcode(i, opcode);
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