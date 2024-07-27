use std::{
    fs,
    path::{Path, PathBuf},
};

use clap::Parser;
use error::Error;
#[cfg(feature = "mimalloc")]
use mimalloc::MiMalloc;
use opcode::{Op, Opcode};
use runtime::{
    function::{Exec, Func},
    module::Module,
    std::prelude,
};
#[cfg(feature = "tracing")]
use tracing_subscriber::EnvFilter;

use compiler::Compiler;
use lexer::Lexer;
use vm::Vm;

mod ast;
mod collections;
mod compiler;
mod error;
mod gc;
mod lexer;
mod opcode;
mod parser;
mod runtime;
mod vm;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

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

fn compile(prelude: &Module, source: impl AsRef<Path>) -> Result<Module, Error> {
    let source = fs::read_to_string(source)?;
    let chars = source.chars().collect::<Vec<_>>();

    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;

    let parser = parser::Parser::new(tokens);
    let stmts = parser.parse()?;

    let compiler = Compiler::new(prelude);
    Ok(compiler.compile(stmts)?)
}

fn print_opcode(i: usize, opcode: &Opcode, indent: usize) {
    let mut prefix = format!("{}{i}:", " ".repeat(indent * 2));

    while prefix.len() < 4 + (indent * 2) {
        prefix.push(' ');
    }

    match opcode.op() {
        Op::Store
        | Op::Load
        | Op::LoadFunc
        | Op::LoadConst
        | Op::Jump
        | Op::JumpIfFalse
        | Op::PushJumpIfFalse
        | Op::PushJumpIfTrue
        | Op::MakeArray
        | Op::MakeSlice
        | Op::Call
        | Op::TailCall
        | Op::LoadElement
        | Op::LoadMember
        | Op::LoadArg => println!("{prefix} {:?} {}", opcode.op(), opcode.code()),
        _ => println!("{prefix} {:?}", opcode.op(),),
    }
}

fn print_func(func: &Func, indent: usize) {
    if let Exec::Vm(codes) = &func.exec {
        let prefix = " ".repeat(indent * 2);
        println!("{prefix}fn {}:", func.name);

        for (i, opcode) in codes.iter().enumerate() {
            print_opcode(i, opcode, indent + 1);
        }
    }
}

fn print_module(module: &Module) {
    for class in module.classes.iter() {
        if class.native() {
            continue;
        }

        println!("class {}:", class.name);

        for (i, func) in class.methods.values().enumerate() {
            if i > 0 {
                println!();
            }

            print_func(func, 1);
        }

        println!();
    }

    for func in module.funcs.iter().filter(|f| !f.native()) {
        print_func(func, 0);
        println!();
    }

    println!("main:");

    for (i, opcode) in module.codes.iter().enumerate() {
        print_opcode(i, opcode, 1);
    }
}

fn main() -> Result<(), Error> {
    #[cfg(feature = "tracing")]
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let opts = Opts::parse();
    let std = prelude();

    match opts.cmd {
        Cmd::Run { source } => {
            let module = compile(&std, source)?;
            let mut vm = Vm::new(module)?;
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
