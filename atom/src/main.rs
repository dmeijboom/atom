use std::{
    fs,
    path::{Path, PathBuf},
    process::exit,
};

use ast::Stmt;
use clap::Parser;
use error::Error;
#[cfg(feature = "mimalloc")]
use mimalloc::MiMalloc;
use opcode::{Op, Opcode};
use runtime::{func::Func, module::Module};
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

const PRELUDE_SOURCE: &str = include_str!("../std/prelude.atom");

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

fn parse(source: &str) -> Result<Vec<Stmt>, Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;
    let parser = parser::Parser::new(tokens);

    Ok(parser.parse()?)
}

fn compile(source: impl AsRef<Path>) -> Result<Module, Error> {
    let mut program = parse(PRELUDE_SOURCE)?;
    let source = fs::read_to_string(source)?;
    program.extend(parse(&source)?);
    let compiler = Compiler::new();

    Ok(compiler.compile(program)?)
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
        | Op::CallExtern
        | Op::TailCall
        | Op::LoadElement
        | Op::LoadMember
        | Op::LoadArg => println!("{prefix} {:?} {}", opcode.op(), opcode.code()),
        _ => println!("{prefix} {:?}", opcode.op(),),
    }
}

fn print_func(func: &Func, indent: usize) {
    let prefix = " ".repeat(indent * 2);
    println!("{prefix}fn {}:", func.name);

    for (i, opcode) in func.codes.iter().enumerate() {
        print_opcode(i, opcode, indent + 1);
    }
}

fn print_module(module: &Module) {
    for class in module.classes.iter() {
        println!("class {}:", class.name);

        for (i, func) in class.methods.values().enumerate() {
            if i > 0 {
                println!();
            }

            print_func(func, 1);
        }

        println!();
    }

    for func in module.funcs.iter() {
        print_func(func, 0);
        println!();
    }

    println!("main:");

    for (i, opcode) in module.codes.iter().enumerate() {
        print_opcode(i, opcode, 1);
    }
}

fn cmd(opts: Opts) -> Result<(), Error> {
    match opts.cmd {
        Cmd::Run { source } => {
            let module = compile(source)?;
            let mut vm = Vm::new(module, runtime::linker())?;
            vm.run()?;
        }
        Cmd::Compile { source, verbose } => {
            let module = compile(source)?;

            if verbose {
                print_module(&module);
            }
        }
    }

    Ok(())
}

fn main() {
    #[cfg(feature = "tracing")]
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let opts = Opts::parse();

    if let Err(e) = cmd(opts) {
        eprintln!("{e} at {}", e.span());

        if let Error::Runtime(vm::Error::Runtime(e)) = e {
            if let Some(trace) = e.trace {
                for call in trace {
                    eprintln!("  in {} at {}", call, call.span);
                }
            }
        }

        exit(1);
    }
}
