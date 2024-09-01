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
use opcode::Opcode;
use runtime::{function::Fn, module::Module, value::Value};
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

const MAX_STACK_SIZE: usize = 250000 / size_of::<Value>();
const MAX_CONST_SIZE: usize = 100000 / size_of::<Value>();

type AtomVm<L> = Vm<L, MAX_STACK_SIZE, MAX_CONST_SIZE>;

#[derive(Parser)]
struct Opts {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Parser)]
enum Cmd {
    Run {
        source: PathBuf,
        #[clap(long)]
        no_optimize: bool,
    },
    Compile {
        source: PathBuf,
        #[clap(long)]
        no_optimize: bool,
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

fn compile(source: impl AsRef<Path>, optimize: bool) -> Result<Module, Error> {
    let mut program = parse(PRELUDE_SOURCE)?;
    let source = fs::read_to_string(source)?;
    program.extend(parse(&source)?);
    let compiler = Compiler::default().with_optimize(optimize);

    Ok(compiler.compile(program)?)
}

fn print_opcode(i: usize, opcode: &Opcode, indent: usize) {
    let mut prefix = format!("{}{i}:", " ".repeat(indent * 2));

    while prefix.len() < 4 + (indent * 2) {
        prefix.push(' ');
    }

    println!("{prefix}{opcode}");
}

fn print_func(f: &Fn, indent: usize) {
    let prefix = " ".repeat(indent * 2);
    println!("{prefix}fn {}:", f.name);

    for (i, opcode) in f.body.chunks_exact(16).map(Opcode::deserialize).enumerate() {
        print_opcode(i, &opcode, indent + 1);
    }
}

fn print_module(module: &Module) {
    for class in module.classes.iter().filter(|c| !c.is_extern()) {
        println!("class {}:", class.name);

        for (i, f) in class.methods.values().enumerate() {
            if i > 0 {
                println!();
            }

            print_func(f, 1);
        }

        println!();
    }

    for f in module.functions.iter().filter(|f| !f.is_extern()) {
        print_func(f, 0);
        println!();
    }

    println!("main:");

    for (i, opcode) in module
        .body
        .chunks_exact(16)
        .map(Opcode::deserialize)
        .enumerate()
    {
        print_opcode(i, &opcode, 1);
    }
}

fn cmd(opts: Opts) -> Result<(), Error> {
    match opts.cmd {
        Cmd::Run {
            source,
            no_optimize,
        } => {
            let module = compile(source, !no_optimize)?;
            let mut vm = AtomVm::with_module(module, runtime::linker())?;
            vm.run()?;

            #[cfg(feature = "timings")]
            {
                println!();
                println!("[Timings]");

                for (op, timing) in vm.timing() {
                    println!(
                        "{:?} took {:.2?} ({:.2?}/op, {}x)",
                        op,
                        timing.elapsed,
                        timing.avg(),
                        timing.count,
                    );
                }
            }
        }
        Cmd::Compile {
            source,
            verbose,
            no_optimize,
        } => {
            let module = compile(source, !no_optimize)?;

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
