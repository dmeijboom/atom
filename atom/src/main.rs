use std::{
    fs,
    path::{Path, PathBuf},
    process::exit,
};

use argh::FromArgs;
use ast::Stmt;
use bytecode::{Bytecode, Serializable, Spanned};
use error::Error;
#[cfg(feature = "mimalloc")]
use mimalloc::MiMalloc;
use runtime::{function::Fn, value::Value, Module, Runtime};
#[cfg(feature = "tracing")]
use tracing_subscriber::EnvFilter;

use compiler::Compiler;
use lexer::Lexer;
use vm::Vm;

mod ast;
mod bytecode;
mod compiler;
mod error;
mod gc;
mod instance;
mod lexer;
mod parser;
#[cfg(feature = "profiler")]
mod profiler;
mod runtime;
mod stack;
mod vm;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

const PRELUDE_SOURCE: &str = include_str!("../std/prelude.atom");

const MAX_STACK_SIZE: usize = 250000 / size_of::<Value>();
const MAX_CONST_SIZE: usize = 100000 / size_of::<Value>();

type AtomVm<L> = Vm<L, MAX_STACK_SIZE, MAX_CONST_SIZE>;

/// CLI options
#[derive(FromArgs)]
struct Opts {
    #[argh(subcommand)]
    cmd: Cmd,
}

/// Command
#[derive(FromArgs)]
#[argh(subcommand)]
enum Cmd {
    Run(RunCmd),
    Compile(CompileCmd),
}

/// Run a program
#[derive(FromArgs)]
#[argh(subcommand, name = "run")]
struct RunCmd {
    #[argh(positional)]
    source: PathBuf,
    #[argh(switch, description = "disable optimizations")]
    no_optimize: bool,
}

/// Compile a program
#[derive(FromArgs)]
#[argh(subcommand, name = "compile")]
struct CompileCmd {
    #[argh(positional)]
    source: PathBuf,
    #[argh(switch, description = "disable optimizations")]
    no_optimize: bool,
    #[argh(switch, description = "show verbose output")]
    verbose: bool,
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

fn print_opcode(i: usize, opcode: &Bytecode, indent: usize) {
    let mut prefix = format!("{}{i}:", " ".repeat(indent * 2));

    while prefix.len() < 4 + (indent * 2) {
        prefix.push(' ');
    }

    println!("{prefix}{opcode}");
}

fn print_func(f: &Fn, indent: usize) {
    let prefix = " ".repeat(indent * 2);
    println!("{prefix}fn {}:", f.name);

    for (i, opcode) in f
        .body
        .chunks_exact(8)
        .map(|mut chunk| Spanned::<Bytecode>::deserialize(&mut chunk))
        .enumerate()
    {
        print_opcode(i, &opcode, indent + 1);
    }
}

fn print_module(module: &Module) {
    for class in module.classes.iter() {
        println!("class {}:", class.name);

        for (i, f) in class.methods.values().enumerate() {
            if i > 0 {
                println!();
            }

            print_func(f, 1);
        }

        println!();
    }

    for f in module.functions.iter() {
        print_func(f, 0);
        println!();
    }

    println!("main:");

    for (i, opcode) in module
        .body
        .chunks_exact(8)
        .map(|mut chunk| Spanned::<Bytecode>::deserialize(&mut chunk))
        .enumerate()
    {
        print_opcode(i, &opcode, 1);
    }
}

#[cfg(feature = "profiler")]
macro_rules! format_col {
    ($max_len:expr, $($arg:tt)*) => {{
        let col = std::fmt::format(format_args!($($arg)*));
        format!("{}{}", col, " ".repeat($max_len - col.len()))
    }};
}

fn cmd(opts: Opts) -> Result<(), Error> {
    match opts.cmd {
        Cmd::Run(RunCmd {
            source,
            no_optimize,
        }) => {
            let module = compile(source, !no_optimize)?;
            let mut vm = AtomVm::with(module, Runtime::default())?;
            vm.run()?;

            #[cfg(feature = "profiler")]
            {
                let report = vm.profiler().report();

                println!("\n-- REPORT (took {:?}) --", report.exec_time);

                for record in report.records {
                    println!(
                        "{} {}",
                        format_col!(15, "{:?}", record.op),
                        format_col!(15, "{} calls", record.call_count),
                    );
                }
            }
        }
        Cmd::Compile(CompileCmd {
            source,
            verbose,
            no_optimize,
        }) => {
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

    let opts = argh::from_env();

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
