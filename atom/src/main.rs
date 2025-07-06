use std::{fs, path::PathBuf, process::exit};

use argh::FromArgs;
use bytecode::{Bytecode, Serializable};
use compiler::{Compiler, GlobalContext, Package};
use error::Error;
use gc::Gc;
#[cfg(feature = "mimalloc")]
use mimalloc::MiMalloc;
use ron::ser::PrettyConfig;
use runtime::{Fn, Value};
#[cfg(feature = "tracing")]
use tracing_subscriber::EnvFilter;

use vm::{Builtins, Vm};

mod ast;
mod builtins;
mod bytecode;
mod collections;
mod compiler;
mod error;
mod frame;
mod gc;
mod lexer;
mod module;
mod parser;
#[cfg(feature = "profiler")]
mod profiler;
mod runtime;
mod vm;

#[cfg(feature = "mimalloc")]
#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

const MAX_STACK_SIZE: usize = 250000 / size_of::<Value>();

type AtomVm<'gc> = Vm<'gc, MAX_STACK_SIZE>;

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
}

/// Compile a program
#[derive(FromArgs)]
#[argh(subcommand, name = "compile")]
struct CompileCmd {
    #[argh(positional)]
    source: PathBuf,
    #[argh(switch, description = "show AST output")]
    ast: bool,
    #[argh(switch, description = "show bytecode output")]
    bytecode: bool,
}

fn print_opcode(i: usize, opcode: &Bytecode, indent: usize) {
    let mut prefix = format!("{}{i}:", " ".repeat(indent * 2));

    while prefix.len() < 6 + (indent * 2) {
        prefix.push(' ');
    }

    println!("{prefix}{opcode}");
}

fn print_func(f: &Fn, indent: usize) {
    let prefix = " ".repeat(indent * 2);
    println!("{prefix}fn {}:", f.name);

    for (i, opcode) in f
        .body
        .chunks_exact(bytecode::SIZE)
        .map(|mut chunk| Bytecode::deserialize(&mut chunk))
        .enumerate()
    {
        print_opcode(i, &opcode, indent + 1);
    }
}

fn print_atoms(ctx: GlobalContext) {
    println!("atoms:");

    for (n, name) in ctx.atoms.into_iter().enumerate() {
        println!("  {n}: {name}");
    }

    println!();
}

fn print_module(package: &Package) {
    for class in package.classes.iter() {
        println!("class {}:", class.name);

        for (i, f) in class.methods.values().enumerate() {
            if i > 0 {
                println!();
            }

            print_func(f, 1);
        }

        println!();
    }

    for f in package.functions.iter() {
        print_func(f, 0);
        println!();
    }

    println!("main:");

    for (i, opcode) in package
        .body
        .chunks_exact(bytecode::SIZE)
        .map(|mut chunk| Bytecode::deserialize(&mut chunk))
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
        Cmd::Run(RunCmd { source }) => {
            let mut ctx = GlobalContext::default();
            let module = vm::compile(source, &mut ctx)?;
            let mut gc = Gc::default();
            let mut builtins = Builtins::default();
            let mut vm = AtomVm::new(&mut gc, ctx, "atom".into(), module)?;

            vm.run(&mut gc, &mut builtins)?;
            gc.sweep();

            #[cfg(feature = "profiler")]
            {
                let stats = gc.stats();
                let report = vm.profiler().report();

                println!(
                    "\n-- REPORT (took {:?}, {} allocations) --",
                    report.exec_time, stats.alloc_count
                );

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
            ast,
            source,
            bytecode,
        }) => {
            let mut ctx = GlobalContext::default();
            let source = fs::read_to_string(source)?;
            let tree = vm::parse(&source)?;

            if ast {
                println!(
                    "{}",
                    ron::ser::to_string_pretty(
                        &tree,
                        PrettyConfig::default()
                            .struct_names(true)
                            .indentor("  ")
                            .compact_arrays(true)
                    )?
                );

                return Ok(());
            }

            let compiler = Compiler::default();
            let module = compiler.compile(&mut ctx, tree)?;

            if bytecode {
                print_atoms(ctx);
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
                    eprintln!("{} at {}", call, call.span);
                }
            }
        }

        exit(1);
    }
}
