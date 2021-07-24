use std::fs;
use std::path::PathBuf;

use clap::Clap;

use crate::compiler::{Code, Compiler, LocalId, IR};
use crate::runtime::Value;
use crate::vm::VM;

mod ast;
mod compiler;
mod parser;
mod runtime;
mod vm;

#[derive(Clap)]
struct Opts {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Clap)]
enum Cmd {
    Run(RunOpts),
}

#[derive(Clap)]
struct RunOpts {
    filename: PathBuf,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    show_ir: bool,
}

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Run(run_opts) => {
            let contents = fs::read_to_string(run_opts.filename).expect("unable to read file");

            let tree = parser::parse(&contents).expect("syntax error");

            if run_opts.show_ast {
                println!("{:#?}", tree);
            }

            let compiler = Compiler::new(tree);
            let module = compiler.compile().expect("compile error");

            if run_opts.show_ir {
                println!("{:#?}", module.funcs);
            }

            let mut vm = VM::new();

            vm.register(module);
            vm.register_external_fn(
                "std.core",
                "println",
                Box::new(|values: Vec<Value>| {
                    println!(
                        "{}",
                        values
                            .into_iter()
                            .map(|value| format!("{}", value))
                            .collect::<Vec<_>>()
                            .join(", "),
                    );

                    Ok(None)
                }),
            );

            vm.eval(vec![
                IR::new(Code::Load(LocalId::new("main".to_string())), 0..0),
                IR::new(Code::Call((vec![], 0)), 0..0),
            ])
            .expect("RuntimeError");
        }
    }
}
