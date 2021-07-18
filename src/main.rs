use std::fs;
use std::path::PathBuf;

use clap::Clap;

use crate::compiler::{Code, Compiler, IR};
use crate::vm::VM;

mod ast;
mod parser;
mod compiler;
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
}

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Run(run_opts) => {
            let contents = fs::read_to_string(run_opts.filename)
                .expect("unable to read file");

            let tree = parser::parse(&contents)
                .expect("syntax error");

            println!("AST");
            println!("{:#?}", tree);

            let compiler = Compiler::new(tree);
            let module = compiler.compile()
                .expect("compile error");

            println!("\nIR");
            println!("{:#?}", module.funcs);

            println!("\nVM");

            let mut vm = VM::new();

            vm.register(module);
            vm
                .eval(vec![
                    IR::new(Code::Load("main".to_string()), 0..0),
                    IR::new(Code::Call(0), 0..0),
                ])
                .expect("RuntimeError");
        }
    }
}
