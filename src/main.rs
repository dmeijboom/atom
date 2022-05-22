mod compiler;
mod module;
mod syntax;

use std::fmt::Debug;
use std::fs;
use std::path::PathBuf;

use clap::Parser as ClapParser;

use compiler::Compiler;
use syntax::Parser;

#[derive(ClapParser)]
#[clap(about = "atom")]
struct Opts {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(ClapParser)]
struct CompileOpts {
    filename: PathBuf,
}

#[derive(ClapParser)]
enum Cmd {
    #[clap(about = "Compile a program")]
    Compile(CompileOpts),
}

fn pretty_print<T: Debug>(input: T) {
    println!("{}", format!("{:#?}", input).replace("    ", "  "));
}

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Compile(opts) => {
            let source = fs::read_to_string(opts.filename).expect("failed to read file");
            let parser = Parser::new(&source);
            let program = parser.parse().expect("parse failed");

            println!("AST:");
            pretty_print(&program);

            let module = Compiler::new()
                .compile(program)
                .expect("compilation failed");

            println!("Module:");
            pretty_print(&module);
        }
    }
}
