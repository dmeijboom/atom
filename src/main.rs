mod backend;
mod frontend;
mod gcc;

use std::fmt::Debug;
use std::fs;
use std::path::PathBuf;

use anyhow::{Context, Result};
use clap::Parser as ClapParser;
use inkwell::context;
use temp_dir::TempDir;

use crate::frontend::Analyzer;
use backend::CodeGen;
use frontend::{syntax::Parser, Compiler};

#[derive(ClapParser)]
#[clap(about = "atom")]
struct Opts {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(ClapParser)]
struct CompileOpts {
    filename: PathBuf,
    #[clap(long)]
    check: bool,
    #[clap(short)]
    output: Option<PathBuf>,
}

#[derive(ClapParser)]
enum Cmd {
    #[clap(about = "Compile a program")]
    Compile(CompileOpts),
}

fn pretty_print<T: Debug>(input: T) {
    println!("{}", format!("{:#?}", input).replace("    ", "  "));
}

fn main() -> Result<()> {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Compile(opts) => {
            let source = fs::read_to_string(opts.filename).context("failed to read file")?;
            let program = Parser::new(&source).parse()?;

            println!("-- AST --");
            pretty_print(&program);

            let program = Analyzer::new().analyze(program)?;

            println!("\n-- Typed AST --");
            pretty_print(&program);

            let module = Compiler::new().compile(program)?;

            println!("\n-- module --");
            pretty_print(&module);

            let ctx = context::Context::create();
            let codegen = CodeGen::new(&ctx, module);
            let buffer = codegen.generate().context("code generation failed")?;

            if opts.check {
                return Ok(());
            }

            let build_dir = TempDir::new()?;

            fs::write(build_dir.child("program.o"), buffer.as_slice())
                .context("failed to write to object file")?;

            gcc::compile(
                build_dir.child("program.o"),
                build_dir.child("program"),
                true,
            )?;

            fs::rename(
                build_dir.child("program"),
                opts.output.unwrap_or_else(|| PathBuf::from("program")),
            )?;

            Ok(())
        }
    }
}
