use std::path::PathBuf;

use clap::Clap;

use atom_ir::{Code, IR};

use crate::compiler::Compiler;
use crate::parser;
use crate::utils::Error;
use crate::vm::VM;

#[derive(Clap)]
pub struct Opts {
    pub(crate) filename: PathBuf,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    show_ir: bool,
    #[clap(long)]
    no_optimizations: bool,
}

pub fn command(module_paths: &[PathBuf], opts: Opts, contents: &str) -> Result<(), Error> {
    let tree = parser::parse(contents)?;

    if opts.show_ast {
        println!("{:#?}", tree);
    }

    let mut compiler = Compiler::new(tree, !opts.no_optimizations);

    for path in module_paths {
        compiler.add_lookup_path(path);
    }

    let module = compiler.compile()?;

    if opts.show_ir {
        println!("Interfaces:");
        println!("{:#?}", module.interfaces);

        println!("\nClasses:");
        println!("{:#?}", module.classes);

        println!("\nFunctions:");
        println!("{:#?}", module.funcs);
    }

    let location = opts
        .filename
        .to_str()
        .map(|s| s.to_string())
        .unwrap_or_else(|| "unknown".to_string());
    let mut vm = VM::new()?;

    vm.register_module(module, location)?;
    vm.eval(
        "main",
        vec![
            IR::new(Code::LoadName("main".to_string()), 0..0),
            IR::new(Code::Call(0), 0..0),
        ],
    )?;

    Ok(())
}
