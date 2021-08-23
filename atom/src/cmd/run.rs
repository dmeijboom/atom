use std::path::PathBuf;

use clap::Clap;

use crate::compiler::{Code, Compiler, IR};
use crate::parser;
use crate::utils::Error;
use crate::vm::{Module, VM};

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

    let compiler = Compiler::new(tree, !opts.no_optimizations);
    let compiled_module = compiler.compile()?;

    if opts.show_ir {
        println!("Interfaces:");
        println!("{:#?}", compiled_module.interfaces);

        println!("\nClasses:");
        println!("{:#?}", compiled_module.classes);

        println!("\nFunctions:");
        println!("{:#?}", compiled_module.funcs);
    }

    let module = Module::new(compiled_module, Some(opts.filename));
    let mut vm = VM::new()?;

    for module_path in module_paths {
        vm.add_module_lookup_path(module_path);
    }

    vm.register_module(module)?;

    vm.eval(
        "main",
        vec![
            IR::new(Code::LoadName("main".to_string()), 0..0),
            IR::new(Code::Call(0), 0..0),
        ],
    )?;

    Ok(())
}
