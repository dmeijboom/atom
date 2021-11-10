use std::path::PathBuf;

use clap::Parser;

use atom_ir::{Code, IR};
use atom_runtime::RuntimeError;

use crate::compiler::{Compiler, LineNumberOffset};
use crate::parser;
use crate::utils::Error;
use crate::vm::VM;

#[derive(Parser)]
pub struct Opts {
    pub(crate) filename: PathBuf,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    show_ir: bool,
    #[clap(long)]
    no_optimizations: bool,
}

pub fn command(module_paths: &[PathBuf], opts: Opts, source: &str) -> Result<(), Error> {
    let tree = parser::parse(source)?;

    if opts.show_ast {
        println!("{:#?}", tree);
    }

    let mut compiler = Compiler::new(
        tree,
        LineNumberOffset::parse(source),
        !opts.no_optimizations,
    );

    for path in module_paths {
        compiler.add_lookup_path(path);
    }

    let mut modules = compiler.compile_all().map_err(|e| {
        if e.filename.is_some() {
            return e;
        }

        if let Some(filename) = opts.filename.to_str().map(|s| s.to_string()) {
            e.with_filename(filename)
        } else {
            e
        }
    })?;
    let module = modules
        .iter_mut()
        .find(|module| module.name == "main")
        .ok_or_else(|| Error::Runtime(RuntimeError::new("main module not found".to_string())))?;

    module.filename = opts.filename.to_str().map(|s| s.to_string());

    if opts.show_ir {
        println!("Interfaces:");
        println!("{:#?}", module.interfaces);

        println!("\nClasses:");
        println!("{:#?}", module.classes);

        println!("\nFunctions:");
        println!("{:#?}", module.functions);
    }

    let mut vm = VM::new()?;

    if let Some(id) = module.functions.get_index_of("main") {
        for module in modules {
            vm.register_module(module)?;
        }

        vm.eval(
            "main",
            IR::with_codes(vec![Code::LoadFn(id), Code::Call(0)]),
        )?;

        return Ok(());
    }

    Err(Error::Runtime(RuntimeError::new(
        "function 'main' was not found in the module".to_string(),
    )))
}
