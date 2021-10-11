use std::path::PathBuf;

use clap::Clap;

use atom_ir::{Code, Location, IR};
use atom_runtime::RuntimeError;

use crate::compiler::{parse_line_numbers_offset, Compiler};
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

pub fn command(module_paths: &[PathBuf], opts: Opts, source: &str) -> Result<(), Error> {
    let tree = parser::parse(source)?;

    if opts.show_ast {
        println!("{:#?}", tree);
    }

    let mut compiler = Compiler::new(
        tree,
        parse_line_numbers_offset(source),
        !opts.no_optimizations,
    );

    for path in module_paths {
        compiler.add_lookup_path(path);
    }

    let modules = compiler.compile_all()?;
    let module = modules
        .iter()
        .find(|module| module.name == "main")
        .ok_or_else(|| Error::Runtime(RuntimeError::new("main module not found".to_string())))?;

    if opts.show_ir {
        println!("Interfaces:");
        println!("{:#?}", module.interfaces);

        println!("\nClasses:");
        println!("{:#?}", module.classes);

        println!("\nFunctions:");
        println!("{:#?}", module.funcs);
    }

    let mut vm = VM::new()?;

    if let Some(id) = module.funcs.iter().position(|func| func.name == "main") {
        for module in modules {
            vm.register_module(module)?;
        }

        vm.eval(
            "main",
            vec![
                IR::new(Code::LoadFn(id), Location::default()),
                IR::new(Code::Call(0), Location::default()),
            ],
        )?;

        return Ok(());
    }

    Err(Error::Runtime(RuntimeError::new(
        "function 'main' was not found in the module".to_string(),
    )))
}
