use std::fs;
use std::path::PathBuf;

use crate::compiler::{CompileError, Compiler, LineNumberOffset};
use crate::error::Error;
use crate::runtime::{AtomRef, ErrorKind, RuntimeError, Value};
use crate::syntax::{parser, Stmt};
use crate::vm::Machine;

pub struct Options {
    pub cleanup: bool,
    pub optimize: bool,
    pub print_ir: bool,
    pub print_ast: bool,
    pub module_name: String,
    pub capture_result: bool,
    pub module_paths: Vec<PathBuf>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            cleanup: true,
            optimize: true,
            print_ir: false,
            print_ast: false,
            capture_result: false,
            module_paths: vec![],
            module_name: "main".to_string(),
        }
    }
}

pub struct RunOutput {
    pub value: Option<Value>,
}

pub struct Engine {
    machine: Machine,
}

impl Engine {
    pub fn new() -> Result<Self, Error> {
        Ok(Self {
            machine: Machine::new()?,
        })
    }

    pub fn run_file(&mut self, mut opts: Options, filename: PathBuf) -> Result<RunOutput, Error> {
        let display_name = filename.to_str().map(|s| s.to_string());
        let source =
            fs::read_to_string(&filename).map_err(|err| CompileError::new(format!("{}", err)))?;

        if let Some(dirname) = filename.parent() {
            opts.module_paths.push(dirname.into());
        }

        self.run(opts, &source).map_err(|err| {
            if let Some(filename) = display_name {
                if let Error::Compile(err) = err {
                    return Error::Compile(err.with_filename(filename));
                }

                if let Error::Runtime(err) = err {
                    return Error::Runtime(err.with_filename(filename));
                }
            }

            err
        })
    }

    pub fn run(&mut self, opts: Options, source: &str) -> Result<RunOutput, Error> {
        let line_numbers_offset = LineNumberOffset::parse(source);
        let tree = parser::parse(source)?;

        self.run_ast(opts, line_numbers_offset, tree)
    }

    pub fn run_ast(
        &mut self,
        opts: Options,
        line_numbers_offset: LineNumberOffset,
        tree: Vec<Stmt>,
    ) -> Result<RunOutput, Error> {
        if opts.print_ast {
            println!("{:#?}", tree);
        }

        let mut compiler = Compiler::new(tree, line_numbers_offset, opts.optimize);

        for path in opts.module_paths {
            compiler.add_lookup_path(&path);
        }

        let modules = compiler.compile_all()?;
        let module = modules
            .iter()
            .find(|module| module.name == opts.module_name)
            .ok_or_else(|| {
                Error::Runtime(RuntimeError::new(
                    ErrorKind::FatalError,
                    "main module not found".to_string(),
                ))
            })?;

        if opts.print_ir {
            println!("Interfaces:");
            println!("{:#?}", module.interfaces);
            println!("\nFunctions:");
            println!("{:#?}", module.functions);
            println!("\nClasses:");
            println!("{:#?}", module.classes);
        }

        let module_name = module.name.clone();

        for module in modules {
            self.machine.register_module(module)?;
        }

        if let Some(main_module) = self.machine.get_module(&module_name) {
            let entrypoint = main_module
                .functions
                .iter()
                .find(|func| func.name == "main")
                .ok_or_else(|| {
                    Error::Runtime(RuntimeError::new(
                        ErrorKind::FatalError,
                        "main function not found".to_string(),
                    ))
                })?;
            let entrypoint = AtomRef::clone(entrypoint);

            self.machine.eval("main", entrypoint)?;

            let output = RunOutput {
                value: self.machine.take_result(),
            };

            if opts.cleanup {
                self.machine.cleanup();
            }

            return Ok(output);
        }

        Err(Error::Runtime(RuntimeError::new(
            ErrorKind::FatalError,
            "no entry point found in main module".to_string(),
        )))
    }
}
