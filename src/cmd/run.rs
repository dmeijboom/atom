use std::path::PathBuf;

use clap::Parser;

use crate::engine::{Engine, Options};
use crate::error::Error;

#[derive(Parser)]
pub struct Opts {
    pub(crate) filename: PathBuf,
    #[clap(long)]
    module_path: Vec<PathBuf>,
    #[clap(long)]
    show_ast: bool,
    #[clap(long)]
    show_ir: bool,
    #[clap(long)]
    no_optimizations: bool,
}

pub fn command(module_paths: Vec<PathBuf>, opts: Opts) -> Result<(), Error> {
    let module_paths = vec![module_paths, opts.module_path].concat();
    let options = Options {
        optimize: !opts.no_optimizations,
        print_ir: opts.show_ir,
        print_ast: opts.show_ast,
        capture_result: false,
        module_paths,
        ..Options::default()
    };

    let mut engine = Engine::new()?;
    engine.run_file(options, opts.filename)?;

    Ok(())
}
