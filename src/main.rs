use ::std::fs;

use clap::Clap;

use crate::cmd::{Cmd, Opts};
use crate::utils::display_error;

#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

mod ast;
mod cmd;
mod compiler;
mod parser;
mod runtime;
mod std;
mod tests;
mod utils;
mod vm;

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Run(run_opts) => {
            let source = fs::read_to_string(&run_opts.filename).expect("failed to read file");

            if let Err(e) = cmd::run(&opts.module_path, run_opts, &source) {
                display_error(&source, e);
            }
        }
        Cmd::Repl => {
            if let Err(e) = cmd::repl(opts.module_path) {
                display_error("", e);
            }
        }
        Cmd::Stats => {
            if let Err(e) = cmd::stats() {
                display_error("", e);
            }
        }
    }
}
