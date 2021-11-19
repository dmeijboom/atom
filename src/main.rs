use clap::Parser;

use crate::cmd::{Cmd, Opts};

#[cfg(feature = "snmalloc")]
#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

mod cmd;
mod compiler;
mod engine;
mod error;
mod runtime;
mod syntax;
mod vm;

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Run(run_opts) => {
            if let Err(err) = cmd::run(opts.module_path, run_opts) {
                eprintln!("{}", err);
            }
        }
        Cmd::Stats => cmd::stats(),
    }
}
