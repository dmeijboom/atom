use ::std::fs;
use ::std::process::exit;

use clap::Clap;

use crate::cmd::{Cmd, Opts};
use crate::utils::display_error;

#[cfg(feature = "snmalloc")]
#[global_allocator]
static ALLOC: snmalloc_rs::SnMalloc = snmalloc_rs::SnMalloc;

mod ast;
mod cmd;
mod compiler;
mod parser;
mod std;
mod tests;
mod utils;
mod vm;

fn main() {
    let opts = Opts::parse();

    //println!("{}", wyhash2::wyhash_single("hello".as_bytes(), 1));

    match opts.cmd {
        Cmd::Run(run_opts) => {
            let source = match fs::read_to_string(&run_opts.filename) {
                Ok(source) => source,
                Err(e) => {
                    eprintln!("unable to read '{}': {}", run_opts.filename.display(), e);
                    exit(1);
                }
            };

            if let Err(e) = cmd::run(&opts.module_path, run_opts, &source) {
                display_error(e);
            }
        }
        Cmd::Stats => {
            if let Err(e) = cmd::stats() {
                display_error(e);
            }
        }
    }
}
