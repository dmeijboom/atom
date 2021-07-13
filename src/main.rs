use std::fs;
use std::path::PathBuf;

use clap::Clap;

mod ast;
mod parser;

#[derive(Clap)]
struct Opts {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Clap)]
enum Cmd {
    Run(RunOpts),
}

#[derive(Clap)]
struct RunOpts {
    filename: PathBuf,
}

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Run(run_opts) => {
            let contents = fs::read_to_string(run_opts.filename)
                .expect("unable to read file");

            let tree = parser::parse(&contents)
                .expect("syntax error");

            println!("{:?}", tree);
        }
    }
}
