mod syntax;

use std::fs;
use std::path::PathBuf;

use clap::Parser as ClapParser;

use syntax::Parser;

#[derive(ClapParser)]
#[clap(about = "atom")]
struct Opts {
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(ClapParser)]
struct CompileOpts {
    filename: PathBuf,
}

#[derive(ClapParser)]
enum Cmd {
    #[clap(about = "Compile a program")]
    Compile(CompileOpts),
}

fn main() {
    let opts = Opts::parse();

    match opts.cmd {
        Cmd::Compile(opts) => {
            let source = fs::read_to_string(opts.filename).expect("failed to read file");
            let parser = Parser::new(&source);
            let nodes = parser.parse().expect("parse failed");

            println!("{:#?}", nodes);
        }
    }
}
