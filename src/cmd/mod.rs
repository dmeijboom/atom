use clap::Clap;

use std::path::PathBuf;

pub use repl::command as repl;
pub use run::command as run;

mod repl;
mod run;

#[derive(Clap)]
pub struct Opts {
    #[clap(long)]
    pub module_path: Vec<PathBuf>,
    #[clap(subcommand)]
    pub cmd: Cmd,
}

#[derive(Clap)]
pub enum Cmd {
    Run(run::Opts),
    Repl,
}
