use std::path::PathBuf;

use clap::Parser;

pub use run::command as run;
pub use stats::command as stats;

mod run;
mod stats;

#[derive(Parser)]
pub struct Opts {
    #[clap(long)]
    pub module_path: Vec<PathBuf>,
    #[clap(subcommand)]
    pub cmd: Cmd,
}

#[derive(Parser)]
pub enum Cmd {
    Run(run::Opts),
    Stats,
}
