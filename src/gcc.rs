use std::path::Path;
use std::process::Command;

use anyhow::{anyhow, Result};

pub fn compile(src: impl AsRef<Path>, dest: impl AsRef<Path>, optimize: bool) -> Result<()> {
    let mut cmd = Command::new("gcc");

    if optimize {
        cmd.arg("-O3");
    }

    let status = cmd
        .arg("-o")
        .arg(dest.as_ref())
        .arg(src.as_ref())
        .spawn()?
        .wait()?;

    if !status.success() {
        return Err(anyhow!("linking failed"));
    }

    Ok(())
}
