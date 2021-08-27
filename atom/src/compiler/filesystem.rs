use std::collections::HashMap;
use std::fs;
use std::io::{Error, ErrorKind, Result};
use std::path::{Path, PathBuf};

pub trait ReadFile {
    fn read_file(&self, path: &Path) -> Result<String>;
}

pub struct Fs {}

impl ReadFile for Fs {
    fn read_file(&self, path: &Path) -> Result<String> {
        fs::read_to_string(path)
    }
}

pub struct VirtFs {
    data: HashMap<PathBuf, String>,
}

impl VirtFs {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, path: impl AsRef<Path>, content: String) {
        self.data.insert(path.as_ref().to_path_buf(), content);
    }
}

impl ReadFile for VirtFs {
    fn read_file(&self, path: &Path) -> Result<String> {
        if let Some(content) = self.data.get(path) {
            return Ok(content.clone());
        }

        Err(Error::new(
            ErrorKind::NotFound,
            format!("file '{}' not found", path.to_str().unwrap()),
        ))
    }
}

pub struct FsWithCache {
    fs: Fs,
    cache: VirtFs,
}

impl FsWithCache {
    pub fn new(fs: Fs, cache: VirtFs) -> Self {
        Self { fs, cache }
    }
}

impl ReadFile for FsWithCache {
    fn read_file(&self, path: &Path) -> Result<String> {
        for (filename, content) in self.cache.data.iter() {
            if path.ends_with(filename) {
                return Ok(content.clone());
            }
        }

        self.fs.read_file(path)
    }
}
