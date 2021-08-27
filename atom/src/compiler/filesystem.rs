use std::collections::HashMap;
use std::fs;
use std::io::{Error, ErrorKind, Result};
use std::path::{Path, PathBuf};

pub enum Content {
    Owned(String),
    Ref(&'static str),
}

impl Content {
    pub fn as_str(&self) -> &str {
        match self {
            Content::Owned(s) => s.as_str(),
            Content::Ref(s) => s,
        }
    }
}

pub trait AbstractFs {
    fn read_file(&self, path: &Path) -> Result<Content>;
    fn file_exist(&self, path: &Path) -> bool;
}

pub struct Fs {}

impl AbstractFs for Fs {
    fn read_file(&self, path: &Path) -> Result<Content> {
        fs::read_to_string(path).map(|s| Content::Owned(s))
    }

    fn file_exist(&self, path: &Path) -> bool {
        path.exists()
    }
}

pub struct VirtFs {
    data: HashMap<PathBuf, &'static str>,
}

impl VirtFs {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    pub fn add_file(&mut self, path: impl AsRef<Path>, content: &'static str) {
        self.data.insert(path.as_ref().to_path_buf(), content);
    }
}

impl AbstractFs for VirtFs {
    fn read_file(&self, path: &Path) -> Result<Content> {
        for (filename, content) in self.data.iter() {
            if path.ends_with(filename) {
                return Ok(Content::Ref(content));
            }
        }

        Err(Error::new(
            ErrorKind::NotFound,
            format!("file '{}' not found", path.to_str().unwrap()),
        ))
    }

    fn file_exist(&self, path: &Path) -> bool {
        for (filename, _) in self.data.iter() {
            if path.ends_with(filename) {
                return true;
            }
        }

        false
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

impl AbstractFs for FsWithCache {
    fn read_file(&self, path: &Path) -> Result<Content> {
        if self.cache.file_exist(path) {
            return self.cache.read_file(path);
        }

        self.fs.read_file(path)
    }

    fn file_exist(&self, path: &Path) -> bool {
        self.cache.file_exist(path) || self.fs.file_exist(path)
    }
}
