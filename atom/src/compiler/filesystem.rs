use std::collections::HashMap;
use std::fs;
use std::io::{Error, ErrorKind, Result};
use std::path::{Path, PathBuf};

pub enum File {
    Cached((PathBuf, &'static str)),
    Loaded((PathBuf, String)),
}

impl File {
    pub fn name(&self) -> &Path {
        match self {
            File::Cached((path, _)) => path,
            File::Loaded((path, _)) => path,
        }
    }

    pub fn source(&self) -> &str {
        match self {
            File::Cached((_, source)) => source,
            File::Loaded((_, source)) => source.as_str(),
        }
    }
}

#[derive(Clone)]
pub struct FileSystemCache {
    modules: HashMap<String, &'static str>,
}

impl FileSystemCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, name: String, source: &'static str) {
        self.modules.insert(name, source);
    }

    pub fn get_module(&self, name: &str) -> Option<&'static str> {
        if let Some(source) = self.modules.get(name) {
            return Some(*source);
        }

        None
    }
}

#[derive(Clone)]
pub struct FileSystem {
    paths: Vec<PathBuf>,
    cache: FileSystemCache,
}

impl FileSystem {
    pub fn new(cache: FileSystemCache) -> Self {
        Self {
            paths: vec![PathBuf::from("./")],
            cache,
        }
    }

    pub fn add_path(&mut self, path: PathBuf) {
        self.paths.push(path);
    }

    pub fn find_path(&self, name: &str) -> Option<PathBuf> {
        let import_path = name.split('.').collect::<Vec<_>>();

        // @TODO: properly sanitize path
        for component in import_path.iter() {
            if component.contains("..") || component.contains(':') {
                return None;
            }
        }

        for path in self.paths.iter() {
            let mut current = path.clone();

            current.push(format!("{}.atom", import_path.join("/")));

            if current.exists() {
                return Some(current);
            }
        }

        None
    }

    pub fn read_file(&self, name: &str) -> Result<File> {
        if let Some(source) = self.cache.get_module(name) {
            return Ok(File::Cached((
                format!("{}.atom", name.replace(".", "/")).into(),
                source,
            )));
        }

        if let Some(path) = self.find_path(name) {
            let source = fs::read_to_string(&path)?;

            return Ok(File::Loaded((path, source)));
        }

        Err(Error::new(
            ErrorKind::NotFound,
            format!("no such module: {}", name),
        ))
    }
}
