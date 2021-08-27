use std::path::PathBuf;

#[derive(Clone)]
pub struct PathFinder {
    paths: Vec<PathBuf>,
}

impl PathFinder {
    pub fn new() -> Self {
        Self { paths: vec![] }
    }

    pub fn add_path(&mut self, path: PathBuf) {
        self.paths.push(path);
    }

    pub fn find_path(&self, import_path: &[&str]) -> Option<PathBuf> {
        // @TODO: properly sanitize path
        for component in import_path {
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
}
