use std::collections::HashMap;
use std::path::PathBuf;

use indexmap::map::IndexMap;
use wyhash2::WyHash;

use atom_runtime::{AtomRef, Class, Result, RuntimeError};

use crate::vm::module::ModuleId;

use super::module::Module;

pub type Middleware = fn(&mut Module) -> Result<()>;

pub struct ModuleCache {
    module_names: Vec<String>,
    lookup_paths: Vec<PathBuf>,
    modules: IndexMap<String, Module, WyHash>,
    middleware: HashMap<String, Middleware>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            module_names: vec![],
            lookup_paths: vec![],
            modules: IndexMap::with_hasher(WyHash::with_seed(4)),
            middleware: HashMap::new(),
        }
    }

    pub fn add(&mut self, module: Module) -> Result<()> {
        let mut module = module;

        module.id = self.modules.len();

        for (name, middleware) in self.middleware.iter() {
            if name == &module.name {
                middleware(&mut module)?;
            }
        }

        self.module_names.push(module.name.clone());
        self.modules.insert(module.name.clone(), module);

        Ok(())
    }

    pub fn register_middleware(&mut self, name: &str, middleware: Middleware) {
        self.middleware.insert(name.to_string(), middleware);
    }

    pub fn contains_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }

    pub fn add_lookup_path(&mut self, path: PathBuf) {
        self.lookup_paths.push(path);
    }

    pub fn find_module_path(&self, name: &str) -> Option<PathBuf> {
        let components = name.split('.').collect::<Vec<_>>();

        for lookup_path in self.lookup_paths.iter() {
            let mut path = lookup_path.clone();

            for component in components.iter().take(components.len() - 1) {
                path.push(component);
            }

            if let Some(last_component) = components.last() {
                path.push(format!("{}.atom", last_component));
            }

            if !path.exists() {
                continue;
            }

            return Some(path);
        }

        None
    }

    pub fn get_module_name(&self, id: ModuleId) -> Result<&String> {
        self.module_names
            .get(id)
            .ok_or_else(|| RuntimeError::new(format!("no such module with ID: {}", id)))
    }

    pub fn get_module_id(&self, name: &str) -> Result<ModuleId> {
        self.module_names
            .iter()
            .position(|module_name| module_name == name)
            .ok_or_else(|| RuntimeError::new(format!("no such module: {}", name)))
    }

    pub fn get_module_by_id(&self, id: ModuleId) -> Result<&Module> {
        if let Some((_, module)) = self.modules.get_index(id) {
            return Ok(module);
        }

        Err(RuntimeError::new(format!("no such module with ID: {}", id)))
    }

    pub fn get_module(&self, name: &str) -> Result<&Module> {
        if let Some(module) = self.modules.get(name) {
            return Ok(module);
        }

        Err(RuntimeError::new(format!("no such module: {}", name)))
    }

    pub fn get_class(&self, module_name: &str, name: &str) -> Result<AtomRef<Class>> {
        let module = self.get_module(module_name)?;

        if let Some(class) = module.classes.get(name) {
            return Ok(AtomRef::clone(class));
        }

        Err(RuntimeError::new(format!(
            "no such class: {}.{}",
            module_name, name
        )))
    }
}
