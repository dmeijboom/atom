use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;

use crate::compiler::{self, Func};
use crate::runtime::{IndexedBTreeMap, Result, RuntimeError, Value};

pub enum Runnable {
    Func(Func),
    External(Box<dyn Fn(Vec<Value>) -> Result<Option<Value>>>),
}

pub struct FuncDesc {
    pub run: Runnable,
}

pub struct FieldDesc {
    pub mutable: bool,
}

pub struct MethodDesc {
    pub func: Func,
}

pub struct ClassDesc {
    pub methods: HashMap<String, MethodDesc>,
    pub fields: IndexedBTreeMap<String, FieldDesc>,
}

pub struct Module {
    pub name: String,
    pub filename: Option<PathBuf>,
    pub func_map: HashMap<String, FuncDesc>,
    pub class_map: HashMap<String, ClassDesc>,
}

impl Module {
    pub fn new(module: compiler::Module, filename: Option<PathBuf>) -> Self {
        let mut vm_module = Self {
            name: module.name.clone(),
            filename,
            func_map: HashMap::new(),
            class_map: HashMap::new(),
        };

        for (name, func) in module.funcs {
            vm_module.func_map.insert(
                name,
                FuncDesc {
                    run: Runnable::Func(func),
                },
            );
        }

        for (name, class) in module.classes {
            let mut fields = BTreeMap::new();
            let mut methods = HashMap::new();

            for (name, field) in class.fields.into_iter() {
                fields.insert(
                    name,
                    FieldDesc {
                        mutable: field.mutable,
                    },
                );
            }

            for (name, func) in class.funcs {
                methods.insert(name, MethodDesc { func });
            }

            vm_module.class_map.insert(
                name,
                ClassDesc {
                    fields: IndexedBTreeMap::new(fields),
                    methods,
                },
            );
        }

        vm_module
    }

    pub fn register_external_fn<F: Fn(Vec<Value>) -> Result<Option<Value>> + 'static>(
        &mut self,
        name: &str,
        func: F,
    ) {
        self.func_map.insert(
            name.to_string(),
            FuncDesc {
                run: Runnable::External(Box::new(func)),
            },
        );
    }
}

pub struct ModuleCache {
    modules: HashMap<String, Module>,
    current_module: Option<String>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            current_module: None,
        }
    }

    pub fn add(&mut self, module: Module) {
        self.modules.insert(module.name.clone(), module);
    }

    pub(crate) fn set_current(&mut self, name: &str) {
        if !self.modules.contains_key(name) {
            panic!("unable to set unknown current module: {}", name);
        }

        self.current_module = Some(name.to_string());
    }

    pub(crate) fn current(&mut self) -> Result<&mut Module> {
        self.current_module
            .clone()
            .as_ref()
            .and_then(move |name| self.modules.get_mut(name))
            .ok_or_else(|| RuntimeError::new("no active module found".to_string()))
    }

    pub(crate) fn current_name(&self) -> Result<&str> {
        self.current_module
            .as_ref()
            .and_then(|name| Some(name.as_str()))
            .ok_or_else(|| RuntimeError::new("no active module found".to_string()))
    }

    pub(crate) fn lookup_class(&self, module_name: &str, class_name: &str) -> Result<&ClassDesc> {
        if let Some(module) = self.modules.get(module_name) {
            if let Some(class_desc) = module.class_map.get(class_name) {
                return Ok(class_desc);
            }

            return Err(RuntimeError::new(format!(
                "no such class: {}.{}",
                module_name, class_name
            )));
        }

        Err(RuntimeError::new(format!(
            "no such module: {}",
            module_name
        )))
    }

    pub(crate) fn lookup_function(
        &self,
        module_name: &str,
        function_name: &str,
    ) -> Result<&FuncDesc> {
        if let Some(module) = self.modules.get(module_name) {
            if let Some(function_desc) = module.func_map.get(function_name) {
                return Ok(function_desc);
            }

            return Err(RuntimeError::new(format!(
                "no such function: {}.{}(..)",
                module_name, function_name
            )));
        }

        Err(RuntimeError::new(format!(
            "no such module: {}",
            module_name
        )))
    }

    pub(crate) fn lookup_method(
        &self,
        module_name: &str,
        class_name: &str,
        function_name: &str,
    ) -> Result<&MethodDesc> {
        let class_desc = self.lookup_class(module_name, class_name)?;

        if let Some(method) = class_desc.methods.get(function_name) {
            return Ok(method);
        }

        Err(RuntimeError::new(format!(
            "no such method: {}.{}.{}(..)",
            module_name, class_name, function_name
        )))
    }
}
