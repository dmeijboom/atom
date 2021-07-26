use std::collections::HashMap;
use std::path::PathBuf;

use crate::compiler;
use crate::compiler::{Class, Func};
use crate::runtime::{Result, Value};

pub enum Runnable {
    Func(Func),
    External(Box<dyn Fn(Vec<Value>) -> Result<Option<Value>>>),
}

pub struct FuncDesc {
    pub run: Runnable,
    pub module_name: String,
}

pub struct ClassDesc {
    pub class: Class,
    pub module_name: String,
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
                    module_name: module.name.clone(),
                },
            );
        }

        for (name, class) in module.classes {
            vm_module.class_map.insert(
                name,
                ClassDesc {
                    class,
                    module_name: module.name.clone(),
                },
            );
        }

        vm_module
    }

    pub fn register_external_fn<F: Fn(Vec<Value>) -> Result<Option<Value>> + 'static>(
        &mut self,
        module_name: &str,
        name: &str,
        func: F,
    ) {
        self.func_map.insert(
            name.to_string(),
            FuncDesc {
                module_name: module_name.to_string(),
                run: Runnable::External(Box::new(func)),
            },
        );
    }
}
