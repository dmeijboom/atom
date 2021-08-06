use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use indexmap::map::IndexMap;

use crate::ast::Pos;
use crate::compiler::{self, Func, IR};
use crate::runtime::{Result, RuntimeError, Value};
use crate::vm::VM;

pub type ExternalFn = fn(&VM, Vec<Value>) -> Result<Option<Value>>;

pub enum FuncSource {
    Native(Rc<Vec<IR>>),
    External(ExternalFn),
}

#[derive(Clone)]
pub struct ArgumentDesc {
    pub mutable: bool,
}

pub struct FuncDesc {
    pub pos: Pos,
    pub public: bool,
    pub source: FuncSource,
    pub args: IndexMap<String, ArgumentDesc>,
}

impl Clone for FuncDesc {
    fn clone(&self) -> Self {
        Self {
            pos: self.pos.clone(),
            args: self.args.clone(),
            public: self.public,
            source: match &self.source {
                FuncSource::Native(instructions) => FuncSource::Native(Rc::clone(&instructions)),
                FuncSource::External(closure) => FuncSource::External(*closure),
            },
        }
    }

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone();
    }
}

impl From<Func> for FuncDesc {
    fn from(func: Func) -> Self {
        FuncDesc {
            pos: func.pos.clone(),
            public: func.public,
            args: func
                .args
                .iter()
                .map(|arg| {
                    (
                        arg.name.clone(),
                        ArgumentDesc {
                            mutable: arg.mutable,
                        },
                    )
                })
                .collect(),
            source: FuncSource::Native(Rc::new(func.body)),
        }
    }
}

#[derive(Clone)]
pub struct FieldDesc {
    pub mutable: bool,
    pub public: bool,
}

#[derive(Clone)]
pub struct MethodDesc {
    pub func: FuncDesc,
}

#[derive(Clone)]
pub struct InterfaceDesc {
    pub public: bool,
    pub functions: Vec<String>,
}

#[derive(Clone)]
pub struct ClassDesc {
    pub public: bool,
    pub methods: HashMap<String, MethodDesc>,
    pub fields: IndexMap<String, FieldDesc>,
}

pub enum TypeDesc<'t> {
    Class(&'t ClassDesc),
    Function(&'t FuncDesc),
    Interface(&'t InterfaceDesc),
}

pub enum Type {
    Class,
    Function,
    Interface,
}

pub struct Module {
    pub name: String,
    pub imports: Vec<String>,
    pub filename: Option<PathBuf>,
    pub globals: HashMap<String, Value>,
    pub func_map: HashMap<String, FuncDesc>,
    pub class_map: HashMap<String, ClassDesc>,
    pub interface_map: HashMap<String, InterfaceDesc>,
}

impl Module {
    pub fn new(module: compiler::Module, filename: Option<PathBuf>) -> Self {
        let mut vm_module = Self {
            name: module.name.clone(),
            filename,
            imports: module.imports.clone(),
            globals: HashMap::new(),
            func_map: HashMap::new(),
            class_map: HashMap::new(),
            interface_map: HashMap::new(),
        };

        for (name, func) in module.funcs {
            vm_module.func_map.insert(name, func.into());
        }

        for (name, class) in module.classes {
            let mut fields = IndexMap::new();
            let mut methods = HashMap::new();

            for (name, field) in class.fields.into_iter() {
                fields.insert(
                    name,
                    FieldDesc {
                        mutable: field.mutable,
                        public: field.public,
                    },
                );
            }

            for (name, func) in class.funcs {
                methods.insert(name, MethodDesc { func: func.into() });
            }

            vm_module.class_map.insert(
                name,
                ClassDesc {
                    fields,
                    methods,
                    public: class.public,
                },
            );
        }

        for (name, interface) in module.interfaces {
            vm_module.interface_map.insert(
                name,
                InterfaceDesc {
                    public: interface.public,
                    functions: interface.functions,
                },
            );
        }

        vm_module
    }

    pub fn register_external_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        func: ExternalFn,
    ) -> Result<()> {
        if let Some(class) = self.class_map.get_mut(class_name) {
            if class.methods.contains_key(method_name) {
                return Err(RuntimeError::new(format!(
                    "unable to redefine method '{}' on class: {}.{}",
                    method_name, self.name, class_name
                )));
            }

            class.methods.insert(
                method_name.to_string(),
                MethodDesc {
                    func: FuncDesc {
                        pos: (0..0),
                        public: true,
                        args: IndexMap::new(),
                        source: FuncSource::External(func),
                    },
                },
            );

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "unable to register external method on unregistered class: {}.{}",
            self.name, class_name
        )))
    }

    pub fn register_external_fn(&mut self, name: &str, func: ExternalFn) {
        self.func_map.insert(
            name.to_string(),
            FuncDesc {
                pos: (0..0),
                public: true,
                args: IndexMap::new(),
                source: FuncSource::External(func),
            },
        );
    }

    pub fn find_type(&self, name: &str) -> Option<Type> {
        if self.class_map.contains_key(name) {
            return Some(Type::Class);
        }

        if self.interface_map.contains_key(name) {
            return Some(Type::Interface);
        }

        if self.func_map.contains_key(name) {
            return Some(Type::Function);
        }

        None
    }
}

pub struct ModuleCache {
    modules: HashMap<String, Module>,
    current_module: Option<String>,
    lookup_paths: Vec<PathBuf>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            current_module: None,
            lookup_paths: vec![],
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

    pub(crate) fn contains_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }

    pub(crate) fn lookup_type(&self, module_name: &str, name: &str) -> Option<TypeDesc> {
        if let Ok(class_desc) = self.lookup_class(module_name, name) {
            return Some(TypeDesc::Class(class_desc));
        }

        if let Ok(interface_desc) = self.lookup_interface(module_name, name) {
            return Some(TypeDesc::Interface(interface_desc));
        }

        if let Ok(fn_desc) = self.lookup_function(module_name, name) {
            return Some(TypeDesc::Function(fn_desc));
        }

        None
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

    pub(crate) fn lookup_interface(
        &self,
        module_name: &str,
        interface_name: &str,
    ) -> Result<&InterfaceDesc> {
        if let Some(module) = self.modules.get(module_name) {
            if let Some(interface_desc) = module.interface_map.get(interface_name) {
                return Ok(interface_desc);
            }

            return Err(RuntimeError::new(format!(
                "no such interface: {}.{}",
                module_name, interface_name
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

    pub(crate) fn add_lookup_path(&mut self, path: PathBuf) {
        self.lookup_paths.push(path);
    }

    pub(crate) fn find_module_path(&self, name: &str) -> Option<PathBuf> {
        let components = name.split(".").collect::<Vec<_>>();

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
}
