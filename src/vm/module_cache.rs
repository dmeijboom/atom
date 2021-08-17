use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use indexmap::map::IndexMap;

use crate::ast::Pos;
use crate::compiler::{self, Func, IR};
use crate::runtime::{Result, RuntimeError, TypeId, Value};
use crate::vm::VM;

pub type ExternalFn = fn(&mut VM, Vec<Value>) -> Result<Option<Value>>;
pub type ModuleId = usize;

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

impl FuncDesc {
    pub fn new(func: Func) -> Self {
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
    pub id: usize,
    pub mutable: bool,
    pub public: bool,
}

pub struct MethodDesc {
    pub func: FuncDesc,
    pub class_name: String,
}

#[derive(Clone)]
pub struct InterfaceDesc {
    pub public: bool,
    pub functions: Vec<String>,
}

pub struct ClassDesc {
    pub public: bool,
    pub methods: IndexMap<String, MethodDesc>,
    pub fields: IndexMap<String, FieldDesc>,
}

impl ClassDesc {
    pub fn get_method(&self, name: &str) -> Result<&MethodDesc> {
        self.methods
            .iter()
            .filter(|(method_name, _)| method_name.as_str() == name)
            .map(|(_, method)| method)
            .next()
            .ok_or_else(|| RuntimeError::new(format!("no such method: {}", name)))
    }
}

pub enum TypeDesc {
    Class(ClassDesc),
    Function(FuncDesc),
    Interface(InterfaceDesc),
}

pub struct Type {
    pub id: TypeId,
    pub name: String,
    pub desc: TypeDesc,
    pub module_id: usize,
}

impl Type {
    pub fn try_as_class(&self) -> Result<&ClassDesc> {
        if let TypeDesc::Class(class_desc) = &self.desc {
            return Ok(class_desc);
        }

        Err(RuntimeError::new(format!("no such class: {}", self.name)))
    }

    pub fn try_as_func(&self) -> Result<&FuncDesc> {
        if let TypeDesc::Function(func_desc) = &self.desc {
            return Ok(func_desc);
        }

        Err(RuntimeError::new(format!(
            "no such function: {}",
            self.name
        )))
    }

    pub fn try_as_interface(&self) -> Result<&InterfaceDesc> {
        if let TypeDesc::Interface(interface_desc) = &self.desc {
            return Ok(interface_desc);
        }

        Err(RuntimeError::new(format!(
            "no such interface: {}",
            self.name
        )))
    }
}

pub struct Module {
    pub id: ModuleId,
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
            id: 0,
            name: module.name.clone(),
            filename,
            imports: module.imports.clone(),
            globals: HashMap::new(),
            func_map: HashMap::new(),
            class_map: HashMap::new(),
            interface_map: HashMap::new(),
        };

        for (name, func) in module.funcs {
            vm_module.func_map.insert(name, FuncDesc::new(func));
        }

        for (name, class) in module.classes {
            let mut fields = IndexMap::new();
            let mut methods = IndexMap::new();

            for (name, field) in class.fields.into_iter() {
                fields.insert(
                    name,
                    FieldDesc {
                        id: fields.len(),
                        mutable: field.mutable,
                        public: field.public,
                    },
                );
            }

            for (method_name, func) in class.funcs {
                methods.insert(
                    method_name,
                    MethodDesc {
                        class_name: name.clone(),
                        func: FuncDesc::new(func),
                    },
                );
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
                    class_name: class_name.to_string(),
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
}

pub type Middleware = fn(&mut Module) -> Result<()>;

pub struct ModuleCache {
    types: Vec<Type>,
    lookup_paths: Vec<PathBuf>,
    modules: IndexMap<String, Module>,
    middleware: HashMap<String, Middleware>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            types: vec![],
            lookup_paths: vec![],
            modules: IndexMap::new(),
            middleware: HashMap::new(),
        }
    }

    pub fn add(&mut self, module: Module) -> Result<()> {
        let mut module = module;

        module.id = self.modules.len();

        if let Some(middleware) = self.middleware.get(&module.name) {
            middleware(&mut module)?;
        }

        // Move types into the cache
        for (name, func_desc) in module.func_map.drain() {
            let id = self.types.len();

            self.types.push(Type {
                id,
                name,
                module_id: module.id,
                desc: TypeDesc::Function(func_desc),
            });
        }

        for (name, class_desc) in module.class_map.drain() {
            let id = self.types.len();

            self.types.push(Type {
                id,
                name,
                module_id: module.id,
                desc: TypeDesc::Class(class_desc),
            });
        }

        for (name, interface_desc) in module.interface_map.drain() {
            let id = self.types.len();

            self.types.push(Type {
                id,
                name,
                module_id: module.id,
                desc: TypeDesc::Interface(interface_desc),
            });
        }

        self.modules.insert(module.name.clone(), module);

        Ok(())
    }

    pub fn register_middleware(&mut self, name: &str, middleware: Middleware) {
        self.middleware.insert(name.to_string(), middleware);
    }

    pub fn contains_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }

    pub fn lookup_type(&self, module_name: &str, name: &str) -> Result<&Type> {
        if let Some(id) = self.modules.get_index_of(module_name) {
            if let Some(result) = self
                .types
                .iter()
                .filter(|type_val| type_val.module_id == id && type_val.name == name)
                .next()
            {
                return Ok(result);
            }
        }

        Err(RuntimeError::new(format!(
            "no such type: {}.{}",
            module_name, name
        )))
    }

    pub fn lookup_module_type_id(&self, module_id: usize, name: &str) -> Option<TypeId> {
        if let Some(result) = self
            .types
            .iter()
            .filter(|type_val| type_val.module_id == module_id && type_val.name == name)
            .next()
        {
            return Some(result.id);
        }

        None
    }

    pub fn lookup_module_by_id(&self, id: ModuleId) -> Result<&Module> {
        if let Some((_, module)) = self.modules.get_index(id) {
            return Ok(module);
        }

        Err(RuntimeError::new(format!("no such module: {}", id)))
    }

    pub fn lookup_module(&self, name: &str) -> Result<&Module> {
        if let Some(module) = self.modules.get(name) {
            return Ok(module);
        }

        Err(RuntimeError::new(format!("no such module: {}", name)))
    }

    pub fn lookup_type_by_id(&self, type_id: TypeId) -> Result<&Type> {
        if let Some(type_val) = self.types.get(type_id) {
            return Ok(type_val);
        }

        Err(RuntimeError::new(format!(
            "no such type with ID: {:?}",
            type_id,
        )))
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

    pub fn fmt_type(&self, id: TypeId) -> String {
        if let Ok(type_val) = self.lookup_type_by_id(id) {
            if let Some((_, module)) = self.modules.get_index(type_val.module_id) {
                return match &type_val.desc {
                    TypeDesc::Class(_) => format!("{}.{}", module.name, type_val.name),
                    TypeDesc::Function(_) => format!("{}.{}", module.name, type_val.name),
                    TypeDesc::Interface(_) => format!("{}.{}", module.name, type_val.name),
                };
            }
        }

        "!".to_string()
    }
}
