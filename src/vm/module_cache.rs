use std::collections::HashMap;
use std::path::PathBuf;
use std::rc::Rc;

use indexmap::map::IndexMap;

use crate::ast::Pos;
use crate::compiler::{self, Func, IR};
use crate::runtime::{Result, RuntimeError, TypeId, Value};
use crate::vm::VM;

pub type ExternalFn = fn(&mut VM, Vec<Value>) -> Result<Option<Value>>;

pub enum FuncSource {
    Native(Rc<Vec<IR>>),
    External(ExternalFn),
}

#[derive(Clone)]
pub struct ArgumentDesc {
    pub mutable: bool,
}

pub struct FuncDesc {
    pub id: usize,
    pub pos: Pos,
    pub public: bool,
    pub source: FuncSource,
    pub args: IndexMap<String, ArgumentDesc>,
}

impl FuncDesc {
    pub fn new(id: usize, func: Func) -> Self {
        FuncDesc {
            id,
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
}

#[derive(Clone)]
pub struct InterfaceDesc {
    pub id: usize,
    pub public: bool,
    pub functions: Vec<String>,
}

pub struct ClassDesc {
    pub id: usize,
    pub public: bool,
    pub methods: IndexMap<String, MethodDesc>,
    pub fields: IndexMap<String, FieldDesc>,
}

pub enum TypeDesc<'t> {
    Class(&'t ClassDesc),
    Function(&'t FuncDesc),
    Interface(&'t InterfaceDesc),
}

pub enum Type {
    Class(usize),
    Function(usize),
    Interface(usize),
}

pub struct Module {
    pub id: usize,
    pub name: String,
    pub imports: Vec<String>,
    pub filename: Option<PathBuf>,
    pub globals: HashMap<String, Value>,
    pub func_map: IndexMap<String, FuncDesc>,
    pub class_map: IndexMap<String, ClassDesc>,
    pub interface_map: IndexMap<String, InterfaceDesc>,
}

impl Module {
    pub fn new(module: compiler::Module, filename: Option<PathBuf>) -> Self {
        let mut vm_module = Self {
            id: 0,
            name: module.name.clone(),
            filename,
            imports: module.imports.clone(),
            globals: HashMap::new(),
            func_map: IndexMap::new(),
            class_map: IndexMap::new(),
            interface_map: IndexMap::new(),
        };

        let mut id = 0;

        for (name, func) in module.funcs {
            vm_module.func_map.insert(name, FuncDesc::new(id, func));

            id += 1;
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

            for (name, func) in class.funcs {
                methods.insert(
                    name,
                    MethodDesc {
                        func: FuncDesc::new(methods.len(), func),
                    },
                );
            }

            vm_module.class_map.insert(
                name,
                ClassDesc {
                    id: vm_module.class_map.len(),
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
                    id: vm_module.interface_map.len(),
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

            let id = class.methods.len();

            class.methods.insert(
                method_name.to_string(),
                MethodDesc {
                    func: FuncDesc {
                        id,
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
        let id = self.func_map.len();

        self.func_map.insert(
            name.to_string(),
            FuncDesc {
                id,
                pos: (0..0),
                public: true,
                args: IndexMap::new(),
                source: FuncSource::External(func),
            },
        );
    }

    pub fn find_type(&self, name: &str) -> Option<Type> {
        if let Some(id) = self.class_map.get_index_of(name) {
            return Some(Type::Class(id));
        }

        if let Some(id) = self.interface_map.get_index_of(name) {
            return Some(Type::Interface(id));
        }

        if let Some(id) = self.func_map.get_index_of(name) {
            return Some(Type::Function(id));
        }

        None
    }
}

pub type Middleware = fn(&mut Module) -> Result<()>;

pub struct ModuleCache {
    lookup_paths: Vec<PathBuf>,
    current_module: Option<usize>,
    modules: IndexMap<String, Module>,
    middleware: HashMap<String, Middleware>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            current_module: None,
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

        self.modules.insert(module.name.clone(), module);

        Ok(())
    }

    pub fn register_middleware(&mut self, name: &str, middleware: Middleware) {
        self.middleware.insert(name.to_string(), middleware);
    }

    pub fn current_id(&self) -> Result<usize> {
        if let Some(id) = self.current_module {
            return Ok(id);
        }

        Err(RuntimeError::new("no active module found".to_string()))
    }

    pub fn set_current(&mut self, id: usize) {
        self.current_module = Some(id);
    }

    pub fn current(&mut self) -> Result<&mut Module> {
        if let Some(id) = &self.current_module {
            if let Some((_, module)) = self.modules.get_index_mut(*id) {
                return Ok(module);
            }
        }

        Err(RuntimeError::new("no active module found".to_string()))
    }

    pub fn contains_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
    }

    pub fn lookup_type(&self, module_name: &str, name: &str) -> Result<(TypeId, TypeDesc)> {
        if let Some(id) = self.modules.get_index_of(module_name) {
            if let Ok(class_desc) = self.lookup_class(module_name, name) {
                return Ok((TypeId::new(id, class_desc.id), TypeDesc::Class(class_desc)));
            }

            if let Ok(interface_desc) = self.lookup_interface(module_name, name) {
                return Ok((
                    TypeId::new(id, interface_desc.id),
                    TypeDesc::Interface(interface_desc),
                ));
            }

            if let Ok(fn_desc) = self.lookup_function(module_name, name) {
                return Ok((TypeId::new(id, fn_desc.id), TypeDesc::Function(fn_desc)));
            }
        }

        Err(RuntimeError::new(format!(
            "no such type: {}.{}",
            module_name, name
        )))
    }

    pub fn lookup_type_id(&self, module_name: &str, name: &str) -> Result<TypeId> {
        if let Some(id) = self.modules.get_index_of(module_name) {
            if let Ok(class_desc) = self.lookup_class(module_name, name) {
                return Ok(TypeId::new(id, class_desc.id));
            }

            if let Ok(interface_desc) = self.lookup_interface(module_name, name) {
                return Ok(TypeId::new(id, interface_desc.id));
            }

            if let Ok(fn_desc) = self.lookup_function(module_name, name) {
                return Ok(TypeId::new(id, fn_desc.id));
            }
        }

        Err(RuntimeError::new(format!(
            "no such type: {}.{}",
            module_name, name
        )))
    }

    pub fn lookup_class(&self, module_name: &str, class_name: &str) -> Result<&ClassDesc> {
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

    pub fn lookup_class_by_id(&self, id: &TypeId) -> Result<&ClassDesc> {
        if let Some((_, module)) = self.modules.get_index(id.module) {
            if let Some((_, class_desc)) = module.class_map.get_index(id.name) {
                return Ok(&class_desc);
            }
        }

        Err(RuntimeError::new(format!("no such class: {:?}", id,)))
    }

    pub fn lookup_interface(
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

    pub fn lookup_interface_by_id(&self, id: &TypeId) -> Result<&InterfaceDesc> {
        if let Some((_, module)) = self.modules.get_index(id.module) {
            if let Some((_, interface_desc)) = module.interface_map.get_index(id.name) {
                return Ok(interface_desc);
            }
        }

        return Err(RuntimeError::new(format!("no such interface: {:?}", id,)));
    }

    pub fn lookup_function(&self, module_name: &str, function_name: &str) -> Result<&FuncDesc> {
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

    pub fn lookup_function_by_id(&self, id: &TypeId) -> Result<&FuncDesc> {
        if let Some((_, module)) = self.modules.get_index(id.module) {
            if let Some((_, function_desc)) = module.func_map.get_index(id.name) {
                return Ok(function_desc);
            }
        }

        Err(RuntimeError::new(format!("no such function: {:?}", id,)))
    }

    pub fn lookup_method_by_id(&self, id: &TypeId) -> Result<&MethodDesc> {
        if let Some((_, module)) = self.modules.get_index(id.module) {
            if let Some(class) = id.class {
                if let Some((_, class_desc)) = module.class_map.get_index(class) {
                    if let Some((_, method_desc)) = class_desc.methods.get_index(id.name) {
                        return Ok(method_desc);
                    }
                }
            }
        }

        Err(RuntimeError::new(format!("no such method: {:?}", id)))
    }

    pub fn lookup_module(&self, name: &str) -> Result<&Module> {
        if let Some(module) = self.modules.get(name) {
            return Ok(module);
        }

        Err(RuntimeError::new(format!("no such module: {}", name)))
    }

    pub fn add_lookup_path(&mut self, path: PathBuf) {
        self.lookup_paths.push(path);
    }

    pub fn find_module_path(&self, name: &str) -> Option<PathBuf> {
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

    pub fn fmt_class(&self, id: &TypeId) -> String {
        if let Some((module_name, module)) = self.modules.get_index(id.module) {
            if let Some((class_name, _)) = module.class_map.get_index(id.name) {
                return format!("{}.{}", module_name, class_name);
            }
        }

        "!".to_string()
    }

    pub fn fmt_interface(&self, id: &TypeId) -> String {
        if let Some((module_name, module)) = self.modules.get_index(id.module) {
            if let Some((interface_name, _)) = module.interface_map.get_index(id.name) {
                return format!("{}.{}", module_name, interface_name);
            }
        }

        "!".to_string()
    }

    pub fn fmt_func(&self, id: &TypeId) -> String {
        if let Some((module_name, module)) = self.modules.get_index(id.module) {
            if let Some((func_name, _)) = module.func_map.get_index(id.name) {
                return format!("{}.{}", module_name, func_name);
            }
        }

        "!".to_string()
    }

    pub fn fmt_method(&self, id: &TypeId) -> String {
        if let Some((module_name, module)) = self.modules.get_index(id.module) {
            if let Some(class) = id.class {
                if let Some((class_name, class_desc)) = module.class_map.get_index(class) {
                    if let Some((func_name, _)) = class_desc.methods.get_index(id.name) {
                        return format!("{}.{}.{}", module_name, class_name, func_name);
                    }
                }
            }
        }

        "!".to_string()
    }
}
