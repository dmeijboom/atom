use std::collections::HashMap;

use indexmap::map::IndexMap;
use wyhash2::WyHash;

use crate::compiler::ir::{Code, Location};
use crate::compiler::{self, ElementKind, FunctionAttr};
use crate::runtime::{
    AtomRef, Class, ErrorKind, ExternalFn, Field, Fn, FnArg, Interface, Origin, Result,
    RuntimeError,
};
use crate::vm::module::{Global, ModuleId};

use super::module::Module;

pub type ExternalHook = fn(&str, &str, Option<&str>) -> Option<ExternalFn>;

pub struct ModuleCache {
    external_hooks: Vec<ExternalHook>,
    modules: IndexMap<String, Module, WyHash>,
}

impl ModuleCache {
    pub fn new() -> Self {
        Self {
            external_hooks: vec![],
            modules: IndexMap::with_hasher(WyHash::default()),
        }
    }

    pub fn add_external_hook(&mut self, hook: ExternalHook) {
        self.external_hooks.push(hook);
    }

    fn call_external_hook(
        &self,
        module_name: &str,
        function_name: &str,
        class_name: Option<&str>,
    ) -> Result<ExternalFn> {
        for hook in self.external_hooks.iter() {
            if let Some(external) = hook(module_name, function_name, class_name) {
                return Ok(external);
            }
        }

        if let Some(class_name) = class_name {
            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!(
                    "external '{}.{}.{}(...)' not found",
                    module_name, class_name, function_name
                ),
            ));
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!(
                "external '{}.{}(...)' not found",
                module_name, function_name
            ),
        ))
    }

    fn make_interface(
        &self,
        module: &Module,
        interface: compiler::Interface,
    ) -> AtomRef<Interface> {
        AtomRef::new(Interface {
            name: interface.name,
            functions: interface.methods,
            origin: Origin::new(
                module.id,
                module.name.clone(),
                module.filename.clone(),
                Location::default(),
            ),
        })
    }

    fn make_fn(
        &self,
        module: &Module,
        func: compiler::Function,
        class_name: Option<&str>,
    ) -> Result<AtomRef<Fn>> {
        let origin = Origin::new(
            module.id,
            module.name.clone(),
            module.filename.clone(),
            func.location,
        );

        if func.attr.contains(FunctionAttr::Extern) {
            let external_func = match class_name {
                Some(class_name) => {
                    self.call_external_hook(&module.name, &func.name, Some(class_name))?
                }
                None => self.call_external_hook(&module.name, &func.name, class_name)?,
            };

            return Ok(AtomRef::new(Fn::external(
                func.name,
                func.attr.contains(FunctionAttr::Public),
                origin,
                external_func,
            )));
        }

        Ok(AtomRef::new(Fn::native(
            func.name,
            func.attr.contains(FunctionAttr::Public),
            !func.body.iter().any(|code| code == &Code::Return),
            origin,
            func.args
                .into_iter()
                .map(|arg| (arg.name, FnArg::new(arg.mutable)))
                .collect(),
            func.body,
        )))
    }

    fn make_class(&self, module: &Module, class: compiler::Class) -> Result<AtomRef<Class>> {
        let mut output = Class {
            name: class.name.clone(),
            fields: class
                .fields
                .into_iter()
                .map(|(name, field)| (name.clone(), Field::new(name, field.mutable, field.public)))
                .collect(),
            methods: HashMap::with_hasher(WyHash::default()),
            static_methods: HashMap::with_hasher(WyHash::default()),
            // @TODO: shouldn't classes also have a position?
            origin: Origin::new(
                module.id,
                module.name.clone(),
                module.filename.clone(),
                Location::default(),
            ),
        };

        for (name, func) in class.methods {
            if func.attr.contains(FunctionAttr::Static) {
                output
                    .static_methods
                    .insert(name, self.make_fn(module, func, Some(&class.name))?);
            } else {
                output
                    .methods
                    .insert(name, self.make_fn(module, func, Some(&class.name))?);
            }
        }

        Ok(AtomRef::new(output))
    }

    pub fn register(&mut self, compiled_module: compiler::Module) -> Result<()> {
        // We don't want to re-register the same module
        if self.modules.contains_key(&compiled_module.name) {
            return Ok(());
        }

        // Then, add the types
        let mut module = Module::new(
            self.modules.len(),
            compiled_module.name,
            compiled_module.filename,
        );

        for (_, func) in compiled_module.functions {
            module.functions.push(self.make_fn(&module, func, None)?);
        }

        for (name, class) in compiled_module.classes {
            module
                .classes
                .insert(name, self.make_class(&module, class)?);
        }

        module.interfaces = compiled_module
            .interfaces
            .into_iter()
            .map(|(_, interface)| self.make_interface(&module, interface))
            .collect();

        // At last, register the globals
        for (import_name, import) in compiled_module.imports {
            let origin = self.modules.get(&import.id.module);
            let value = match import.kind {
                ElementKind::Fn => {
                    let func = origin
                        .and_then(|module| {
                            module
                                .functions
                                .iter()
                                .find(|func| func.name == import_name)
                        })
                        .ok_or_else(|| {
                            RuntimeError::new(
                                ErrorKind::FatalError,
                                format!("unable to resolve import: {}", import_name),
                            )
                        })?;

                    Global::Fn(AtomRef::downgrade(func))
                }
                ElementKind::Class => {
                    let class = origin
                        .and_then(|module| module.classes.get(&import_name))
                        .ok_or_else(|| {
                            RuntimeError::new(
                                ErrorKind::FatalError,
                                format!("unable to resolve import: {}", import_name),
                            )
                        })?;

                    Global::Class(AtomRef::downgrade(class))
                }
                ElementKind::Interface => {
                    let interface = origin
                        .and_then(|module| {
                            module
                                .interfaces
                                .iter()
                                .find(|interface| interface.name == import_name)
                        })
                        .ok_or_else(|| {
                            RuntimeError::new(
                                ErrorKind::FatalError,
                                format!("unable to resolve import: {}", import_name),
                            )
                        })?;

                    Global::Interface(AtomRef::downgrade(interface))
                }
            };

            module.globals.push(value);
        }

        self.modules.insert(module.name.clone(), module);

        Ok(())
    }

    pub fn get_module_by_id(&self, id: ModuleId) -> Result<&Module> {
        if let Some((_, module)) = self.modules.get_index(id) {
            return Ok(module);
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!("no such module with ID: {}", id),
        ))
    }

    pub fn get_module(&self, name: &str) -> Result<&Module> {
        if let Some(module) = self.modules.get(name) {
            return Ok(module);
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!("no such module: {}", name),
        ))
    }

    pub fn get_class(&self, module_name: &str, name: &str) -> Result<AtomRef<Class>> {
        let module = self.get_module(module_name)?;

        if let Some(class) = module.classes.get(name) {
            return Ok(AtomRef::clone(class));
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!("no such class: {}.{}", module_name, name),
        ))
    }
}
