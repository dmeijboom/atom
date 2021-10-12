use std::collections::HashMap;

use indexmap::map::IndexMap;
use wyhash2::WyHash;

use atom_ir::Location;
use atom_runtime::{
    AtomRef, Class, ExternalFn, Field, Fn, FnArg, Interface, Origin, Result, RuntimeError, Value,
};

use crate::compiler::{self, TypeKind};
use crate::vm::module::ModuleId;

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
        name: &str,
        method_name: Option<&str>,
    ) -> Result<ExternalFn> {
        for hook in self.external_hooks.iter() {
            if let Some(external) = hook(module_name, name, method_name) {
                return Ok(external);
            }
        }

        if let Some(method_name) = method_name {
            return Err(RuntimeError::new(format!(
                "external '{}.{}.{}(...)' not found",
                module_name, name, method_name
            )));
        }

        Err(RuntimeError::new(format!(
            "external '{}.{}(...)' not found",
            module_name, name
        )))
    }

    fn make_interface(&self, module: &Module, interface: compiler::Interface) -> Interface {
        Interface {
            name: interface.name,
            functions: interface.functions,
            origin: Origin::new(
                module.id,
                module.name.clone(),
                module.filename.clone(),
                Location::default(),
            ),
        }
    }

    fn make_fn(
        &self,
        module: &Module,
        func: compiler::Func,
        class_name: Option<&str>,
    ) -> Result<Fn> {
        let origin = Origin::new(
            module.id,
            module.name.clone(),
            module.filename.clone(),
            func.location,
        );

        if func.is_extern {
            let external_func = match class_name {
                Some(class_name) => {
                    self.call_external_hook(&module.name, class_name, Some(&func.name))?
                }
                None => self.call_external_hook(&module.name, &func.name, class_name)?,
            };

            return Ok(Fn::external(func.name, origin, external_func));
        }

        Ok(Fn::native(
            func.name,
            origin,
            func.args
                .into_iter()
                .map(|arg| (arg.name, FnArg::new(arg.mutable)))
                .collect(),
            func.body,
        ))
    }

    fn make_class(&self, module: &Module, class: compiler::Class) -> Result<Class> {
        let mut output = Class {
            name: class.name.clone(),
            fields: class
                .fields
                .into_iter()
                .enumerate()
                .map(|(id, (name, field))| {
                    (
                        name.clone(),
                        Field::new(id, name, field.mutable, field.public),
                    )
                })
                .collect(),
            methods: HashMap::with_hasher(WyHash::default()),
            // @TODO: shouldn't classes also have a position?
            origin: Origin::new(
                module.id,
                module.name.clone(),
                module.filename.clone(),
                Location::default(),
            ),
        };

        for (name, func) in class.funcs {
            output.methods.insert(
                name,
                AtomRef::new(self.make_fn(module, func, Some(&class.name))?),
            );
        }

        Ok(output)
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

        for func in compiled_module.funcs {
            module
                .funcs
                .push(AtomRef::new(self.make_fn(&module, func, None)?));
        }

        for (name, class) in compiled_module.classes {
            module
                .classes
                .insert(name, AtomRef::new(self.make_class(&module, class)?));
        }

        module.interfaces = compiled_module
            .interfaces
            .into_iter()
            .map(|(_, interface)| AtomRef::new(self.make_interface(&module, interface)))
            .collect();

        // At last, register the globals
        for (global_name, global) in compiled_module.globals {
            let sub_module = self.modules.get(&global.module_name).ok_or_else(|| {
                RuntimeError::new(format!(
                    "unable to register global '{}' for unknown module: {}",
                    global_name, global.module_name
                ))
            })?;

            let value = match global.kind {
                TypeKind::Fn => {
                    let func = sub_module
                        .funcs
                        .iter()
                        .find(|func| func.name == global_name)
                        .ok_or_else(|| {
                            RuntimeError::new(format!(
                                "unable to register function '{}' for module: {}",
                                global_name, global.module_name
                            ))
                        })?;

                    Value::Fn(AtomRef::clone(func))
                }
                TypeKind::Class => {
                    let class = sub_module.classes.get(&global_name).ok_or_else(|| {
                        RuntimeError::new(format!(
                            "unable to register class '{}' for module: {}",
                            global_name, global.module_name
                        ))
                    })?;

                    Value::Class(AtomRef::clone(class))
                }
                TypeKind::Interface => {
                    let interface = sub_module
                        .interfaces
                        .iter()
                        .find(|interface| interface.name == global_name)
                        .ok_or_else(|| {
                            RuntimeError::new(format!(
                                "unable to register interface '{}' for module: {}",
                                global_name, global.module_name
                            ))
                        })?;

                    Value::Interface(AtomRef::clone(interface))
                }
            };

            module.globals.push(value);
        }

        self.modules.insert(module.name.clone(), module);

        Ok(())
    }

    pub fn contains_module(&self, module_name: &str) -> bool {
        self.modules.contains_key(module_name)
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
