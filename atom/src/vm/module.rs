use std::collections::HashMap;

use wyhash2::WyHash;

use atom_runtime::{
    AtomRef, Class, ExternalFn, Field, Fn, FnArg, Interface, Origin, Result, RuntimeError, Value,
};

use crate::compiler;

fn parse_interface(module: &Module, interface: compiler::Interface) -> Interface {
    Interface {
        name: interface.name,
        public: interface.public,
        functions: interface.functions,
        origin: Origin::new(module.name.clone(), module.location.clone(), 0..0),
    }
}

fn parse_fn(module: &Module, func: compiler::Func) -> Fn {
    Fn::native(
        func.name,
        Origin::new(module.name.clone(), module.location.clone(), func.pos),
        func.public,
        func.args
            .into_iter()
            .map(|arg| (arg.name, FnArg::new(arg.mutable)))
            .collect(),
        func.body,
    )
}

fn parse_class(module: &Module, class: compiler::Class) -> Class {
    Class {
        name: class.name,
        public: class.public,
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
        methods: class
            .funcs
            .into_iter()
            .map(|(name, func)| (name, AtomRef::new(parse_fn(module, func))))
            .collect(),
        // @TODO: shouldn't classes also have a position?
        origin: Origin::new(module.name.clone(), module.location.clone(), 0..0),
    }
}

pub type ModuleId = usize;

pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub location: String,
    pub imports: Vec<String>,
    pub globals: HashMap<String, Value, WyHash>,
    pub funcs: HashMap<String, AtomRef<Fn>, WyHash>,
    pub classes: HashMap<String, AtomRef<Class>, WyHash>,
    pub interfaces: HashMap<String, AtomRef<Interface>, WyHash>,
}

impl Module {
    pub fn new(compiler_module: compiler::Module, location: String) -> Self {
        let mut module = Self {
            id: 0,
            name: compiler_module.name,
            location,
            imports: compiler_module.imports,
            globals: HashMap::with_hasher(WyHash::default()),
            funcs: HashMap::with_hasher(WyHash::default()),
            classes: HashMap::with_hasher(WyHash::default()),
            interfaces: HashMap::with_hasher(WyHash::default()),
        };

        module.funcs = compiler_module
            .funcs
            .into_iter()
            .map(|(name, func)| (name, AtomRef::new(parse_fn(&module, func))))
            .collect();

        module.classes = compiler_module
            .classes
            .into_iter()
            .map(|(name, class)| (name, AtomRef::new(parse_class(&module, class))))
            .collect();

        module.interfaces = compiler_module
            .interfaces
            .into_iter()
            .map(|(name, interface)| (name, AtomRef::new(parse_interface(&module, interface))))
            .collect();

        module
    }

    pub fn register_external_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        func: ExternalFn,
    ) -> Result<()> {
        if let Some(class_ref) = self.classes.get_mut(class_name) {
            let class = class_ref.as_mut();

            if class.methods.contains_key(method_name) {
                return Err(RuntimeError::new(format!(
                    "unable to redefine method '{}' on class: {}.{}",
                    method_name, self.name, class_name
                )));
            }

            class.methods.insert(
                method_name.to_string(),
                AtomRef::new(Fn::external(
                    method_name.to_string(),
                    Origin::new(self.name.clone(), "unknown".to_string(), 0..0),
                    true,
                    func,
                )),
            );

            return Ok(());
        }

        Err(RuntimeError::new(format!(
            "unable to register external method on unregistered class: {}.{}",
            self.name, class_name
        )))
    }

    pub fn register_external_fn(&mut self, name: &str, func: ExternalFn) {
        self.funcs.insert(
            name.to_string(),
            AtomRef::new(Fn::external(
                name.to_string(),
                Origin::new(self.name.clone(), "unknown".to_string(), 0..0),
                true,
                func,
            )),
        );
    }
}
