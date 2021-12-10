use crate::compiler::error::Result;
use crate::compiler::mir::{DeclKind, Mir};
use crate::compiler::module::Id;
use crate::compiler::{mir, Class, Element, ElementKind, Function, Interface, Module};

/// Implements several validations for the program and sets up the module for further processing
pub struct Validator<'c> {
    mir: &'c Mir,
    module: &'c mut Module,
}

impl<'c> Validator<'c> {
    pub fn new(module: &'c mut Module, mir: &'c Mir) -> Self {
        Self { module, mir }
    }

    fn register_function(&mut self, function: &mir::Function, public: bool) {
        if public {
            self.module.exports.insert(
                function.name.clone(),
                Element::new(
                    ElementKind::Fn,
                    Id::new(self.module.name.clone(), function.name.clone()),
                ),
            );
        }

        self.module.functions.insert(
            function.name.clone(),
            Function {
                name: function.name.clone(),
                attr: function.attr,
                args: function.args.clone(),
                location: function.loc.clone(),
                ..Function::default()
            },
        );
    }

    fn register_class(&mut self, class: &mir::Class, public: bool) {
        if public {
            self.module.exports.insert(
                class.name.clone(),
                Element::new(
                    ElementKind::Class,
                    Id::new(self.module.name.clone(), class.name.clone()),
                ),
            );
        }

        let mut output = Class {
            name: class.name.clone(),
            fields: class
                .fields
                .iter()
                .cloned()
                .map(|field| (field.name.clone(), field))
                .collect(),
            ..Class::default()
        };

        for function in class.methods.iter() {
            output.methods.insert(
                function.name.clone(),
                Function {
                    name: function.name.clone(),
                    attr: function.attr,
                    args: function.args.clone(),
                    location: function.loc.clone(),
                    ..Function::default()
                },
            );
        }

        self.module.classes.insert(class.name.clone(), output);
    }

    fn register_interface(&mut self, interface: &mir::Interface, public: bool) {
        if public {
            self.module.exports.insert(
                interface.name.clone(),
                Element::new(
                    ElementKind::Interface,
                    Id::new(self.module.name.clone(), interface.name.clone()),
                ),
            );
        }

        self.module.interfaces.insert(
            interface.name.clone(),
            Interface {
                name: interface.name.clone(),
                methods: interface.functions.clone(),
            },
        );
    }

    pub fn compile(mut self) -> Result<()> {
        // Register global types early
        for decl in self.mir.program.iter() {
            match &decl.kind {
                DeclKind::Function(function) => self.register_function(function, decl.public),
                DeclKind::Class(class) => self.register_class(class, decl.public),
                DeclKind::Interface(interface) => self.register_interface(interface, decl.public),
            }
        }

        Ok(())
    }
}
