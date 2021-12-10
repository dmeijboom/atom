use std::collections::HashMap;

use crate::compiler::{CompileError, LineNumberOffset};
use crate::syntax::{
    ClassDeclStmt, Field, FnDeclStmt, ImportStmt, InterfaceDeclStmt, Stmt, Visitable, Visitor,
};

#[derive(Debug, Hash)]
enum Kind {
    Class,
    Method,
    Field,
    Import,
    Function,
    Interface,
}

pub struct Validator<'l> {
    parent_is_class: bool,
    elements: HashMap<String, Kind>,
    children: HashMap<String, Kind>,
    line_numbers_offset: &'l LineNumberOffset,
}

impl<'l> Validator<'l> {
    pub fn new(line_numbers_offset: &'l LineNumberOffset) -> Self {
        Validator {
            parent_is_class: false,
            elements: HashMap::new(),
            children: HashMap::new(),
            line_numbers_offset,
        }
    }

    fn check_unique(&mut self, kind: Kind, name: String) -> Result<(), CompileError> {
        if let Some(kind) = self.elements.get(&name) {
            return Err(CompileError::new(format!(
                "unable to redefine {} {}",
                format!("{:?}", kind).to_lowercase(),
                name,
            )));
        }

        self.elements.insert(name, kind);

        Ok(())
    }

    fn check_unique_child(&mut self, kind: Kind, name: String) -> Result<(), CompileError> {
        if self.children.contains_key(&name) {
            return Err(CompileError::new(format!(
                "unable to redefine {} {}",
                format!("{:?}", kind).to_lowercase(),
                name
            )));
        }

        self.children.insert(name, kind);

        Ok(())
    }

    pub fn validate(mut self, tree: &mut Vec<Stmt>) -> Result<(), CompileError> {
        self.visit_stmt_list(tree)
    }
}

impl<'l> Visitor for Validator<'l> {
    type Error = CompileError;

    fn visit_fn_decl_stmt(&mut self, fn_decl_stmt: &mut FnDeclStmt) -> Result<(), Self::Error> {
        if self.parent_is_class {
            self.check_unique_child(Kind::Method, fn_decl_stmt.name.clone())
        } else {
            self.check_unique(Kind::Function, fn_decl_stmt.name.clone())
        }
        .map_err(|e| e.with_location(self.line_numbers_offset.get_location(&fn_decl_stmt.pos)))?;

        if !self.parent_is_class {
            self.children.clear();
        }

        Ok(())
    }

    fn visit_import_stmt(&mut self, import_stmt: &mut ImportStmt) -> Result<(), Self::Error> {
        for path in import_stmt.path.iter() {
            self.check_unique(Kind::Import, path.clone())?;
        }

        import_stmt.accept(self)
    }

    fn visit_class_field(&mut self, field: &mut Field) -> Result<(), Self::Error> {
        self.check_unique_child(Kind::Field, field.name.to_string())
            .map_err(|e| e.with_location(self.line_numbers_offset.get_location(&field.pos)))?;

        field.accept(self)
    }

    fn visit_class_decl_stmt(
        &mut self,
        class_decl_stmt: &mut ClassDeclStmt,
    ) -> Result<(), Self::Error> {
        self.parent_is_class = true;
        self.check_unique(Kind::Class, class_decl_stmt.name.to_string())
            .map_err(|e| {
                e.with_location(self.line_numbers_offset.get_location(&class_decl_stmt.pos))
            })?;

        class_decl_stmt.accept(self)?;

        self.parent_is_class = false;
        self.children.clear();

        Ok(())
    }

    fn visit_interface_decl_stmt(
        &mut self,
        interface_decl_stmt: &mut InterfaceDeclStmt,
    ) -> Result<(), Self::Error> {
        self.check_unique(Kind::Interface, interface_decl_stmt.name.to_string())
            .map_err(|e| {
                e.with_location(
                    self.line_numbers_offset
                        .get_location(&interface_decl_stmt.pos),
                )
            })?;

        interface_decl_stmt.accept(self)
    }
}
