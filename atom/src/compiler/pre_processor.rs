use std::collections::HashMap;

use atom_ir::Location;

use crate::compiler::filesystem::File;
use crate::compiler::{parse_line_numbers_offset, CompileError};
use crate::parser::ast::{
    AssignStmt, BreakStmt, ClassDeclStmt, ElseStmt, Expr, ExprStmt, ExternFnDeclStmt, FnArg,
    FnDeclStmt, ForStmt, IfStmt, ImportStmt, InterfaceDeclStmt, LetDeclStmt, LetStmt,
    MixinDeclStmt, ModuleStmt, Pos, RaiseStmt, ReturnStmt, Stmt, UnsafeStmt, Variable,
};
use crate::parser::{self, Visitable, Visitor};

use super::filesystem::FileSystem;
use super::result::Result;

pub struct Global {
    pub name: String,
    pub module_name: String,
}

pub struct Module {
    pub name: String,
    pub tree: Vec<Stmt>,
    pub line_numbers_offset: Vec<usize>,
}

impl Module {
    pub fn new(name: String, tree: Vec<Stmt>, line_numbers_offset: Vec<usize>) -> Self {
        Self {
            name,
            tree,
            line_numbers_offset,
        }
    }
}

#[derive(Default)]
struct Scope {
    names: Vec<String>,
}

pub struct PreProcessor {
    pos: Pos,
    fs: FileSystem,
    scope: Vec<Scope>,
    modules: Vec<Module>,
    line_numbers_offset: Vec<usize>,
}

impl PreProcessor {
    pub fn new(fs: FileSystem, line_numbers_offset: Vec<usize>) -> Self {
        Self {
            fs,
            pos: 0..0,
            scope: vec![Scope::default()],
            modules: vec![],
            line_numbers_offset,
        }
    }

    fn import_module(&mut self, name: &str) -> Result<()> {
        let file = self
            .fs
            .read_file(name)
            .map_err(|e| CompileError::new(format!("{}", e)))?;
        let tree = parser::parse(file.source())
            .map_err(|e| CompileError::new(format!("ParseError: {}", e)))?;
        let line_numbers_offset = parse_line_numbers_offset(file.source());

        self.modules
            .push(Module::new(name.to_string(), tree, line_numbers_offset));

        Ok(())
    }

    pub fn pass(&mut self, tree: &[Stmt]) -> Result<()> {
        for stmt in tree {
            self.visit_stmt(stmt)?;
        }

        Ok(())
    }
}

impl Visitor for PreProcessor {
    type Error = CompileError;

    fn visit_name(&mut self, name: &str) -> Result<()> {
        let scope = self.scope.last_mut().unwrap();

        if scope.names.contains(&name.to_string()) {
            return Err(
                CompileError::new(format!("unable to redefine name: {}", name))
                    .with_location(Location::from_offset(&self.line_numbers_offset, &self.pos)),
            );
        }

        scope.names.push(name.to_string());

        Ok(())
    }

    fn visit_import_stmt(&mut self, import_stmt: &ImportStmt) -> Result<()> {
        // Import module (only if it hasn't been imported yet)
        let mut components = import_stmt.name.split('.').collect::<Vec<_>>();
        let name = components.pop().ok_or_else(|| {
            CompileError::new(format!("invalid import path: {}", import_stmt.name))
        })?;
        let module_name = components.join(".");

        if !self.modules.iter().any(|module| module_name == name) {
            self.import_module(&module_name)?;
        }

        self.visit_name(&name)?;

        Ok(())
    }

    fn visit_class_decl_stmt(&mut self, class_decl_stmt: &ClassDeclStmt) -> Result<()> {
        self.visit_name(&class_decl_stmt.name)?;

        self.scope.push(Scope::default());

        for field in class_decl_stmt.fields.iter() {
            self.visit_name(&field.name)?;
        }

        self.visit_fn_decl_stmt_list(&class_decl_stmt.funcs)?;
        self.visit_extern_fn_decl_stmt_list(&class_decl_stmt.extern_funcs)?;

        self.scope.pop();

        Ok(())
    }

    fn visit_mixin_decl_stmt(&mut self, mixin_decl_stmt: &MixinDeclStmt) -> Result<()> {
        self.visit_name(&mixin_decl_stmt.name)?;

        self.scope.push(Scope::default());

        self.visit_fn_decl_stmt_list(&mixin_decl_stmt.funcs)?;
        self.visit_extern_fn_decl_stmt_list(&mixin_decl_stmt.extern_funcs)?;

        self.scope.pop();

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        self.pos = stmt.pos();

        stmt.walk(self)
    }
}
