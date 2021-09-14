use atom_ir::Location;

use crate::compiler::filesystem::File;
use crate::compiler::{parse_line_numbers_offset, CompileError};
use crate::parser::ast::{Expr, FnArg, IfStmt, ImportStmt, Pos, Stmt, Variable};
use crate::parser::{self, Visitable, Visitor};

use super::filesystem::FileSystem;
use super::result::Result;

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

pub struct PreProcessor {
    pos: Pos,
    names: Vec<String>,
    modules: Vec<Module>,
    fs: FileSystem,
    line_numbers_offset: Vec<usize>,
}

impl PreProcessor {
    pub fn new(fs: FileSystem, line_numbers_offset: Vec<usize>) -> Self {
        Self {
            fs,
            pos: 0..0,
            names: vec![],
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

    pub fn pass(&mut self, tree: Vec<Stmt>) -> Result<()> {
        for stmt in tree {
            stmt.walk(self)?;
        }

        Ok(())
    }
}

impl Visitor for PreProcessor {
    type Error = CompileError;

    fn visit_name(&mut self, name: &str) -> Result<()> {
        if self.names.contains(&name.to_string()) {
            return Err(CompileError::new(format!(
                "unable to redefine name: {}",
                name
            )));
        }

        self.names.push(name.to_string());

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

        // @TODO: Import the exact item from the module
        //let module = self
        //    .modules
        //    .iter()
        //    .find(|module| module.name == module_name)
        //    .unwrap();

        Ok(())
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        self.pos = stmt.pos();

        stmt.walk(self)
    }
}
