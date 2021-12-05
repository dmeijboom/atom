use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;
use std::sync::RwLock;

use crate::syntax::parser;
use crate::syntax::Stmt;

use super::codegen::CodeGenerator;
use super::error::{CompileError, Result};
use super::filesystem::{FileSystem, FileSystemCache};
use super::frontend::Frontend;
use super::line_number_offset::LineNumberOffset;
use super::mir;
use super::module::Module;
use super::optimizers::{
    call_void, pre_compute_labels, remove_store_load_single_use, remove_type_cast, reorder_locals,
    replace_load_with_const, tail_call,
};

fn validate_unique(names: &[(&str, &str)]) -> Result<()> {
    for (i, (_, name)) in names.iter().enumerate() {
        let other = names
            .iter()
            .enumerate()
            .find(|(other_index, (_, other_name))| *other_index != i && name == other_name);

        if let Some((_, (typename, name))) = other {
            return Err(CompileError::new(format!(
                "unable to redefine {}: {}",
                typename, name
            )));
        }
    }

    Ok(())
}

const STD_SOURCES: [(&str, &str); 10] = [
    ("std.sys", include_str!("../std/atom/std/sys.atom")),
    ("std.core", include_str!("../std/atom/std/core.atom")),
    ("std.map", include_str!("../std/atom/std/map.atom")),
    ("std.io", include_str!("../std/atom/std/io.atom")),
    ("std.math", include_str!("../std/atom/std/math.atom")),
    (
        "std.encoding.utf8",
        include_str!("../std/atom/std/encoding/utf8.atom"),
    ),
    (
        "std.encoding.binary",
        include_str!("../std/atom/std/encoding/binary.atom"),
    ),
    (
        "std.encoding.json",
        include_str!("../std/atom/std/encoding/json.atom"),
    ),
    ("testing", include_str!("../std/atom/testing.atom")),
    ("__prelude", include_str!("../std/atom/prelude.atom")),
];

pub struct Compiler {
    fs: FileSystem,
    optimize: bool,
    module: Module,
    tree: Vec<Stmt>,
    modules: Rc<RwLock<HashMap<String, Module>>>,
    line_numbers_offset: LineNumberOffset,
}

impl Compiler {
    pub fn new(tree: Vec<Stmt>, line_numbers_offset: LineNumberOffset, optimize: bool) -> Self {
        let mut cache = FileSystemCache::new();

        for (module_name, source) in STD_SOURCES {
            cache.add_module(module_name.to_string(), source);
        }

        let fs = FileSystem::new(cache);

        Self {
            tree,
            fs,
            optimize,
            module: Module::new(),
            line_numbers_offset,
            modules: Rc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn add_lookup_path(&mut self, path: impl AsRef<Path>) {
        self.fs.add_path(path.as_ref().to_path_buf());
    }

    fn merge_module(&mut self, mut module: Module) {
        for (key, mixin_stmt) in module.mixins.drain() {
            self.module.mixins.insert(key, mixin_stmt);
        }

        for (key, element) in module.imports.drain(..) {
            self.module.imports.insert(key, element);
        }

        for (name, class) in module.classes.drain(..) {
            self.module.classes.insert(name, class);
        }

        for (name, interface) in module.interfaces.drain(..) {
            self.module.interfaces.insert(name, interface);
        }

        for (name, function) in module.functions.drain(..) {
            self.module.functions.insert(name, function);
        }

        for (name, element) in module.exports.drain() {
            self.module.exports.insert(name, element);
        }
    }

    fn fork(
        &self,
        module_name: String,
        tree: Vec<Stmt>,
        line_numbers_offset: LineNumberOffset,
    ) -> Self {
        Self {
            fs: self.fs.clone(),
            optimize: self.optimize,
            module: Module::with_name(module_name),
            tree,
            line_numbers_offset,
            modules: Rc::clone(&self.modules),
        }
    }

    fn prelude_pass(&mut self) -> Result<()> {
        // Core module shouldn't include the prelude as that would create an infinite loop
        if self.module.name == "__prelude"
            || self.module.name == "std.core"
            || self.module.name == "std.map"
            || self.module.name == "std.math"
            || self.module.name == "std.encoding.binary"
        {
            return Ok(());
        }

        let prelude = self.parse_and_compile("__prelude".to_string())?;

        self.merge_module(prelude);

        Ok(())
    }

    fn parse_and_compile(&self, name: String) -> Result<Module> {
        let file = self
            .fs
            .read_file(&name)
            .map_err(|e| CompileError::new(format!("failed to read '{}': {}", name, e)))?;
        let filename = file.name().to_str().map(|filename| filename.to_string());
        let tree = parser::parse(file.source()).map_err(|err| match filename.clone() {
            None => err.into(),
            Some(filename) => CompileError::from(err).with_filename(filename),
        })?;
        let line_numbers_offset = LineNumberOffset::parse(file.source());
        let mut module = self
            .fork(name, tree, line_numbers_offset)
            .compile()
            .map_err(|err| {
                if let Some(filename) = filename.clone() {
                    if err.filename.is_none() {
                        return err.with_filename(filename);
                    }
                }

                err
            })?;

        module.filename = filename;

        Ok(module)
    }

    fn import_name(&mut self, name: &str) -> Result<()> {
        let mut components = name.split('.').collect::<Vec<_>>();

        if components.len() < 2 {
            return Err(CompileError::new(format!("invalid import path: {}", name)));
        }

        let name = components.pop().unwrap();
        let module_name = components.join(".");
        let module_exist = { self.modules.read().unwrap().contains_key(&module_name) };

        if !module_exist {
            let module = self.parse_and_compile(module_name.to_string())?;

            {
                self.modules
                    .write()
                    .unwrap()
                    .insert(module_name.clone(), module);
            }
        }

        let guard = self.modules.read().unwrap();
        let module = guard.get(&module_name).unwrap();

        if let Some(elem) = module.exports.get(name) {
            if self.module.imports.contains_key(name) {
                return Err(CompileError::new(format!(
                    "unable to redefine global: {}",
                    name
                )));
            }

            self.module.imports.insert(name.to_string(), elem.clone());

            return Ok(());
        } else if let Some(mixin) = module.mixins.get(name) {
            // As mixins are being used at compile time we need to import them instead of delegating that to the vm
            self.module.mixins.insert(mixin.name.clone(), mixin.clone());

            return Ok(());
        }

        Err(CompileError::new(format!(
            "failed to import unknown name '{}' from: {}",
            name, module.name
        )))
    }

    fn imports_pass(&mut self) -> Result<()> {
        loop {
            let index = self
                .tree
                .iter()
                .position(|stmt| matches!(stmt, Stmt::Import(_)));

            if let Some(index) = index {
                if let Stmt::Import(import_stmt) = self.tree.remove(index) {
                    for path in import_stmt.path {
                        self.import_name(&path).map_err(|e| {
                            e.with_location(self.line_numbers_offset.get_location(&import_stmt.pos))
                        })?;
                    }

                    continue;
                }
            }

            break;
        }

        Ok(())
    }

    fn name_validation_pass(&self) -> Result<()> {
        let mut names: Vec<(&str, &str)> = vec![];

        for stmt in self.tree.iter() {
            match stmt {
                Stmt::FnDecl(fn_decl_stmt) => names.push(("function", &fn_decl_stmt.name)),
                Stmt::Import(import_stmt) => {
                    for path in import_stmt.path.iter() {
                        names.push(("import", path))
                    }
                }
                Stmt::ClassDecl(class_decl_stmt) => {
                    let mut class_names: Vec<(&str, &str)> = vec![];

                    for field in class_decl_stmt.fields.iter() {
                        class_names.push(("field", &field.name));
                    }

                    for func in class_decl_stmt.funcs.iter() {
                        class_names.push(("function", &func.name));
                    }

                    validate_unique(&class_names).map_err(|e| {
                        CompileError::new(format!(
                            "{} for class {}",
                            e.message(),
                            class_decl_stmt.name
                        ))
                    })?;

                    names.push(("class", &class_decl_stmt.name))
                }
                Stmt::InterfaceDecl(interface_decl_stmt) => {
                    names.push(("interface", &interface_decl_stmt.name))
                }
                _ => {}
            }
        }

        validate_unique(&names)
    }

    fn mixins_pass(&mut self) -> Result<()> {
        // Import all local mixins
        loop {
            let index = self
                .tree
                .iter()
                .position(|stmt| matches!(stmt, Stmt::MixinDecl(_)));

            if let Some(index) = index {
                if let Stmt::MixinDecl(mixin_decl_stmt) = self.tree.remove(index) {
                    self.module
                        .mixins
                        .insert(mixin_decl_stmt.name.clone(), mixin_decl_stmt);

                    continue;
                }
            }

            break;
        }

        // Expand AST for classes that extends mixins
        for stmt in self.tree.iter_mut() {
            if let Stmt::ClassDecl(class_decl_stmt) = stmt {
                for name in class_decl_stmt.extends.iter() {
                    let mixin = self
                        .module
                        .mixins
                        .get(name)
                        .ok_or_else(|| CompileError::new(format!("no such mixin: {}", name)))?;

                    class_decl_stmt.funcs.append(&mut mixin.funcs.clone());
                }
            }
        }

        Ok(())
    }

    fn module_name_pass(&mut self) -> Result<()> {
        loop {
            let index = self
                .tree
                .iter()
                .position(|stmt| matches!(stmt, Stmt::Module(_)));

            if let Some(index) = index {
                if index != 0 {
                    return Err(CompileError::new(
                        "module statement must be the first statement in a file and can only exist once".to_string(),
                    ));
                }

                if let Stmt::Module(module_stmt) = self.tree.remove(index) {
                    self.module.name = module_stmt.name;
                }

                continue;
            }

            break;
        }

        Ok(())
    }

    pub fn compile(mut self) -> Result<Module> {
        self.module_name_pass()?;
        self.prelude_pass()?;
        self.name_validation_pass()?;
        self.imports_pass()?;
        self.mixins_pass()?;

        let compiler = mir::Compiler::new(&self.line_numbers_offset);
        let mir = compiler.compile(&self.tree)?;

        let frontend = Frontend::new(&mut self.module, &mir);
        frontend.compile()?;

        let backend = CodeGenerator::new(
            &mut self.module,
            &mir,
            if self.optimize {
                // The order of optimizations really matters
                vec![
                    remove_type_cast::optimize,
                    tail_call::optimize,
                    call_void::optimize,
                    replace_load_with_const::optimize,
                    remove_store_load_single_use::optimize,
                    reorder_locals::optimize,
                    // Should always be the latest instruction otherwise the pre-computed label index
                    // would point to the wrong instruction
                    pre_compute_labels::optimize,
                ]
            } else {
                vec![]
            },
        );
        backend.compile()?;

        Ok(self.module)
    }

    pub fn compile_all(self) -> Result<Vec<Module>> {
        let modules = Rc::clone(&self.modules);
        let module = self.compile()?;
        let mut modules = Rc::try_unwrap(modules)
            .map_err(|_| {
                CompileError::new("unable to call compile_all more than once".to_string())
            })?
            .into_inner()
            .unwrap();

        // Sort modules based on their dependencies (@TODO: better error reporting)
        let mut output = vec![];
        let mut resolved = vec![];

        while !modules.is_empty() {
            let name = modules
                .iter()
                .find(|(_, module)| {
                    !module
                        .imports
                        .iter()
                        .any(|(_, import)| !resolved.contains(&import.id.module))
                })
                .map(|(name, _)| name)
                .ok_or_else(|| {
                    CompileError::new(format!(
                        "circular dependency not allowed: {}",
                        modules
                            .iter()
                            .map(|(name, _)| name.clone())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ))
                })?
                .clone();

            let module = modules.remove(&name).unwrap();

            resolved.push(name);
            output.push(module);
        }

        output.push(module);

        Ok(output)
    }
}
