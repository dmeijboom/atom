use atom_ir::Location;

use crate::parser::ast::Stmt;
use crate::parser::Visitable;

use super::filesystem::{FileSystem, FileSystemCache};
use super::pre_processor::PreProcessor;
use super::result::Result;

const STD_SOURCES: [(&str, &str); 4] = [
    ("std.core", include_str!("../std/atom/std/core.atom")),
    ("std.io", include_str!("../std/atom/std/io.atom")),
    (
        "std.encoding.utf8",
        include_str!("../std/atom/std/encoding/utf8.atom"),
    ),
    (
        "std.encoding.json",
        include_str!("../std/atom/std/encoding/json.atom"),
    ),
];

pub struct Compiler {
    tree: Vec<Stmt>,
    // Unfortunately the parser doesn't expose line or column information so we're using a map of
    // newline positions to calculate the line number and column instead
    line_numbers_offset: Vec<usize>,
}

impl Compiler {
    pub fn new(tree: Vec<Stmt>, line_numbers_offset: Vec<usize>) -> Self {
        Self {
            tree,
            line_numbers_offset,
        }
    }

    pub fn compile(self) -> Result<()> {
        let mut cache = FileSystemCache::new();

        for (module_name, source) in STD_SOURCES {
            cache.add_module(module_name.to_string(), source);
        }

        let fs = FileSystem::new(cache);

        PreProcessor::new(fs, self.line_numbers_offset).pass(&self.tree)?;

        Ok(())
    }
}
