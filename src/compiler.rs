use std::fmt::{Display, Formatter};

use crate::module::{Block, Fn, Instr, InstrKind, Module};
use crate::syntax::{Expr, ExprKind, FnDef, Node, NodeKind, Span, StmtKind};

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub message: String,
}

impl Error {
    pub fn new(span: Span, message: String) -> Self {
        Self { span, message }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CompileError: {}", self.message)
    }
}

type Result<T> = std::result::Result<T, Error>;

pub struct Compiler {
    module: Module,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            module: Module::default(),
        }
    }

    fn validate(&self, program: &[Node]) -> Result<()> {
        let mut names = vec![];

        for node in program {
            match &node.kind {
                NodeKind::FnDef(fn_def) => {
                    if names.contains(&&fn_def.name) {
                        return Err(Error::new(
                            node.span.clone(),
                            format!("unable to redefine Fn '{}'", fn_def.name,),
                        ));
                    }

                    names.push(&fn_def.name);
                }
            }
        }

        Ok(())
    }

    fn compile_expr(&mut self, block: &mut Block, expr: Expr) -> Result<()> {
        match expr.kind {
            ExprKind::Literal(literal) => block
                .body
                .push(Instr::new(literal.span, InstrKind::Const(literal.kind))),
        }

        Ok(())
    }

    fn compile_fn_def(&mut self, span: Span, fn_def: FnDef) -> Result<()> {
        let mut func = Fn {
            name: fn_def.name,
            body: vec![],
        };

        let mut block = Block::default();

        for stmt in fn_def.body {
            match stmt.kind {
                StmtKind::ExprEnd(expr) => self.compile_expr(&mut block, expr)?,
                _ => unimplemented!(),
            }
        }

        func.body.push(block);
        self.module.funcs.push(func);

        Ok(())
    }

    pub fn compile(mut self, program: Vec<Node>) -> Result<Module> {
        self.validate(&program)?;

        for node in program {
            match node.kind {
                NodeKind::FnDef(fn_def) => self.compile_fn_def(node.span, fn_def)?,
            }
        }

        Ok(self.module)
    }
}
