use std::str::FromStr;

use crate::compiler::scope::ScopeKind;
use crate::compiler::{Error, ScopeList};
use crate::module::{BasicType, Block, Fn, Instr, InstrKind, Module, Terminator, Type};
use crate::syntax;
use crate::syntax::{Expr, ExprKind, FnDef, LiteralKind, Node, NodeKind, Span, StmtKind};

type Result<T> = std::result::Result<T, Error>;

macro_rules! error {
    ($span: expr, $($arg:tt)*) => {
        Err(Error::new($span, format!($($arg)*)))
    };
}

pub struct Compiler {
    module: Module,
    scopes: ScopeList,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            module: Module::default(),
            scopes: ScopeList::new(),
        }
    }

    fn validate(&self, program: &[Node]) -> Result<()> {
        let mut names = vec![];

        for node in program {
            match &node.kind {
                NodeKind::FnDef(fn_def) => {
                    if names.contains(&&fn_def.name) {
                        return error!(
                            node.span.clone(),
                            "unable to redefine Fn '{}'", fn_def.name,
                        );
                    }

                    names.push(&fn_def.name);
                }
            }
        }

        Ok(())
    }

    fn compile_expr(&mut self, block: &mut Block, expr: Expr) -> Result<Type> {
        match expr.kind {
            ExprKind::Literal(literal) => {
                let ty = match &literal.kind {
                    LiteralKind::Int(_) => Type::Basic(BasicType::Int),
                    LiteralKind::Float(_) => Type::Basic(BasicType::Float),
                    _ => unimplemented!(),
                };

                block
                    .body
                    .push(Instr::new(literal.span, InstrKind::Const(literal.kind)));

                Ok(ty)
            }
        }
    }

    fn compile_type(&mut self, span: Span, ty: syntax::Type) -> Result<Type> {
        if let Ok(ty) = BasicType::from_str(&ty.name) {
            return Ok(Type::Basic(ty));
        }

        match ty.name {
            _ => error!(span, "unknown type: {}", ty.name),
        }
    }

    fn compile_fn_def(&mut self, span: Span, fn_def: FnDef) -> Result<()> {
        let mut func = Fn {
            name: fn_def.name,
            body: vec![],
            return_type: if let Some(ty) = fn_def.sig.return_type {
                self.compile_type(span, ty)?
            } else {
                Type::Basic(BasicType::Void)
            },
        };

        self.scopes.enter(ScopeKind::Fn);

        let mut block = Block::default();
        let body_len = fn_def.body.len();

        for (i, stmt) in fn_def.body.into_iter().enumerate() {
            match stmt.kind {
                StmtKind::Return(expr) => {
                    let span = expr.span.clone();
                    let ty = self.compile_expr(&mut block, expr)?;

                    if ty != func.return_type {
                        return error!(
                            span,
                            "invalid return type '{}' for function '{}' (expected '{}')",
                            ty,
                            func.name,
                            func.return_type,
                        );
                    }

                    if block.term.is_some() {
                        return error!(span, "block already terminated");
                    }

                    block.term = Some(Terminator::Return);
                }
                StmtKind::Expr(expr) => {
                    let span = expr.span.clone();
                    let ty = self.compile_expr(&mut block, expr)?;

                    if i == body_len - 1 && block.term.is_none() {
                        if ty != func.return_type {
                            return error!(
                                span,
                                "invalid return type '{}' for function '{}' (expected '{}')",
                                ty,
                                func.name,
                                func.return_type,
                            );
                        }

                        block.term = Some(Terminator::Return);
                    }
                }
                StmtKind::ExprEnd(expr) => {
                    self.compile_expr(&mut block, expr)?;
                }
                _ => unimplemented!(),
            }
        }

        self.scopes.exit();

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
