use crate::backend::{Block, Fn, Instr, InstrKind, Module, Terminator, Type as LlvmType};
use crate::frontend::scope::{ScopeKind, ScopeList};
use crate::frontend::syntax::{BinaryOp, Span};
use crate::frontend::typed_ast::{Expr, ExprKind, FnDef, NodeKind, StmtKind};
use crate::frontend::types::{self, Numeric, Type};
use crate::frontend::{Error, Node};

type Result<T> = std::result::Result<T, Error>;

macro_rules! match_types {
    ($ty:expr, $($atom_type:expr => $llvm_type:expr),+) => {
        $(
            if $ty == &$atom_type {
                return Ok($llvm_type);
            }
        )+
    };
}

pub struct Compiler {
    module: Module,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            module: Module::default(),
        }
    }

    fn compile_expr(&mut self, block: &mut Block, expr: Expr) -> Result<()> {
        match expr.kind {
            ExprKind::Ident(name) => {
                unimplemented!()
                //let (local, idx) = self.scopes.head().local(&name).ok_or_else(|| {
                //    Error::new(expr.span.clone(), format!("no such name: {}", name))
                //})?;

                //block.body.push(Instr::new(expr.span, InstrKind::Load(idx)));
            }
            ExprKind::Literal(literal) => {
                block
                    .body
                    .push(Instr::new(expr.span, InstrKind::Const(literal)));
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let kind = match lhs.ty.attr.numeric {
                    Some(Numeric::Int { signed, .. }) => match op {
                        BinaryOp::Add => InstrKind::IntAdd,
                        BinaryOp::Sub => InstrKind::IntSub,
                        BinaryOp::Mul => InstrKind::IntMul,
                        BinaryOp::Div => {
                            if signed {
                                InstrKind::IntSDiv
                            } else {
                                InstrKind::IntUDiv
                            }
                        }
                        _ => unimplemented!(),
                    },
                    Some(Numeric::Float { .. }) => match op {
                        BinaryOp::Add => InstrKind::FloatAdd,
                        BinaryOp::Sub => InstrKind::FloatSub,
                        BinaryOp::Mul => InstrKind::FloatMul,
                        BinaryOp::Div => InstrKind::FloatDiv,
                        _ => unimplemented!(),
                    },
                    _ => unreachable!(),
                };

                self.compile_expr(block, *lhs)?;
                self.compile_expr(block, *rhs)?;

                block.body.push(Instr::new(expr.span, kind));
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn to_concrete_type(&self, span: &Span, ty: &Type) -> Result<LlvmType> {
        match_types!(
            ty,
            types::INT8 => LlvmType::Int8,
            types::INT16 => LlvmType::Int16,
            types::INT => LlvmType::Int32,
            types::INT64 => LlvmType::Int64,
            types::FLOAT => LlvmType::Float32,
            types::FLOAT64 => LlvmType::Float64,
            types::BOOL => LlvmType::Int1,
            types::VOID => LlvmType::Void
        );

        Err(Error::new(span.clone(), format!("invalid type: {}", ty)))
    }

    fn compile_fn_def(&mut self, span: Span, fn_def: FnDef) -> Result<()> {
        let mut block = Block::default();

        for stmt in fn_def.body {
            if block.term.is_some() {
                break;
            }

            match stmt.kind {
                StmtKind::Return(expr) => {
                    self.compile_expr(&mut block, expr)?;
                    block.term = Some(Terminator::Return);
                }
                StmtKind::Let(name, expr) => {
                    unimplemented!()
                    //self.compile_expr(&mut block, expr)?;
                    //
                    //block
                    //    .body
                    //    .push(Instr::new(stmt.span, InstrKind::Store(idx)));
                }
                StmtKind::Expr(expr) => {
                    self.compile_expr(&mut block, expr)?;
                }
                _ => unimplemented!(),
            }
        }

        //let mut locals = Vec::with_capacity(scope.locals().len());

        //for local in scope.locals() {
        //    locals.push(self.to_concrete_type(&local.ty)?);
        //}

        let return_type = self.to_concrete_type(&span, &fn_def.return_type)?;

        self.module.funcs.push(Fn {
            name: fn_def.name,
            body: vec![block],
            return_type,
            locals: vec![],
        });

        Ok(())
    }

    pub fn compile(mut self, program: Vec<Node>) -> Result<Module> {
        for node in program {
            match node.kind {
                NodeKind::FnDef(fn_def) => self.compile_fn_def(node.span, fn_def)?,
            }
        }

        Ok(self.module)
    }
}
