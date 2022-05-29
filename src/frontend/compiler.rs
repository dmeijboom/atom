use crate::backend::{Block, Fn, Instr, InstrKind, Module, Terminator, Type as LlvmType};
use crate::frontend::scope::{Scope, ScopeKind, ScopeList};
use crate::frontend::syntax::{BinaryOp, Span};
use crate::frontend::typed_ast::{Expr, ExprKind, FnDef, NodeKind, Program, StmtKind};
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

struct Cursor<T> {
    index: usize,
    data: Vec<T>,
}

impl<T> Default for Cursor<T> {
    fn default() -> Self {
        Self {
            index: 0,
            data: vec![],
        }
    }
}

impl<T> Cursor<T> {
    pub fn new(data: Vec<T>) -> Self {
        Self { index: 0, data }
    }

    pub fn move_to(&mut self, index: usize) {
        self.index = index;
    }

    #[inline]
    pub fn head(&self) -> &T {
        &self.data[self.index]
    }
}

pub struct Compiler {
    module: Module,
    scopes: Cursor<Scope>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            module: Module::default(),
            scopes: Cursor::default(),
        }
    }

    fn compile_expr(&mut self, block: &mut Block, expr: Expr) -> Result<()> {
        match expr.kind {
            ExprKind::Ident(name) => {
                let (_, idx) = self.scopes.head().local(&name).unwrap();
                block.body.push(Instr::new(expr.span, InstrKind::Load(idx)));
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
        self.scopes.move_to(fn_def.scope);

        let mut block = Block::default();

        for stmt in fn_def.body {
            self.scopes.move_to(stmt.scope);

            assert!(block.term.is_none(), "block should not be terminated");

            match stmt.kind {
                StmtKind::Return(expr) => {
                    self.compile_expr(&mut block, expr)?;
                    block.term = Some(Terminator::Return);
                }
                StmtKind::Let(name, expr) => {
                    let (_, idx) = self.scopes.head().local(&name).unwrap();

                    self.compile_expr(&mut block, expr)?;

                    block
                        .body
                        .push(Instr::new(stmt.span, InstrKind::Store(idx)));
                }
                StmtKind::Expr(expr) => {
                    self.compile_expr(&mut block, expr)?;
                }
                _ => unimplemented!(),
            }
        }

        let mut locals = vec![];

        self.scopes.move_to(fn_def.scope);

        loop {
            for local in self.scopes.head().locals() {
                locals.push(self.to_concrete_type(&span, &local.ty)?);
            }

            if let Some(idx) = self.scopes.head().parent {
                self.scopes.move_to(idx);
                continue;
            }

            break;
        }

        let return_type = self.to_concrete_type(&span, &fn_def.return_type)?;

        self.module.funcs.push(Fn {
            name: fn_def.name,
            body: vec![block],
            return_type,
            locals,
        });

        Ok(())
    }

    pub fn compile(mut self, program: Program) -> Result<Module> {
        self.scopes = Cursor::new(program.scopes);

        for node in program.nodes {
            match node.kind {
                NodeKind::FnDef(fn_def) => self.compile_fn_def(node.span, fn_def)?,
            }
        }

        Ok(self.module)
    }
}
