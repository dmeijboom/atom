use crate::backend::{Block, Fn, Instr, InstrId, InstrKind, Module, Terminator, Type as LlvmType};
use crate::frontend::scope::{find_local, Cursor, CursorData, Scope};
use crate::frontend::syntax::{BinaryOp, LiteralKind, LogicalOp, Span};
use crate::frontend::typed_ast::{Expr, ExprKind, FnDef, NodeKind, Program, Stmt, StmtKind};
use crate::frontend::types::{self, Type, TypeKind};
use crate::frontend::Error;

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

pub struct Compiler<'c> {
    id: InstrId,
    module: Module,
    cursor: Cursor<'c, Scope>,
}

impl Compiler<'_> {
    pub fn new() -> Self {
        Self {
            id: 0,
            cursor: Cursor::default(),
            module: Module::default(),
        }
    }

    fn new_id(&mut self) -> InstrId {
        let id = self.id;
        self.id += 1;
        id
    }

    fn compile_expr(&mut self, block: &mut Block, expr: Expr) -> Result<()> {
        match expr.kind {
            ExprKind::Fn(name) => {
                block.body.push(Instr::new(
                    self.new_id(),
                    expr.span,
                    InstrKind::LoadFn(name),
                ));
            }
            ExprKind::Name(name) => {
                let (_, idx) =
                    find_local(self.cursor.as_slice(), self.cursor.index, &name).unwrap();

                block
                    .body
                    .push(Instr::new(self.new_id(), expr.span, InstrKind::Load(idx)));
            }
            ExprKind::Call(callee, args) => {
                let arg_count = args.len();

                for arg in args {
                    self.compile_expr(block, arg)?;
                }

                self.compile_expr(block, *callee)?;

                block.body.push(Instr::new(
                    self.new_id(),
                    expr.span,
                    InstrKind::Call(arg_count),
                ));
            }
            ExprKind::Literal(literal) => match literal {
                LiteralKind::Int(val) => {
                    let signed = matches!(expr.ty.kind, TypeKind::Int { signed, .. } if signed);

                    block.body.push(Instr::new(
                        self.new_id(),
                        expr.span,
                        if signed {
                            InstrKind::ConstInt(val)
                        } else {
                            InstrKind::ConstUint(val as u64)
                        },
                    ));
                }
                LiteralKind::Bool(val) => block.body.push(Instr::new(
                    self.new_id(),
                    expr.span,
                    InstrKind::ConstBool(val),
                )),
                LiteralKind::Float(val) => block.body.push(Instr::new(
                    self.new_id(),
                    expr.span,
                    InstrKind::ConstFloat(val),
                )),
                LiteralKind::String(_) => unimplemented!(),
            },
            ExprKind::Binary(op, lhs, rhs) => {
                let kind = match lhs.ty.kind {
                    TypeKind::Int { signed, .. } => match op {
                        BinaryOp::Add => InstrKind::IntAdd,
                        BinaryOp::Sub => InstrKind::IntSub,
                        BinaryOp::Mul => InstrKind::IntMul,
                        BinaryOp::Div if signed => InstrKind::IntSDiv,
                        BinaryOp::Div if !signed => InstrKind::IntUDiv,
                        BinaryOp::ShiftLeft => InstrKind::IntShl,
                        BinaryOp::ShiftRight if signed => InstrKind::IntSShr,
                        BinaryOp::ShiftRight if !signed => InstrKind::IntUShr,
                        BinaryOp::Lte if signed => InstrKind::IntSLte,
                        BinaryOp::Lte if !signed => InstrKind::IntULte,
                        BinaryOp::Lt if signed => InstrKind::IntSLt,
                        BinaryOp::Lt if !signed => InstrKind::IntULt,
                        BinaryOp::Gte if signed => InstrKind::IntSGte,
                        BinaryOp::Gte if !signed => InstrKind::IntUGte,
                        BinaryOp::Gt if signed => InstrKind::IntSGt,
                        BinaryOp::Gt if !signed => InstrKind::IntUGt,
                        BinaryOp::Eq => InstrKind::IntEq,
                        BinaryOp::Neq => InstrKind::IntNeq,
                        _ => unimplemented!(),
                    },
                    TypeKind::Float { .. } => match op {
                        BinaryOp::Add => InstrKind::FloatAdd,
                        BinaryOp::Sub => InstrKind::FloatSub,
                        BinaryOp::Mul => InstrKind::FloatMul,
                        BinaryOp::Div => InstrKind::FloatDiv,
                        BinaryOp::Lte => InstrKind::FloatLte,
                        BinaryOp::Lt => InstrKind::FloatLt,
                        BinaryOp::Gte => InstrKind::FloatGte,
                        BinaryOp::Gt => InstrKind::FloatGt,
                        BinaryOp::Eq => InstrKind::FloatEq,
                        BinaryOp::Neq => InstrKind::FloatNeq,
                        _ => unreachable!(),
                    },
                    TypeKind::Bool => match op {
                        BinaryOp::Eq => InstrKind::IntEq,
                        BinaryOp::Neq => InstrKind::IntNeq,
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };

                self.compile_expr(block, *lhs)?;
                self.compile_expr(block, *rhs)?;

                block.body.push(Instr::new(self.new_id(), expr.span, kind));
            }
            ExprKind::Logical(op, lhs, rhs) => {
                self.compile_expr(block, *lhs)?;
                self.compile_expr(block, *rhs)?;

                block.body.push(Instr::new(
                    self.new_id(),
                    expr.span,
                    match op {
                        LogicalOp::And => InstrKind::And,
                        LogicalOp::Or => InstrKind::Or,
                    },
                ));
            }
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

        // Functions are always being stored as function pointers in atom
        if let TypeKind::Function(fn_type) = &ty.kind {
            return Ok(LlvmType::Ptr(Box::new(LlvmType::Fn(
                Box::new(self.to_concrete_type(&Span::default(), &fn_type.return_type)?),
                vec![],
            ))));
        }

        Err(Error::new(span.clone(), format!("invalid type {}", ty)))
    }

    fn compile_assign(
        &mut self,
        block: &mut Block,
        span: Span,
        name: String,
        expr: Expr,
    ) -> Result<()> {
        let (_, idx) = find_local(self.cursor.as_slice(), self.cursor.index, &name).unwrap();

        self.compile_expr(block, expr)?;

        block
            .body
            .push(Instr::new(self.new_id(), span, InstrKind::Store(idx)));

        Ok(())
    }

    fn compile_body(&mut self, block: &mut Block, body: Vec<Stmt>) -> Result<()> {
        for stmt in body {
            self.cursor.move_to(stmt.scope);

            assert!(block.term.is_none(), "block should not be terminated");

            // @TODO: how to deal with dead code?
            if block.is_terminated() {
                return Ok(());
            }

            match stmt.kind {
                StmtKind::Let(name, expr) => {
                    if let Some(expr) = expr {
                        self.compile_assign(block, stmt.span, name, expr)?;
                    }
                }
                StmtKind::Assign(name, expr) => {
                    self.compile_assign(block, stmt.span, name, expr)?;
                }
                StmtKind::If(cond, body, alt) => {
                    self.compile_expr(block, cond)?;

                    let mut then = Block::default();
                    self.compile_body(&mut then, body)?;

                    let mut or_else = Block::default();
                    self.compile_body(&mut or_else, alt)?;

                    block.body.push(Instr::new(
                        self.new_id(),
                        stmt.span,
                        InstrKind::Branch(then, or_else),
                    ));
                }
                StmtKind::Return(expr) => {
                    if let Some(expr) = expr {
                        self.compile_expr(block, expr)?;
                    }

                    block.term = Some(Terminator::Return);
                }
                StmtKind::Expr(expr) => {
                    self.compile_expr(block, expr)?;
                }
            }
        }

        Ok(())
    }

    fn compile_fn_def(&mut self, span: Span, fn_def: FnDef) -> Result<()> {
        self.cursor.move_to(fn_def.scope);

        let mut body = Block::default();
        self.compile_body(&mut body, fn_def.body)?;

        if !body.is_terminated() {
            if fn_def.return_type != types::VOID {
                return Err(Error::new(
                    span,
                    format!("fn '{}(...)' must return a value", fn_def.name),
                ));
            }

            body.term = Some(Terminator::Return);
        }

        let mut locals = vec![];

        self.cursor.move_to(fn_def.scope);

        loop {
            for local in self.cursor.head().locals() {
                // Defaulting to `Int1` should be fine as it's guaranteed to be unused anyway
                locals.push(
                    local
                        .ty
                        .as_ref()
                        .map(|ty| self.to_concrete_type(&span, ty))
                        .unwrap_or_else(|| Ok(LlvmType::Int1))?,
                );
            }

            if !self.cursor.next_sibling(fn_def.scope) {
                break;
            }
        }

        let return_type = self.to_concrete_type(&span, &fn_def.return_type)?;

        self.module.funcs.push(Fn {
            name: fn_def.name,
            body,
            return_type,
            locals,
        });

        Ok(())
    }

    pub fn compile(mut self, program: Program) -> Result<Module> {
        self.cursor = Cursor::new(CursorData::Owned(program.scopes));

        for node in program.nodes {
            match node.kind {
                NodeKind::FnDef(fn_def) => self.compile_fn_def(node.span, fn_def)?,
            }
        }

        Ok(self.module)
    }
}
