use crate::compiler::scope::ScopeKind;
use crate::compiler::types::{Numeric, Type};
use crate::compiler::{types, Error, ScopeList};
use crate::module;
use crate::module::{Block, Fn, Instr, InstrKind, Module, Terminator};
use crate::syntax::{
    self, BinaryOp, Expr, ExprKind, FnDef, InferType, Node, NodeKind, Span, StmtKind,
};

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
                let ty = literal.kind.infer_type();

                block
                    .body
                    .push(Instr::new(literal.span, InstrKind::Const(literal.kind)));

                Ok(ty)
            }
            ExprKind::Binary(binary) => {
                let lhs = self.compile_expr(block, binary.left)?;
                let rhs = self.compile_expr(block, binary.right)?;

                if lhs != rhs {
                    return error!(
                        expr.span,
                        "invalid type for binary expression: {} and {} (should be the equal)",
                        lhs,
                        rhs
                    );
                }

                let instr_kind = match lhs.attr.numeric {
                    Some(Numeric::Int { signed, .. }) => match binary.op {
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
                    Some(Numeric::Float { .. }) => match binary.op {
                        BinaryOp::Add => InstrKind::FloatAdd,
                        BinaryOp::Sub => InstrKind::FloatSub,
                        BinaryOp::Mul => InstrKind::FloatMul,
                        BinaryOp::Div => InstrKind::FloatDiv,
                        _ => unimplemented!(),
                    },
                    _ => {
                        return error!(
                        expr.span,
                        "invalid non-numeric type for binary expression: {} (should be numeric)",
                        lhs
                    )
                    }
                };

                block.body.push(Instr::new(expr.span, instr_kind));

                Ok(lhs)
            }
            _ => unimplemented!(),
        }
    }

    fn to_concrete_type(&self, ty: &Type) -> Result<module::Type> {
        match_types!(
            ty,
            types::INT8 => module::Type::Int8,
            types::INT16 => module::Type::Int16,
            types::INT => module::Type::Int32,
            types::INT64 => module::Type::Int64,
            types::FLOAT => module::Type::Float32,
            types::FLOAT64 => module::Type::Float64,
            types::BOOL => module::Type::Int1,
            types::VOID => module::Type::Void
        );

        Err(Error::new(Span::default(), format!("invalid type: {}", ty)))
    }

    fn compile_type(&mut self, span: &Span, ty: syntax::Type) -> Result<Type> {
        if let Some(ty) = Type::from_name(&ty.name) {
            return Ok(ty);
        }

        error!(span.clone(), "unknown type: {}", ty.name)
    }

    fn compile_fn_def(&mut self, span: Span, fn_def: FnDef) -> Result<()> {
        let return_type = if let Some(ty) = fn_def.sig.return_type {
            self.compile_type(&span, ty)?
        } else {
            types::VOID
        };

        self.scopes.enter(ScopeKind::Fn);

        let mut block = Block::default();
        let body_len = fn_def.body.len();
        let mut inferred_return_type = None;

        for (i, stmt) in fn_def.body.into_iter().enumerate() {
            if block.term.is_some() {
                break;
            }

            match stmt.kind {
                StmtKind::Return(expr) => {
                    let ty = self.compile_expr(&mut block, expr)?;

                    inferred_return_type = Some(ty);
                    block.term = Some(Terminator::Return);
                }
                StmtKind::Expr(expr) => {
                    let ty = self.compile_expr(&mut block, expr)?;

                    if i == body_len - 1 && block.term.is_none() {
                        inferred_return_type = Some(ty);
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

        let inferred_return_type = inferred_return_type.unwrap_or(types::VOID);

        if return_type != inferred_return_type {
            return error!(
                span,
                "invalid return type '{}' for '{}(...)' (expected: {})",
                inferred_return_type,
                fn_def.name,
                return_type
            );
        }

        let return_type = self.to_concrete_type(&return_type)?;

        self.module.funcs.push(Fn {
            name: fn_def.name,
            body: vec![block],
            return_type,
        });

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
