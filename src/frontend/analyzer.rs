use crate::frontend::scope::{ScopeKind, ScopeList};
use crate::frontend::syntax::{BinaryOp, InferType, Span};
use crate::frontend::typed_ast::*;
use crate::frontend::types::Numeric;
use crate::frontend::{syntax, types, Error, Node, Type};

type Result<T> = std::result::Result<T, Error>;

macro_rules! error {
    ($span: expr, $($arg:tt)*) => {
        Err(Error::new($span, format!($($arg)*)))
    };
}

pub struct Analyzer {
    nodes: Vec<Node>,
    scopes: ScopeList,
    return_type: Option<Type>,
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            scopes: ScopeList::new(),
            return_type: None,
        }
    }

    fn get_type(&mut self, span: &Span, ty: &syntax::Type) -> Result<Type> {
        if let Some(ty) = Type::from_name(&ty.name) {
            return Ok(ty);
        }

        error!(span.clone(), "unknown type: {}", ty.name)
    }

    fn expr(&mut self, expr: syntax::Expr) -> Result<Expr> {
        Ok(match expr.kind {
            syntax::ExprKind::Ident(name) => {
                let (local, _) = self.scopes.head().local(&name).ok_or_else(|| {
                    Error::new(expr.span.clone(), format!("no such name: {}", name))
                })?;

                Expr::new(expr.span, local.ty.clone(), ExprKind::Ident(name))
            }
            syntax::ExprKind::Literal(literal) => {
                let ty = literal.kind.infer_type();
                Expr::new(expr.span, ty, ExprKind::Literal(literal.kind))
            }
            syntax::ExprKind::Binary(binary) => {
                let lhs = self.expr(binary.left)?;
                let rhs = self.expr(binary.right)?;

                if matches!(binary.op, BinaryOp::ShiftLeft | BinaryOp::ShiftRight)
                    && !matches!(lhs.ty.attr.numeric, Some(Numeric::Int { .. }))
                {
                    return error!(
                        expr.span,
                        "invalid types for binary shift expression: {} and {} (should be Int)",
                        lhs.ty,
                        rhs.ty
                    );
                }

                if lhs.ty != rhs.ty {
                    return error!(
                        expr.span,
                        "invalid type for binary expression: {} and {} (should be the equal)",
                        lhs.ty,
                        rhs.ty
                    );
                }

                if lhs.ty.attr.numeric.is_none() {
                    return error!(
                        expr.span,
                        "invalid type for binary expression: {} (should be numeric)", lhs.ty
                    );
                }

                // @FIXME: is the outcome always equal to the left hand side?

                Expr::new(
                    expr.span,
                    lhs.ty.clone(),
                    ExprKind::Binary(binary.op, lhs.into(), rhs.into()),
                )
            }
        })
    }

    fn body(&mut self, fn_body: Vec<syntax::Stmt>) -> Result<Vec<Stmt>> {
        let mut body = vec![];
        let body_size = fn_body.len();

        for (i, stmt) in fn_body.into_iter().enumerate() {
            match stmt.kind {
                syntax::StmtKind::If(if_stmt) => {
                    let expr = self.expr(if_stmt.cond)?;

                    if expr.ty != types::BOOL {
                        return error!(
                            stmt.span,
                            "invalid type for condition of if statement: {} (should be bool)",
                            expr.ty
                        );
                    }

                    let if_body = self.body(if_stmt.body)?;

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        StmtKind::If(expr, if_body),
                    ));
                }
                syntax::StmtKind::Assign(name, expr) => {
                    let expr = self.expr(expr)?;
                    let (local, _) = self.scopes.head().local(&name).ok_or_else(|| {
                        Error::new(expr.span.clone(), format!("no such name: {}", name))
                    })?;

                    if !local.mutable {
                        return error!(
                            stmt.span,
                            "unable to assign twice to mutable name: {}", name
                        );
                    }

                    if local.ty != expr.ty {
                        return error!(
                            stmt.span,
                            "invalid type '{}' for name '{}' (expected: {})",
                            local.ty,
                            name,
                            expr.ty
                        );
                    }

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        StmtKind::Assign(name, expr),
                    ))
                }
                syntax::StmtKind::Return(expr) => {
                    let expr = self.expr(expr)?;

                    if let Some(return_type) = &self.return_type {
                        if &expr.ty != return_type {
                            return error!(
                                expr.span,
                                "invalid return type '{}' (expected: {})", expr.ty, return_type
                            );
                        }
                    }

                    self.return_type = Some(expr.ty.clone());

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        StmtKind::Return(expr),
                    ));

                    break;
                }
                syntax::StmtKind::Let(let_stmt) => {
                    let expr = self.expr(let_stmt.value)?;

                    if let Some(ty) = let_stmt.ty {
                        let ty = self.get_type(&stmt.span, &ty)?;

                        if expr.ty != ty {
                            return error!(
                                stmt.span,
                                "invalid type '{}' for let statement (expected: {})", expr.ty, ty
                            );
                        }
                    }

                    let scope = self.scopes.head_mut();
                    scope.declare(let_stmt.name.clone(), expr.ty.clone(), let_stmt.mutable)?;

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        StmtKind::Let(let_stmt.name, expr),
                    ));
                }
                syntax::StmtKind::Expr(expr) => {
                    let expr = self.expr(expr)?;

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        if i == body_size - 1 {
                            self.return_type = Some(expr.ty.clone());
                            StmtKind::Return(expr)
                        } else {
                            StmtKind::Expr(expr)
                        },
                    ));
                }
                syntax::StmtKind::ExprEnd(expr) => {
                    let expr = self.expr(expr)?;

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        StmtKind::Expr(expr),
                    ));
                }
            }
        }

        Ok(body)
    }

    fn func(&mut self, span: &Span, fn_def: syntax::FnDef) -> Result<FnDef> {
        self.return_type = if let Some(ty) = fn_def.sig.return_type {
            Some(self.get_type(span, &ty)?)
        } else {
            None
        };

        self.scopes.enter(ScopeKind::Fn);
        let body = self.body(fn_def.body)?;
        let scope = self.scopes.exit();

        Ok(FnDef {
            name: fn_def.name,
            body,
            return_type: self.return_type.take().unwrap_or(types::VOID),
            scope,
        })
    }

    pub fn analyze(mut self, program: Vec<syntax::Node>) -> Result<Program> {
        for node in program {
            match node.kind {
                syntax::NodeKind::FnDef(fn_def) => {
                    let kind = NodeKind::FnDef(self.func(&node.span, fn_def)?);
                    self.nodes.push(Node::new(node.span, kind));
                }
            }
        }

        Ok(Program {
            scopes: self.scopes.consume(),
            nodes: self.nodes,
        })
    }
}
