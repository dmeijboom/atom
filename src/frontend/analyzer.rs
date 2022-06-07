use crate::frontend::scope::{ScopeKind, ScopeList};
use crate::frontend::syntax::{Alt, BinaryOp, InferType, Span};
use crate::frontend::typed_ast::*;
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
                let (local, _) = self.scopes.local(&name).ok_or_else(|| {
                    Error::new(expr.span.clone(), format!("no such name: {}", name))
                })?;

                Expr::new(expr.span, local.ty.clone(), ExprKind::Ident(name))
            }
            syntax::ExprKind::Literal(literal) => {
                let ty = literal.kind.infer_type();
                Expr::new(expr.span, ty, ExprKind::Literal(literal.kind))
            }
            syntax::ExprKind::Logical(logical) => {
                let lhs = self.expr(logical.left)?;
                let rhs = self.expr(logical.right)?;

                if lhs.ty != types::BOOL || rhs.ty != types::BOOL {
                    return error!(
                        expr.span,
                        "invalid type for logical expression: {} and {} (should be Bool)",
                        lhs.ty,
                        rhs.ty
                    );
                }

                Expr::new(
                    expr.span,
                    types::BOOL,
                    ExprKind::Logical(logical.op, lhs.into(), rhs.into()),
                )
            }
            syntax::ExprKind::Binary(binary) => {
                let lhs = self.expr(binary.left)?;
                let rhs = self.expr(binary.right)?;
                let (valid, output) = match binary.op {
                    BinaryOp::ShiftLeft | BinaryOp::ShiftRight => (
                        lhs.ty == rhs.ty && lhs.ty.is_int() && rhs.ty.is_int(),
                        Some(types::INT),
                    ),
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => (
                        lhs.ty == rhs.ty && lhs.ty.is_numeric() && rhs.ty.is_numeric(),
                        None,
                    ),
                    BinaryOp::Lte
                    | BinaryOp::Lt
                    | BinaryOp::Gte
                    | BinaryOp::Gt
                    | BinaryOp::Eq
                    | BinaryOp::Neq => (
                        lhs.ty == rhs.ty && lhs.ty.attr.primitive && rhs.ty.attr.primitive,
                        Some(types::BOOL),
                    ),
                };

                if !valid {
                    return error!(
                        expr.span,
                        "invalid types for binary expression: {} and {}", lhs.ty, rhs.ty,
                    );
                }

                Expr::new(
                    expr.span,
                    output.unwrap_or_else(|| lhs.ty.clone()),
                    ExprKind::Binary(binary.op, lhs.into(), rhs.into()),
                )
            }
        })
    }

    fn if_else(&mut self, if_stmt: syntax::If) -> Result<Stmt> {
        let expr = self.expr(if_stmt.cond)?;

        if expr.ty != types::BOOL {
            return error!(
                if_stmt.span,
                "invalid type for condition of if statement: {} (should be Bool)", expr.ty
            );
        }

        self.scopes.enter(ScopeKind::Local);
        let if_body = self.body(if_stmt.body)?;
        self.scopes.leave();

        let else_body = match if_stmt.alt {
            None => vec![],
            Some(alt) => match alt {
                Alt::ElseIf(else_if) => {
                    vec![self.if_else(*else_if)?]
                }
                Alt::Else(body) => {
                    self.scopes.enter(ScopeKind::Local);
                    let else_body = self.body(body)?;
                    self.scopes.leave();

                    else_body
                }
            },
        };

        Ok(Stmt::new(
            if_stmt.span,
            self.scopes.head().id,
            StmtKind::If(expr, if_body, else_body),
        ))
    }

    fn body(&mut self, fn_body: Vec<syntax::Stmt>) -> Result<Vec<Stmt>> {
        let mut body = vec![];
        let body_size = fn_body.len();

        for (i, stmt) in fn_body.into_iter().enumerate() {
            match stmt.kind {
                syntax::StmtKind::If(if_stmt) => {
                    body.push(self.if_else(if_stmt)?);
                }
                syntax::StmtKind::Assign(name, expr) => {
                    let expr = self.expr(expr)?;
                    let (local, _) = self.scopes.local(&name).ok_or_else(|| {
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
                    let (expr, ty) = if let Some(expr) = expr {
                        let expr = self.expr(expr)?;
                        let ty = expr.ty.clone();

                        (Some(expr), ty)
                    } else {
                        (None, types::VOID)
                    };

                    if let Some(return_type) = &self.return_type {
                        if &ty != return_type {
                            return error!(
                                stmt.span,
                                "invalid return type '{}' (expected: {})", ty, return_type
                            );
                        }
                    }

                    self.return_type = Some(ty);

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
                            StmtKind::Return(Some(expr))
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
        let scope = self.scopes.leave();

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
