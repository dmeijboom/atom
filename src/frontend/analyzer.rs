use crate::frontend::scope::{ScopeKind, ScopeList};
use crate::frontend::syntax::{InferType, Span};
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
}

impl Analyzer {
    pub fn new() -> Self {
        Self {
            nodes: vec![],
            scopes: ScopeList::new(),
        }
    }

    fn get_type(&mut self, span: &Span, ty: &syntax::Type) -> Result<Type> {
        if let Some(ty) = Type::from_name(&ty.name) {
            return Ok(ty);
        }

        error!(span.clone(), "unknown type: {}", ty.name)
    }

    fn analyze_expr(&mut self, expr: syntax::Expr) -> Result<Expr> {
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
                let lhs = self.analyze_expr(binary.left)?;
                let rhs = self.analyze_expr(binary.right)?;

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

    fn analyze_fn(&mut self, span: &Span, fn_def: syntax::FnDef) -> Result<FnDef> {
        let return_type = if let Some(ty) = fn_def.sig.return_type {
            Some(self.get_type(span, &ty)?)
        } else {
            None
        };

        self.scopes.enter(ScopeKind::Fn);

        let mut body = vec![];
        let body_size = fn_def.body.len();
        let mut inferred_return_type = None;

        for (i, stmt) in fn_def.body.into_iter().enumerate() {
            match stmt.kind {
                syntax::StmtKind::Return(expr) => {
                    let expr = self.analyze_expr(expr)?;

                    inferred_return_type = Some(expr.ty.clone());

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        StmtKind::Return(expr),
                    ));

                    break;
                }
                syntax::StmtKind::Let(name, expr) => {
                    let expr = self.analyze_expr(expr)?;

                    let scope = self.scopes.head_mut();
                    scope.declare(name.clone(), expr.ty.clone())?;

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        StmtKind::Let(name, expr),
                    ));
                }
                syntax::StmtKind::Expr(expr) => {
                    let expr = self.analyze_expr(expr)?;

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        if i == body_size - 1 {
                            inferred_return_type = Some(expr.ty.clone());
                            StmtKind::Return(expr)
                        } else {
                            StmtKind::Expr(expr)
                        },
                    ));
                }
                syntax::StmtKind::ExprEnd(expr) => {
                    let expr = self.analyze_expr(expr)?;

                    body.push(Stmt::new(
                        stmt.span,
                        self.scopes.head().id,
                        StmtKind::Expr(expr),
                    ));
                }
            }
        }

        let scope = self.scopes.exit();

        let inferred_return_type = inferred_return_type.unwrap_or(types::VOID);
        let return_type = match return_type {
            Some(return_type) if return_type != inferred_return_type => {
                return error!(
                    span.clone(),
                    "invalid return type '{}' for '{}(...)' (expected: {})",
                    inferred_return_type,
                    fn_def.name,
                    return_type
                );
            }
            Some(return_type) => return_type,
            None => inferred_return_type,
        };

        Ok(FnDef {
            name: fn_def.name,
            body,
            return_type,
            scope,
        })
    }

    pub fn analyze(mut self, program: Vec<syntax::Node>) -> Result<Program> {
        for node in program {
            match node.kind {
                syntax::NodeKind::FnDef(fn_def) => {
                    let kind = NodeKind::FnDef(self.analyze_fn(&node.span, fn_def)?);
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
