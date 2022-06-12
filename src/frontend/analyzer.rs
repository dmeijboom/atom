use crate::frontend::scope::{Local, ScopeKind, ScopeList};
use crate::frontend::syntax::{Alt, BinaryOp, InferType};
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

    fn get_type(&mut self, ty: &syntax::Type) -> Result<Type> {
        if let Some(ty) = Type::from_name(&ty.name) {
            return Ok(ty);
        }

        error!(ty.span.clone(), "unknown type {}", ty.name)
    }

    fn expr(&mut self, expr: syntax::Expr) -> Result<Expr> {
        Ok(match expr.kind {
            syntax::ExprKind::Ident(name) => {
                let (local, _, initialised) = self.scopes.local(&name).ok_or_else(|| {
                    Error::new(expr.span.clone(), format!("no such name: {}", name))
                })?;

                if !initialised {
                    return error!(
                        expr.span.clone(),
                        "name '{}' is not guaranteed to be initialised", name
                    );
                }

                // This should be safe as it's guaranteed to be initialised and thus always has a type
                Expr::new(
                    expr.span,
                    local.ty.as_ref().unwrap().clone(),
                    ExprKind::Ident(name),
                )
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
                        "invalid types {} and {} for logical expression (should be Bool)",
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
                let (valid, hint, output) = match binary.op {
                    BinaryOp::ShiftLeft | BinaryOp::ShiftRight => (
                        lhs.ty == rhs.ty && lhs.ty.is_int() && rhs.ty.is_int(),
                        "should be Int",
                        Some(types::INT),
                    ),
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => (
                        lhs.ty == rhs.ty && lhs.ty.is_numeric() && rhs.ty.is_numeric(),
                        "should be 'Numeric",
                        None,
                    ),
                    BinaryOp::Lte
                    | BinaryOp::Lt
                    | BinaryOp::Gte
                    | BinaryOp::Gt
                    | BinaryOp::Eq
                    | BinaryOp::Neq => (
                        lhs.ty == rhs.ty && lhs.ty.attr.primitive && rhs.ty.attr.primitive,
                        "should be 'Primitive",
                        Some(types::BOOL),
                    ),
                };

                if !valid {
                    return error!(
                        expr.span,
                        "invalid types {} and {} for binary expression ({})", lhs.ty, rhs.ty, hint,
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
                "invalid type {} for condition (should be Bool)", expr.ty
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
                    let scope_id = self.scopes.head().id;
                    let (local, _, initialised) =
                        self.scopes.local_mut(&name).ok_or_else(|| {
                            Error::new(expr.span.clone(), format!("no such name: {}", name))
                        })?;

                    if !local.mutable && initialised {
                        return error!(
                            stmt.span,
                            "cannot assign more than once to immutable name '{}'", name
                        );
                    }

                    if let Some(ty) = &local.ty {
                        if ty != &expr.ty {
                            return error!(
                                stmt.span,
                                "invalid type {} for name '{}' (should be {})", expr.ty, name, ty
                            );
                        }
                    } else {
                        local.ty = Some(expr.ty.clone());
                    }

                    if !initialised {
                        local.initialised_at.push(scope_id);
                    }

                    body.push(Stmt::new(stmt.span, scope_id, StmtKind::Assign(name, expr)))
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
                                "invalid return type {} (should be {})", ty, return_type
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
                    let mut ty = if let Some(ty) = let_stmt.ty {
                        Some(self.get_type(&ty)?)
                    } else {
                        None
                    };
                    let expr = if let Some(value) = let_stmt.value {
                        let expr = self.expr(value)?;

                        if let Some(ty) = &ty {
                            if &expr.ty != ty {
                                return error!(
                                    stmt.span,
                                    "invalid type {} for let statement (should be {})", expr.ty, ty
                                );
                            }
                        } else {
                            ty = Some(expr.ty.clone());
                        }

                        Some(expr)
                    } else {
                        None
                    };

                    let scope = self.scopes.head_mut();
                    scope.declare(Local {
                        ty,
                        name: let_stmt.name.clone(),
                        mutable: let_stmt.mutable,
                        initialised_at: if expr.is_some() {
                            vec![scope.id]
                        } else {
                            vec![]
                        },
                    })?;

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

    fn func(&mut self, fn_def: syntax::FnDef) -> Result<FnDef> {
        self.return_type = if let Some(ty) = fn_def.sig.return_type {
            Some(self.get_type(&ty)?)
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
                    let kind = NodeKind::FnDef(self.func(fn_def)?);
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

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use test_case::test_case;

    use super::*;

    use crate::frontend::syntax::Parser;

    macro_rules! assert_err {
        ($err:expr, $expected:expr) => {
            assert!($err.is_err(), "an error was expected");

            if let Err(err) = $err {
                assert_eq!(format!("{}", err), $expected);
            }
        };
    }

    fn analyze(source: &str) -> Result<Program> {
        let parser = Parser::new(source);
        let program = parser.parse()?;
        let analyzer = Analyzer::new();

        Ok(analyzer.analyze(program)?)
    }

    #[test]
    fn test_unknown_type() {
        let result = analyze("fn main :: () -> Test {}");
        assert_err!(result, "CompileError: unknown type Test at 1:16");
    }

    #[test]
    fn test_name_not_initialized() {
        let result = analyze(
            "fn main {
            let x;

            if true {
                x = 1;
            }

            x
        }",
        );
        assert_err!(
            result,
            "CompileError: name 'x' is not guaranteed to be initialised at 9:8"
        );
    }

    #[test]
    fn test_logical_expr_not_bool() {
        let result = analyze(
            "fn main {
            true && 1
        }",
        );
        assert_err!(result, "CompileError: invalid types Bool and Int for logical expression (should be Bool) at 2:12");
    }

    #[test_case("10 << true", "CompileError: invalid types Int and Bool for binary expression (should be Int) at 1:10"; "left shift with bool")]
    #[test_case("false * 1.04", "CompileError: invalid types Bool and Float for binary expression (should be 'Numeric) at 1:10"; "arithmetic with non-numeric type")]
    fn test_binary_expr_invalid(source: &str, expected: &str) {
        let result = analyze(&format!("fn main {{ {} }}", source));
        assert_err!(result, expected);
    }

    #[test]
    fn test_if_cond_not_bool() {
        let result = analyze(
            "fn main {
            if 1 {
                1
            }
        }",
        );
        assert_err!(
            result,
            "CompileError: invalid type Int for condition (should be Bool) at 2:12"
        );
    }

    #[test]
    fn test_assign_immutable_name() {
        let result = analyze(
            "fn main {
            let x = 1;
            x = 2;
        }",
        );
        assert_err!(
            result,
            "CompileError: cannot assign more than once to immutable name 'x' at 3:14"
        );
    }

    #[test_case("let mut x = true;\nx = 20;", "CompileError: invalid type Int for name 'x' (should be Bool) at 2:2"; "invalid type")]
    #[test_case("let x :: Int;\nx = true;", "CompileError: invalid type Bool for name 'x' (should be Int) at 2:2"; "invalid type late initialization")]
    fn test_invalid_type_assign_name(source: &str, expected: &str) {
        let result = analyze(&format!("fn main {{ {} }}", source));
        assert_err!(result, expected);
    }
}
