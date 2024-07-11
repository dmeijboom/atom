use std::{collections::VecDeque, fmt::Display};

use crate::{
    ast::{self, Expr, ExprKind, Literal, Stmt, StmtKind},
    codes::{BinaryOp, Code, Const, Op},
    lexer::Span,
};

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown name: {0}")]
    UnknownName(String),
}

impl ErrorKind {
    pub fn at(self, span: Span) -> Error {
        Error { kind: self, span }
    }
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl std::error::Error for Error {}

struct Var {
    name: String,
    scope: usize,
}

#[derive(Debug)]
pub struct Module {
    pub codes: Vec<Code>,
    pub consts: Vec<Const>,
}

pub struct Compiler {
    scope: usize,
    stmts: VecDeque<Stmt>,
    codes: Vec<Code>,
    consts: Vec<Const>,
    vars: Vec<Var>,
}

impl Compiler {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Compiler {
            scope: 0,
            stmts: VecDeque::from(stmts),
            codes: vec![],
            consts: vec![],
            vars: vec![],
        }
    }

    fn next(&mut self) -> Option<Stmt> {
        self.stmts.pop_front()
    }

    fn push_const(&mut self, const_: Const) -> usize {
        let idx = self.consts.len();
        self.consts.push(const_);
        idx
    }

    fn push_var(&mut self, span: Span, name: String) -> Result<usize, Error> {
        if let Ok(idx) = self.load_var(span, &name) {
            return Ok(idx);
        }

        let idx = self.vars.len();

        self.vars.push(Var {
            name,
            scope: self.scope,
        });

        // @TODO: there has to be a smarter way to do this..
        self.vars.sort_by(|a, b| b.scope.cmp(&a.scope));

        Ok(idx)
    }

    fn load_var(&mut self, span: Span, name: &str) -> Result<usize, Error> {
        self.vars
            .iter()
            .position(|v| v.name == name)
            .ok_or_else(|| ErrorKind::UnknownName(name.to_string()).at(span))
    }

    fn expr(&mut self, expr: Expr) -> Result<(), Error> {
        let code = match expr.kind {
            ExprKind::Unary(_, _) => todo!(),
            ExprKind::Binary(lhs, op, rhs) => {
                self.expr(*lhs)?;
                self.expr(*rhs)?;

                match op {
                    ast::BinaryOp::Add => Op::BinaryOp(BinaryOp::Add).at(expr.span),
                    ast::BinaryOp::Sub => Op::BinaryOp(BinaryOp::Sub).at(expr.span),
                    ast::BinaryOp::Mul => Op::BinaryOp(BinaryOp::Mul).at(expr.span),
                    ast::BinaryOp::Div => Op::BinaryOp(BinaryOp::Div).at(expr.span),
                    ast::BinaryOp::Eq => todo!(),
                    ast::BinaryOp::Ne => todo!(),
                    ast::BinaryOp::Gt => todo!(),
                    ast::BinaryOp::Gte => todo!(),
                    ast::BinaryOp::Lt => todo!(),
                    ast::BinaryOp::Lte => todo!(),
                    ast::BinaryOp::LogicalOr => todo!(),
                    ast::BinaryOp::LogicalAnd => todo!(),
                    ast::BinaryOp::BitwiseOr => Op::BinaryOp(BinaryOp::BitwiseOr).at(expr.span),
                    ast::BinaryOp::BitwiseAnd => Op::BinaryOp(BinaryOp::BitwiseAnd).at(expr.span),
                    ast::BinaryOp::BitwiseXor => Op::BinaryOp(BinaryOp::BitwiseXor).at(expr.span),
                }
            }
            ExprKind::Array(_) => todo!(),
            ExprKind::Member(_, _) => todo!(),
            ExprKind::Ident(name) => {
                let idx = self.load_var(expr.span, &name)?;
                Op::Load(idx).at(expr.span)
            }
            ExprKind::Call(_, _) => todo!(),
            ExprKind::Literal(lit) => {
                let idx = match lit {
                    Literal::Bool(_) => todo!(),
                    Literal::Int(i) => self.push_const(Const::Int(i)),
                    Literal::Float(f) => self.push_const(Const::Float(f)),
                    Literal::String(_) => todo!(),
                };

                Op::LoadConst(idx).at(expr.span)
            }
        };

        self.codes.push(code);

        Ok(())
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), Error> {
        match stmt.kind {
            StmtKind::Let(name, expr) => {
                self.expr(expr)?;
                let idx = self.push_var(stmt.span, name)?;
                self.codes.push(Op::Store(idx).at(stmt.span));
            }
            StmtKind::Assign(name, expr) => {
                let idx = self.load_var(stmt.span, &name)?;
                self.expr(expr)?;
                self.codes.push(Op::Store(idx).at(stmt.span));
            }
            StmtKind::Expr(expr) => {
                self.expr(expr)?;
                self.codes.push(Op::Discard.at(stmt.span));
            }
            StmtKind::Return(expr) => {
                self.expr(expr)?;
                self.codes.push(Op::Return.at(stmt.span));
            }
            StmtKind::Fn(_, _, _) => todo!(),
        }

        Ok(())
    }

    pub fn compile(mut self) -> Result<Module, Error> {
        while let Some(stmt) = self.next() {
            self.stmt(stmt)?;
        }

        Ok(Module {
            codes: self.codes,
            consts: self.consts,
        })
    }
}
