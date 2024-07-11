use std::collections::VecDeque;

use crate::{
    ast::{self, Expr, ExprKind, Literal, Stmt, StmtKind},
    codes::{BinaryOp, Code, Const, Op},
};

#[derive(Debug)]
pub struct Module {
    pub codes: Vec<Code>,
    pub consts: Vec<Const>,
}

pub struct Compiler {
    stmts: VecDeque<Stmt>,
    codes: Vec<Code>,
    consts: Vec<Const>,
}

impl Compiler {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Compiler {
            stmts: VecDeque::from(stmts),
            codes: Vec::new(),
            consts: Vec::new(),
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

    fn expr(&mut self, expr: Expr) {
        let code = match expr.kind {
            ExprKind::Unary(_, _) => todo!(),
            ExprKind::Binary(lhs, op, rhs) => {
                self.expr(*lhs);
                self.expr(*rhs);

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
                    ast::BinaryOp::Assign => todo!(),
                    ast::BinaryOp::LogicalOr => todo!(),
                    ast::BinaryOp::LogicalAnd => todo!(),
                    ast::BinaryOp::BitwiseOr => Op::BinaryOp(BinaryOp::BitwiseOr).at(expr.span),
                    ast::BinaryOp::BitwiseAnd => Op::BinaryOp(BinaryOp::BitwiseAnd).at(expr.span),
                    ast::BinaryOp::BitwiseXor => Op::BinaryOp(BinaryOp::BitwiseXor).at(expr.span),
                }
            }
            ExprKind::Array(_) => todo!(),
            ExprKind::Member(_, _) => todo!(),
            ExprKind::Ident(_) => todo!(),
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
    }

    fn stmt(&mut self, stmt: Stmt) {
        match stmt.kind {
            StmtKind::Let(_, _) => todo!(),
            StmtKind::Expr(expr) => self.expr(expr),
            StmtKind::Fn(_, _, _) => todo!(),
        }
    }

    pub fn compile(mut self) -> Module {
        while let Some(stmt) = self.next() {
            self.stmt(stmt);
        }

        Module {
            codes: self.codes,
            consts: self.consts,
        }
    }
}
