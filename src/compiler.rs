use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
    mem,
    rc::Rc,
};

use crate::{
    ast::{self, Expr, ExprKind, Literal, Stmt, StmtKind},
    codes::{BinaryOp, Code, CompareOp, Const, Func, Op},
    lexer::Span,
};

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown name: {0}")]
    UnknownName(String),
    #[error("fn '{0}' already exists")]
    FnAlreadyExists(String),
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

#[derive(Debug)]
struct Var {
    index: usize,
    name: String,
    scope: usize,
}

#[derive(Debug)]
struct Local {
    index: usize,
}

#[derive(Debug, Default)]
pub struct Module {
    pub codes: Rc<Vec<Code>>,
    pub consts: Vec<Const>,
    pub funcs: Vec<Rc<Func>>,
}

pub struct Compiler {
    scope: usize,
    vars: Vec<Var>,
    codes: Vec<Code>,
    consts: Vec<Const>,
    funcs: Vec<Rc<Func>>,
    locals: VecDeque<HashMap<String, Local>>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            scope: 0,
            vars: vec![],
            codes: vec![],
            funcs: vec![],
            consts: vec![],
            locals: VecDeque::default(),
        }
    }

    fn push_scope(&mut self, locals: Vec<String>) -> Vec<Code> {
        let locals = locals
            .into_iter()
            .enumerate()
            .map(|(i, n)| (n, Local { index: i }))
            .collect();

        self.scope += 1;
        self.locals.push_front(locals);

        mem::take(&mut self.codes)
    }

    fn pop_scope(&mut self, codes: Vec<Code>) -> Vec<Code> {
        self.scope -= 1;
        self.locals.pop_front();

        mem::replace(&mut self.codes, codes)
    }

    fn loc(&self) -> usize {
        self.codes.len()
    }

    fn push_const(&mut self, const_: Const) -> usize {
        if let Some(idx) = self.consts.iter().position(|c| c == &const_) {
            return idx;
        }

        let idx = self.consts.len();
        self.consts.push(const_);
        idx
    }

    fn push_code(&mut self, code: Code) -> usize {
        let idx = self.codes.len();
        self.codes.push(code);
        idx
    }

    fn push_var(&mut self, span: Span, name: String) -> Result<usize, Error> {
        if let Ok(idx) = self.load_var(span, &name, true) {
            return Ok(idx);
        }

        let idx = self.vars.len();

        self.vars.push(Var {
            index: idx,
            name,
            scope: self.scope,
        });

        self.vars.sort_by(|a, b| b.scope.cmp(&a.scope));

        Ok(idx)
    }

    fn call(&mut self, span: Span, callee: Expr, args: Vec<Expr>) -> Result<Code, Error> {
        let arg_count = args.len();
        self.expr_list(args)?;
        self.expr(callee)?;

        Ok(Op::Call(arg_count).at(span))
    }

    fn load_local(&mut self, name: &str) -> Option<usize> {
        if let Some(locals) = self.locals.front() {
            return locals.get(name).map(|l| l.index);
        }

        None
    }

    fn load_var(&mut self, span: Span, name: &str, strict_scope: bool) -> Result<usize, Error> {
        self.vars
            .iter()
            .filter(|v| {
                v.name == name
                    && if strict_scope {
                        v.scope == self.scope
                    } else {
                        v.scope <= self.scope
                    }
            })
            .map(|v| v.index)
            .next()
            .ok_or_else(|| ErrorKind::UnknownName(name.to_string()).at(span))
    }

    fn logical(&mut self, span: Span, rhs: Expr, cond: bool) -> Result<(), Error> {
        let idx = self.push_code(
            match cond {
                true => Op::PushJumpIfTrue(0),
                false => Op::PushJumpIfFalse(0),
            }
            .at(Span::default()),
        );

        self.expr(rhs)?;
        self.codes[idx] = match cond {
            true => Op::PushJumpIfTrue(self.loc()),
            false => Op::PushJumpIfFalse(self.loc()),
        }
        .at(span);

        Ok(())
    }

    fn expr_list(&mut self, exprs: Vec<Expr>) -> Result<(), Error> {
        for expr in exprs {
            self.expr(expr)?;
        }

        Ok(())
    }

    // Load a name based in the following order: var > local > func
    fn load_name(&mut self, span: Span, name: String) -> Result<Code, Error> {
        match self.load_var(span, &name, false) {
            Ok(idx) => Ok(Op::Load(idx).at(span)),
            Err(e) => match self.load_local(&name) {
                Some(idx) => Ok(Op::LoadArg(idx).at(span)),
                None => match self.funcs.iter().position(|f| f.name == name) {
                    Some(idx) => Ok(Op::LoadFunc(idx).at(span)),
                    None => Err(e),
                },
            },
        }
    }

    fn expr(&mut self, expr: Expr) -> Result<(), Error> {
        let code = match expr.kind {
            ExprKind::Unary(op, unary_expr) => match op {
                ast::UnaryOp::Not => {
                    let span = expr.span;
                    self.expr(*unary_expr)?;
                    Op::UnaryNot.at(span)
                }
            },
            ExprKind::Binary(lhs, op, rhs) => {
                self.expr(*lhs)?;

                let code = match op {
                    ast::BinaryOp::Add => Op::BinaryOp(BinaryOp::Add).at(expr.span),
                    ast::BinaryOp::Sub => Op::BinaryOp(BinaryOp::Sub).at(expr.span),
                    ast::BinaryOp::Mul => Op::BinaryOp(BinaryOp::Mul).at(expr.span),
                    ast::BinaryOp::Div => Op::BinaryOp(BinaryOp::Div).at(expr.span),
                    ast::BinaryOp::Eq => Op::CompareOp(CompareOp::Eq).at(expr.span),
                    ast::BinaryOp::Ne => Op::CompareOp(CompareOp::Ne).at(expr.span),
                    ast::BinaryOp::Gt => Op::CompareOp(CompareOp::Gt).at(expr.span),
                    ast::BinaryOp::Gte => Op::CompareOp(CompareOp::Gte).at(expr.span),
                    ast::BinaryOp::Lt => Op::CompareOp(CompareOp::Lt).at(expr.span),
                    ast::BinaryOp::Lte => Op::CompareOp(CompareOp::Lte).at(expr.span),
                    ast::BinaryOp::LogicalOr => return self.logical(expr.span, *rhs, true),
                    ast::BinaryOp::LogicalAnd => return self.logical(expr.span, *rhs, false),
                    ast::BinaryOp::BitwiseOr => Op::BinaryOp(BinaryOp::BitwiseOr).at(expr.span),
                    ast::BinaryOp::BitwiseAnd => Op::BinaryOp(BinaryOp::BitwiseAnd).at(expr.span),
                    ast::BinaryOp::BitwiseXor => Op::BinaryOp(BinaryOp::BitwiseXor).at(expr.span),
                };

                self.expr(*rhs)?;
                code
            }
            ExprKind::Array(items) => {
                let len = items.len();
                self.expr_list(items)?;
                Op::MakeArray(len).at(expr.span)
            }
            ExprKind::Member(object, member) => {
                self.expr(*object)?;
                let idx = self.push_const(Const::Str(member));
                Op::LoadMember(idx).at(expr.span)
            }
            ExprKind::CompMember(object, elem) => {
                self.expr(*object)?;
                self.expr(*elem)?;
                Op::LoadElement.at(expr.span)
            }
            ExprKind::Ident(name) => self.load_name(expr.span, name)?,
            ExprKind::Call(callee, args) => self.call(expr.span, *callee, args)?,
            ExprKind::Literal(lit) => Op::LoadConst(match lit {
                Literal::Bool(b) => self.push_const(Const::Bool(b)),
                Literal::Int(i) => self.push_const(Const::Int(i)),
                Literal::Float(f) => self.push_const(Const::Float(f)),
                Literal::String(s) => self.push_const(Const::Str(s)),
            })
            .at(expr.span),
        };

        self.push_code(code);

        Ok(())
    }

    fn func(
        &mut self,
        span: Span,
        name: String,
        args: Vec<String>,
        stmts: Vec<Stmt>,
    ) -> Result<(), Error> {
        if self.funcs.iter().any(|f| f.name == name) {
            return Err(ErrorKind::FnAlreadyExists(name).at(span));
        }

        let idx = self.funcs.len();
        let arg_count = args.len();

        self.funcs.push(Rc::new(Func::new(name.clone(), arg_count)));
        let previous = self.push_scope(args);
        self.compile_body(stmts, true)?;

        let codes = self.pop_scope(previous);
        self.funcs[idx] = Rc::new(Func::with_codes(name, arg_count, codes));

        Ok(())
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), Error> {
        match stmt.kind {
            StmtKind::Let(name, expr) => {
                self.expr(expr)?;
                let idx = self.push_var(stmt.span, name)?;
                self.push_code(Op::Store(idx).at(stmt.span));
            }
            StmtKind::Assign(name, expr) => {
                let idx = self.load_var(stmt.span, &name, false)?;
                self.expr(expr)?;
                self.push_code(Op::Store(idx).at(stmt.span));
            }
            StmtKind::Expr(expr) => {
                self.expr(expr)?;
                self.push_code(Op::Discard.at(stmt.span));
            }
            StmtKind::Return(expr) => {
                self.expr(expr)?;
                self.push_code(Op::Return.at(stmt.span));
            }
            StmtKind::Fn(name, args, stmts) => self.func(stmt.span, name, args, stmts)?,
            StmtKind::If(expr, stmts) => {
                self.expr(expr)?;
                let idx = self.push_code(Op::JumpIfFalse(0).at(stmt.span));
                self.scope += 1;
                self.compile_body(stmts, false)?;
                self.scope -= 1;
                self.codes[idx] = Op::JumpIfFalse(self.loc()).at(stmt.span);
            }
        }

        Ok(())
    }

    fn optimize_tail_call(&mut self) {
        let n = self.codes.len() - 2;

        if let Some(code) = self.codes.last_mut() {
            match code.op {
                Op::Call(idx) => code.op = Op::TailCall(idx),
                Op::Return => {
                    if let Some(code) = self.codes.get_mut(n) {
                        if let Op::Call(idx) = code.op {
                            code.op = Op::TailCall(idx);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn compile_body(&mut self, stmts: Vec<Stmt>, func: bool) -> Result<(), Error> {
        for stmt in stmts {
            self.stmt(stmt)?;
        }

        if func {
            self.optimize_tail_call();
        }

        Ok(())
    }

    pub fn compile(mut self, stmts: Vec<Stmt>) -> Result<Module, Error> {
        self.compile_body(stmts, false)?;

        Ok(Module {
            codes: Rc::new(self.codes),
            consts: self.consts,
            funcs: self.funcs,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope() {
        let mut compiler = Compiler::new();

        compiler.push_var(Span::default(), "n".to_string()).unwrap();
        compiler.push_scope(vec![]);

        let expected = compiler.push_var(Span::default(), "n".to_string()).unwrap();
        let actual = compiler.load_var(Span::default(), "n", false).unwrap();

        assert_eq!(expected, actual);
    }
}
