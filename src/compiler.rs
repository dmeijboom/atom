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

struct Var {
    name: String,
    scope: usize,
}

#[derive(Debug, Default)]
pub struct Module {
    pub codes: Rc<Vec<Code>>,
    pub consts: Vec<Const>,
    pub funcs: HashMap<String, Func>,
}

pub struct Compiler {
    scope: usize,
    stmts: VecDeque<Stmt>,
    vars: Vec<Var>,
    codes: Vec<Code>,
    consts: Vec<Const>,
    funcs: HashMap<String, Func>,
}

impl Compiler {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Compiler {
            scope: 0,
            stmts: VecDeque::from(stmts),
            vars: vec![],
            codes: vec![],
            consts: vec![],
            funcs: HashMap::new(),
        }
    }

    fn fork(&mut self, stmts: Vec<Stmt>) -> Compiler {
        let vars = mem::take(&mut self.vars);
        let consts = mem::take(&mut self.consts);
        let funcs = mem::take(&mut self.funcs);

        Compiler {
            scope: self.scope + 1,
            stmts: VecDeque::from(stmts),
            vars,
            codes: vec![],
            consts,
            funcs,
        }
    }

    fn merge_back(&mut self, other: Compiler) -> Vec<Code> {
        self.consts = other.consts;
        self.vars = other.vars;
        self.funcs = other.funcs;

        other.codes
    }

    fn loc(&self) -> usize {
        self.codes.len()
    }

    fn next(&mut self) -> Option<Stmt> {
        self.stmts.pop_front()
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

    fn call(&mut self, span: Span, callee: Expr, args: Vec<Expr>) -> Result<Code, Error> {
        let arg_count = args.len();
        self.expr(callee)?;
        self.expr_list(args)?;

        Ok(Op::Call(arg_count).at(span))
    }

    fn load_var(&mut self, span: Span, name: &str) -> Result<usize, Error> {
        self.vars
            .iter()
            .position(|v| v.name == name)
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

    fn load_name(&mut self, span: Span, name: String) -> Result<Code, Error> {
        match self.load_var(span, &name) {
            Ok(idx) => Ok(Op::Load(idx).at(span)),
            Err(e) => match self.funcs.contains_key(&name) {
                true => {
                    let idx = self.push_const(Const::Str(name));
                    Ok(Op::LoadFunc(idx).at(span))
                }
                false => Err(e),
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

    fn stmt(&mut self, stmt: Stmt) -> Result<(), Error> {
        match stmt.kind {
            StmtKind::Let(name, expr) => {
                self.expr(expr)?;
                let idx = self.push_var(stmt.span, name)?;
                self.push_code(Op::Store(idx).at(stmt.span));
            }
            StmtKind::Assign(name, expr) => {
                let idx = self.load_var(stmt.span, &name)?;
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
            StmtKind::Fn(name, _, stmts) => {
                if self.funcs.contains_key(&name) {
                    return Err(ErrorKind::FnAlreadyExists(name).at(stmt.span));
                }

                let mut compiler = self.fork(stmts);
                compiler.compile_body()?;

                let codes = self.merge_back(compiler);
                let idx = self.push_const(Const::Str(name.clone()));

                self.funcs.insert(name, Func::new(idx, codes));
            }
        }

        Ok(())
    }

    fn compile_body(&mut self) -> Result<(), Error> {
        while let Some(stmt) = self.next() {
            self.stmt(stmt)?;
        }

        Ok(())
    }

    pub fn compile(mut self) -> Result<Module, Error> {
        self.compile_body()?;

        Ok(Module {
            codes: Rc::new(self.codes),
            consts: self.consts,
            funcs: self.funcs,
        })
    }
}
