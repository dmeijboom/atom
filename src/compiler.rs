use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
    mem,
    rc::{Rc, Weak},
};

use crate::{
    ast::{self, Expr, ExprKind, IfStmt, Literal, Stmt, StmtKind},
    lexer::Span,
    opcode::{Const, Op, Opcode},
    runtime::{
        function::{Exec, Func},
        std::StdLib,
    },
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

#[derive(Debug)]
pub struct Module {
    pub codes: Rc<[Opcode]>,
    pub consts: Vec<Const>,
    pub funcs: Vec<Rc<Func>>,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            codes: Rc::new([]),
            consts: vec![],
            funcs: vec![],
        }
    }
}

pub struct Compiler<'a> {
    std: &'a StdLib,
    scope: usize,
    vars: Vec<Var>,
    codes: Vec<Opcode>,
    consts: Vec<Const>,
    funcs: Vec<Rc<Func>>,
    scope_funcs: HashMap<usize, Weak<Func>>,
    locals: VecDeque<HashMap<String, Local>>,
}

impl<'a> Compiler<'a> {
    pub fn new(std: &'a StdLib) -> Self {
        Compiler {
            scope: 0,
            std,
            vars: vec![],
            codes: vec![],
            funcs: vec![],
            consts: vec![],
            locals: VecDeque::default(),
            scope_funcs: HashMap::default(),
        }
    }

    fn push_scope(&mut self, locals: Vec<String>, func: Option<Weak<Func>>) -> Vec<Opcode> {
        let locals = locals
            .into_iter()
            .enumerate()
            .map(|(i, n)| (n, Local { index: i }))
            .collect();

        self.scope += 1;
        self.locals.push_front(locals);

        if let Some(func) = func {
            self.scope_funcs.insert(self.scope, func);
        }

        mem::take(&mut self.codes)
    }

    fn pop_scope(&mut self, codes: Vec<Opcode>) -> Vec<Opcode> {
        self.scope_funcs.remove(&self.scope);
        self.scope -= 1;
        self.locals.pop_front();

        mem::replace(&mut self.codes, codes)
    }

    fn pos(&self) -> usize {
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

    fn push_code(&mut self, code: Opcode) -> usize {
        let idx = self.codes.len();
        self.codes.push(code);
        idx
    }

    fn replace_code(&mut self, idx: usize, code: usize) {
        self.codes[idx] = Opcode::with_code(self.codes[idx].op(), code);
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

    fn call(&mut self, span: Span, callee: Expr, args: Vec<Expr>) -> Result<Opcode, Error> {
        let arg_count = args.len();
        self.expr_list(args)?;
        self.expr(callee)?;

        let last = self.codes.last_mut();

        if let Some(last) = last {
            let max = u32::MAX as usize;
            let code = last.code();

            if last.op() == Op::LoadFunc && code <= max && arg_count <= max {
                self.codes.pop();
                return Ok(
                    Opcode::with_code2(Op::DirectCall, code as u32, arg_count as u32).at(span),
                );
            }
        }

        Ok(Opcode::with_code(Op::Call, arg_count).at(span))
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
        let idx = self.push_code(match cond {
            true => Opcode::new(Op::PushJumpIfTrue).at(span),
            false => Opcode::new(Op::PushJumpIfFalse).at(span),
        });

        self.expr(rhs)?;
        self.replace_code(idx, self.pos());

        Ok(())
    }

    fn expr_list(&mut self, exprs: Vec<Expr>) -> Result<(), Error> {
        for expr in exprs {
            self.expr(expr)?;
        }

        Ok(())
    }

    // Load a name based in the following order: var > local > vm func > std func
    fn load_name(&mut self, span: Span, name: String) -> Result<Opcode, Error> {
        match self.load_var(span, &name, false) {
            Ok(idx) => Ok(Opcode::with_code(Op::Load, idx).at(span)),
            Err(e) => match self.load_local(&name) {
                Some(idx) => Ok(Opcode::with_code(Op::LoadArg, idx).at(span)),
                None => match self.funcs.iter().position(|f| f.name == name) {
                    Some(idx) => Ok(Opcode::with_code(Op::LoadFunc, idx).at(span)),
                    None => match self.std.funcs.iter().position(|f| f.name == name) {
                        Some(idx) => Ok(Opcode::with_code(Op::LoadNativeFunc, idx).at(span)),
                        None => Err(e),
                    },
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
                    Opcode::new(Op::UnaryNot).at(span)
                }
            },
            ExprKind::Binary(lhs, op, rhs) => {
                self.expr(*lhs)?;

                let code = match op {
                    ast::BinaryOp::Add => Opcode::new(Op::Add).at(expr.span),
                    ast::BinaryOp::Sub => Opcode::new(Op::Sub).at(expr.span),
                    ast::BinaryOp::Mul => Opcode::new(Op::Mul).at(expr.span),
                    ast::BinaryOp::Rem => Opcode::new(Op::Rem).at(expr.span),
                    ast::BinaryOp::Div => Opcode::new(Op::Div).at(expr.span),
                    ast::BinaryOp::Eq => Opcode::new(Op::Eq).at(expr.span),
                    ast::BinaryOp::Ne => Opcode::new(Op::Ne).at(expr.span),
                    ast::BinaryOp::Gt => Opcode::new(Op::Gt).at(expr.span),
                    ast::BinaryOp::Gte => Opcode::new(Op::Gte).at(expr.span),
                    ast::BinaryOp::Lt => Opcode::new(Op::Lt).at(expr.span),
                    ast::BinaryOp::Lte => Opcode::new(Op::Lte).at(expr.span),
                    ast::BinaryOp::LogicalOr => return self.logical(expr.span, *rhs, true),
                    ast::BinaryOp::LogicalAnd => return self.logical(expr.span, *rhs, false),
                    ast::BinaryOp::BitwiseOr => Opcode::new(Op::BitwiseOr).at(expr.span),
                    ast::BinaryOp::BitwiseAnd => Opcode::new(Op::BitwiseAnd).at(expr.span),
                    ast::BinaryOp::BitwiseXor => Opcode::new(Op::BitwiseXor).at(expr.span),
                };

                self.expr(*rhs)?;
                code
            }
            ExprKind::Array(items) => {
                let len = items.len();
                self.expr_list(items)?;
                Opcode::with_code(Op::MakeArray, len).at(expr.span)
            }
            ExprKind::Member(object, member) => {
                self.expr(*object)?;
                let idx = self.push_const(Const::Str(member));
                Opcode::with_code(Op::LoadMember, idx).at(expr.span)
            }
            ExprKind::CompMember(object, elem) => {
                self.expr(*object)?;
                self.expr(*elem)?;
                Opcode::new(Op::LoadElement).at(expr.span)
            }
            ExprKind::Ident(name) => self.load_name(expr.span, name)?,
            ExprKind::Call(callee, args) => self.call(expr.span, *callee, args)?,
            ExprKind::Literal(lit) => Opcode::with_code(
                Op::LoadConst,
                match lit {
                    Literal::Bool(b) => self.push_const(Const::Bool(b)),
                    Literal::Int(i) => self.push_const(Const::Int(i)),
                    Literal::Float(f) => self.push_const(Const::Float(f)),
                    Literal::String(s) => self.push_const(Const::Str(s)),
                },
            )
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

        let func = Rc::new(Func::new(name.clone(), arg_count));
        let weak = Rc::downgrade(&func);

        self.funcs.push(func);
        let previous = self.push_scope(args, Some(weak));
        self.compile_body(stmts, true)?;

        let codes = self.pop_scope(previous);
        let func = Rc::get_mut(&mut self.funcs[idx]).unwrap();

        func.arg_count = arg_count;
        func.exec = Exec::Vm(codes.into());

        Ok(())
    }

    fn for_loop(&mut self, expr: Expr, stmts: Vec<Stmt>) -> Result<(), Error> {
        let span = expr.span;
        let pos = self.pos();

        self.expr(expr)?;
        let idx = self.push_code(Opcode::new(Op::JumpIfFalse).at(span));
        self.compile_body(stmts, false)?;
        self.push_code(Opcode::with_code(Op::Jump, pos));
        self.replace_code(idx, self.pos());

        Ok(())
    }

    fn if_stmt(&mut self, IfStmt(expr, stmts, alt): IfStmt) -> Result<(), Error> {
        let span = expr.as_ref().map(|e| e.span).unwrap_or_default();
        let end_block = if let Some(expr) = expr {
            self.expr(expr)?;
            Some(self.push_code(Opcode::new(Op::JumpIfFalse)))
        } else {
            None
        };

        self.scope += 1;
        self.compile_body(stmts, false)?;
        self.scope -= 1;

        match alt {
            Some(alt) => {
                let end_stmt = self.push_code(Opcode::new(Op::Jump).at(span));

                if let Some(idx) = end_block {
                    self.replace_code(idx, self.pos());
                }

                self.if_stmt(*alt)?;
                self.replace_code(end_stmt, self.pos());
            }
            None => {
                if let Some(idx) = end_block {
                    self.replace_code(idx, self.pos());
                }
            }
        }

        Ok(())
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), Error> {
        match stmt.kind {
            StmtKind::If(if_stmt) => self.if_stmt(if_stmt)?,
            StmtKind::Let(name, expr) => {
                self.expr(expr)?;
                let idx = self.push_var(stmt.span, name)?;
                self.push_code(Opcode::with_code(Op::Store, idx).at(stmt.span));
            }
            StmtKind::Assign(op, name, mut expr) => {
                let idx = self.load_var(stmt.span, &name, false)?;

                if let Some(op) = op {
                    expr = ExprKind::Binary(
                        Box::new(ExprKind::Ident(name).at(stmt.span)),
                        op.into(),
                        Box::new(expr),
                    )
                    .at(stmt.span);
                }

                self.expr(expr)?;
                self.push_code(Opcode::with_code(Op::Store, idx).at(stmt.span));
            }
            StmtKind::Expr(expr) => {
                self.expr(expr)?;
                self.push_code(Opcode::new(Op::Discard).at(stmt.span));
            }
            StmtKind::Return(expr) => {
                self.expr(expr)?;
                self.push_code(Opcode::new(Op::Return).at(stmt.span));
            }
            StmtKind::Fn(name, args, stmts) => self.func(stmt.span, name, args, stmts)?,
            StmtKind::For(expr, stmts) => self.for_loop(expr, stmts)?,
        }

        Ok(())
    }

    fn optimize_tail_call(&mut self) {
        let n = self.codes.len() - 2;

        if let Some(opcode) = self.codes.last_mut() {
            match opcode.op() {
                Op::Call => {
                    *opcode = Opcode::with_code(Op::TailCall, opcode.code()).at(opcode.span);
                }
                Op::Return => {
                    if let Some(opcode) = self.codes.get_mut(n) {
                        if let Op::Call = opcode.op() {
                            *opcode =
                                Opcode::with_code(Op::TailCall, opcode.code()).at(opcode.span);
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
            codes: self.codes.into(),
            consts: self.consts,
            funcs: self.funcs,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::std::stdlib;

    use super::*;

    #[test]
    fn test_scope() {
        let lib = stdlib();
        let mut compiler = Compiler::new(&lib);

        compiler.push_var(Span::default(), "n".to_string()).unwrap();
        compiler.push_scope(vec![], None);

        let expected = compiler.push_var(Span::default(), "n".to_string()).unwrap();
        let actual = compiler.load_var(Span::default(), "n", false).unwrap();

        assert_eq!(expected, actual);
    }
}
