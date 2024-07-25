use std::{
    collections::{HashMap, VecDeque},
    mem,
    rc::{Rc, Weak},
};

use wyhash2::WyHash;

use crate::{
    ast::{self, Expr, ExprKind, IfStmt, Literal, Stmt, StmtKind},
    error::{IntoSpanned, SpannedError},
    lexer::Span,
    opcode::{Const, Op, Opcode},
    runtime::{
        class::Class,
        function::{Exec, Func, Receiver},
        std::StdLib,
    },
};

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown name: {0}")]
    UnknownName(String),
    #[error("fn '{0}' already exists")]
    DuplicateFn(String),
    #[error("method '{0}' of '{}' already exists", _1.name)]
    DuplicateMethod(String, Rc<Class>),
    #[error("class '{0}' already exists")]
    DuplicateClass(String),
}

pub type CompileError = SpannedError<ErrorKind>;

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
    pub classes: Vec<Rc<Class>>,
}

impl Default for Module {
    fn default() -> Self {
        Self {
            codes: Rc::new([]),
            consts: vec![],
            funcs: vec![],
            classes: vec![],
        }
    }
}

fn new_rc<T>(item: T) -> (Rc<T>, Weak<T>) {
    let strong = Rc::new(item);
    let weak = Rc::downgrade(&strong);

    (strong, weak)
}

pub enum Scope {
    Global,
    Local,
    Func(usize),
}

pub struct Compiler<'a> {
    std: &'a StdLib,
    scope: Vec<Scope>,
    vars: Vec<Var>,
    codes: Vec<Opcode>,
    consts: Vec<Const>,
    funcs: Vec<Rc<Func>>,
    classes: Vec<Rc<Class>>,
    locals: VecDeque<HashMap<String, Local, WyHash>>,
}

impl<'a> Compiler<'a> {
    pub fn new(std: &'a StdLib) -> Self {
        Compiler {
            std,
            scope: vec![Scope::Global],
            vars: vec![],
            codes: vec![],
            funcs: vec![],
            consts: vec![],
            classes: vec![],
            locals: VecDeque::default(),
        }
    }

    fn scope_id(&self) -> usize {
        self.scope.len() - 1
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

    fn push_var(&mut self, span: Span, name: String) -> Result<usize, CompileError> {
        if let Ok(idx) = self.load_var(span, &name, true) {
            return Ok(idx);
        }

        let idx = self.vars.len();

        self.vars.push(Var {
            index: idx,
            name,
            scope: self.scope_id(),
        });

        self.vars.sort_by(|a, b| b.scope.cmp(&a.scope));

        Ok(idx)
    }

    fn call(&mut self, span: Span, callee: Expr, args: Vec<Expr>) -> Result<Opcode, CompileError> {
        let arg_count = args.len();
        self.expr_list(args)?;
        self.expr(callee)?;
        Ok(Opcode::with_code(Op::Call, arg_count).at(span))
    }

    fn load_local(&mut self, name: &str) -> Option<usize> {
        if let Some(locals) = self.locals.front() {
            return locals.get(name).map(|l| l.index);
        }

        None
    }

    fn load_var(
        &mut self,
        span: Span,
        name: &str,
        strict_scope: bool,
    ) -> Result<usize, CompileError> {
        self.vars
            .iter()
            .filter(|v| {
                v.name == name
                    && if strict_scope {
                        v.scope == self.scope_id()
                    } else {
                        v.scope <= self.scope_id()
                    }
            })
            .map(|v| v.index)
            .next()
            .ok_or_else(|| ErrorKind::UnknownName(name.to_string()).at(span))
    }

    fn logical(&mut self, span: Span, rhs: Expr, cond: bool) -> Result<(), CompileError> {
        let idx = self.push_code(match cond {
            true => Opcode::new(Op::PushJumpIfTrue).at(span),
            false => Opcode::new(Op::PushJumpIfFalse).at(span),
        });

        self.expr(rhs)?;
        self.replace_code(idx, self.pos());

        Ok(())
    }

    fn expr_list(&mut self, exprs: Vec<Expr>) -> Result<(), CompileError> {
        for expr in exprs {
            self.expr(expr)?;
        }

        Ok(())
    }

    // Load a name based in the following order: var > local > vm class > vm func > std func
    fn load_name(&mut self, span: Span, name: String) -> Result<Opcode, CompileError> {
        match self.load_var(span, &name, false) {
            Ok(idx) => Ok(Opcode::with_code(Op::Load, idx).at(span)),
            Err(e) => match self.load_local(&name) {
                Some(idx) => Ok(Opcode::with_code(Op::LoadArg, idx).at(span)),
                None => match self.classes.iter().position(|c| c.name == name) {
                    Some(idx) => Ok(Opcode::with_code(Op::LoadClass, idx).at(span)),
                    None => match self.funcs.iter().position(|f| f.name == name) {
                        Some(idx) => Ok(Opcode::with_code(Op::LoadFunc, idx).at(span)),
                        None => match self.std.funcs.iter().position(|f| f.name == name) {
                            Some(idx) => Ok(Opcode::with_code(Op::LoadNativeFunc, idx).at(span)),
                            None => Err(e),
                        },
                    },
                },
            },
        }
    }

    fn assign(
        &mut self,
        span: Span,
        op: Option<ast::AssignOp>,
        lhs: Expr,
        mut rhs: Expr,
    ) -> Result<Opcode, CompileError> {
        if let Some(op) = op {
            rhs = ExprKind::Binary(Box::new(lhs.clone()), op.into(), Box::new(rhs)).at(span);
        }

        Ok(match lhs.kind {
            ExprKind::Ident(name) => {
                let idx = self.load_var(rhs.span, &name, false)?;
                self.expr(rhs)?;
                Opcode::with_code(Op::Store, idx).at(span)
            }
            ExprKind::Member(object, member) => {
                self.expr(*object)?;
                self.expr(rhs)?;
                let idx = self.push_const(Const::Str(member));
                Opcode::with_code(Op::StoreMember, idx).at(span)
            }
            ExprKind::CompMember(object, index) => {
                self.expr(*object)?;
                self.expr(*index)?;
                self.expr(rhs)?;
                Opcode::new(Op::StoreElement).at(span)
            }
            _ => unimplemented!(),
        })
    }

    fn expr(&mut self, expr: Expr) -> Result<(), CompileError> {
        let code = match expr.kind {
            ExprKind::Unary(op, unary_expr) => match op {
                ast::UnaryOp::Not => {
                    let span = expr.span;
                    self.expr(*unary_expr)?;
                    Opcode::new(Op::UnaryNot).at(span)
                }
            },
            ExprKind::Assign(op, lhs, rhs) => self.assign(expr.span, op, *lhs, *rhs)?,
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

    fn compile_fn_body(
        &mut self,
        scope: Scope,
        args: Vec<String>,
        stmts: Vec<Stmt>,
    ) -> Result<Vec<Opcode>, CompileError> {
        let locals = args
            .into_iter()
            .enumerate()
            .map(|(i, n)| (n, Local { index: i }))
            .collect();

        self.scope.push(scope);
        self.locals.push_front(locals);

        let previous = mem::take(&mut self.codes);

        self.compile_body(stmts)?;

        let scope = self.scope.pop();
        self.locals.pop_front();
        let codes = mem::replace(&mut self.codes, previous);

        Ok(self.optim(scope.unwrap(), codes))
    }

    fn method(
        &mut self,
        span: Span,
        class: Weak<Class>,
        methods: &mut HashMap<String, Rc<Func>, WyHash>,
        name: String,
        args: Vec<String>,
        stmts: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        if methods.contains_key(&name) {
            return Err(ErrorKind::DuplicateMethod(name, class.upgrade().unwrap()).at(span));
        }

        let arg_count = args.iter().filter(|a| a.as_str() != "self").count();
        let func = Rc::new(Func::new(name.clone(), arg_count).with_receiver(Receiver::Class));

        methods.insert(name.clone(), func);

        let codes = self.compile_fn_body(Scope::Local, args, stmts)?;
        let func = methods.get_mut(&name).and_then(|m| Rc::get_mut(m)).unwrap();

        func.exec = Exec::Vm(codes.into());

        Ok(())
    }

    fn fn_stmt(
        &mut self,
        span: Span,
        name: String,
        args: Vec<String>,
        stmts: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        if self.funcs.iter().any(|f| f.name == name) {
            return Err(ErrorKind::DuplicateFn(name).at(span));
        }

        let idx = self.funcs.len();
        let func = Rc::new(Func::new(name.clone(), args.len()));

        self.funcs.push(func);

        let codes = self.compile_fn_body(Scope::Func(idx), args, stmts)?;
        let func = Rc::get_mut(&mut self.funcs[idx]).unwrap();

        func.exec = Exec::Vm(codes.into());

        Ok(())
    }

    fn for_stmt(&mut self, span: Span, expr: Expr, body: Vec<Stmt>) -> Result<(), CompileError> {
        let pos = self.pos();
        self.expr(expr)?;
        let idx = self.push_code(Opcode::new(Op::JumpIfFalse).at(span));
        self.compile_body(body)?;
        self.push_code(Opcode::with_code(Op::Jump, pos));
        self.replace_code(idx, self.pos());

        Ok(())
    }

    fn class_stmt(
        &mut self,
        span: Span,
        name: String,
        methods: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        if self.classes.iter().any(|c| c.name == name) {
            return Err(ErrorKind::DuplicateClass(name).at(span));
        }

        let idx = self.classes.len();
        let (class, weak) = new_rc(Class::new(name.clone()));
        self.classes.push(class);

        let mut funcs = HashMap::with_hasher(WyHash::default());

        for method in methods {
            let span = method.span;
            let (name, args, stmts) = match method.kind {
                StmtKind::Fn(name, args, stmts) => (name, args, stmts),
                _ => unreachable!(),
            };

            self.method(span, Weak::clone(&weak), &mut funcs, name, args, stmts)?;
        }

        drop(weak);

        let class = Rc::get_mut(&mut self.classes[idx]).unwrap();
        class.methods = funcs;

        Ok(())
    }

    fn for_cond_stmt(
        &mut self,
        span: Span,
        pre: Stmt,
        expr: Expr,
        post: Expr,
        body: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        self.stmt(pre)?;
        let pos = self.pos();
        self.expr(expr)?;
        let idx = self.push_code(Opcode::new(Op::JumpIfFalse).at(span));
        self.compile_body(body)?;
        self.expr(post)?;

        self.push_code(Opcode::with_code(Op::Jump, pos));
        self.replace_code(idx, self.pos());

        Ok(())
    }

    fn if_stmt(&mut self, IfStmt(expr, stmts, alt): IfStmt) -> Result<(), CompileError> {
        let span = expr.as_ref().map(|e| e.span).unwrap_or_default();
        let end_block = if let Some(expr) = expr {
            self.expr(expr)?;
            Some(self.push_code(Opcode::new(Op::JumpIfFalse)))
        } else {
            None
        };

        self.scope.push(Scope::Local);
        self.compile_body(stmts)?;
        self.scope.pop();

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

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CompileError> {
        match stmt.kind {
            StmtKind::If(if_stmt) => self.if_stmt(if_stmt)?,
            StmtKind::Let(name, expr) => {
                let idx = self.push_var(stmt.span, name)?;

                if let Some(expr) = expr {
                    self.expr(expr)?;
                    self.push_code(Opcode::with_code(Op::Store, idx).at(stmt.span));
                }
            }
            StmtKind::Expr(expr) => {
                let assignment = expr.is_assign();
                self.expr(expr)?;

                if !assignment {
                    self.push_code(Opcode::new(Op::Discard).at(stmt.span));
                }
            }
            StmtKind::Return(expr) => {
                self.expr(expr)?;
                self.push_code(Opcode::new(Op::Return).at(stmt.span));
            }
            StmtKind::Fn(name, args, stmts) => self.fn_stmt(stmt.span, name, args, stmts)?,
            StmtKind::For(expr, body) => self.for_stmt(stmt.span, expr, body)?,
            StmtKind::ForCond(pre, expr, post, body) => {
                self.for_cond_stmt(stmt.span, *pre, expr, post, body)?
            }
            StmtKind::Class(name, methods) => self.class_stmt(stmt.span, name, methods)?,
        }

        Ok(())
    }

    fn compile_body(&mut self, stmts: Vec<Stmt>) -> Result<(), CompileError> {
        for stmt in stmts {
            self.stmt(stmt)?;
        }

        Ok(())
    }

    fn optim_tail_call(&mut self, func: usize, codes: &mut [Opcode]) {
        let mut iter = codes.iter_mut().rev();

        while let Some(op) = iter.next() {
            match op.op() {
                Op::Return => continue,
                Op::Call => {
                    let arg_count = op.code();
                    let idx = match iter.next() {
                        Some(op) if op.op() == Op::LoadFunc => op.code(),
                        _ => break,
                    };

                    if idx != func {
                        break;
                    }

                    *op = Opcode::with_code(Op::TailCall, arg_count);
                    break;
                }
                _ => {}
            }
        }
    }

    fn optim(&mut self, scope: Scope, mut codes: Vec<Opcode>) -> Vec<Opcode> {
        if let Scope::Func(func) = scope {
            if codes
                .iter()
                .filter(|c| c.op() == Op::LoadFunc && c.code() == func)
                .count()
                != 1
            {
                return codes;
            }

            self.optim_tail_call(func, &mut codes);
        }

        codes
    }

    pub fn compile(mut self, stmts: Vec<Stmt>) -> Result<Module, CompileError> {
        self.compile_body(stmts)?;

        Ok(Module {
            codes: self.codes.into(),
            consts: self.consts,
            funcs: self.funcs,
            classes: self.classes,
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
        compiler.scope.push(Scope::Local);

        let expected = compiler.push_var(Span::default(), "n".to_string()).unwrap();
        let actual = compiler.load_var(Span::default(), "n", false).unwrap();

        assert_eq!(expected, actual);
    }
}
