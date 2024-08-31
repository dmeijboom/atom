use std::{
    collections::{HashMap, VecDeque},
    io, mem,
    rc::Rc,
};

use bytes::{Buf, BufMut, BytesMut};
use wyhash2::WyHash;

use crate::{
    ast::{self, Expr, ExprKind, FnArg, IfStmt, Literal, Stmt, StmtKind},
    error::{IntoSpanned, SpannedError},
    lexer::Span,
    opcode::{Const, Op, Opcode},
    runtime::{class::Class, function::Fn, module::Module, Name},
};

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown name '{0}'")]
    UnknownName(String),
    #[error("fn '{0}' already exists")]
    DuplicateFn(String),
    #[error("method '{0}' already exists")]
    DuplicateMethod(String),
    #[error("class '{0}' already exists")]
    DuplicateClass(String),
    #[error("name '{0}' is not initialized")]
    NameUninitialized(String),
    #[error("name '{0}' is not used")]
    NameUnused(String),
    #[error("failed to write opcode(s): {0}")]
    FailedWriteOpcode(#[from] io::Error),
}

pub type CompileError = SpannedError<ErrorKind>;

#[derive(Debug, PartialEq)]
struct Var {
    id: usize,
    init: bool,
    used: bool,
    span: Span,
}

impl Var {
    fn new(span: Span, id: usize) -> Self {
        Self {
            span,
            id,
            init: false,
            used: false,
        }
    }

    fn with_init(span: Span, id: usize, init: bool) -> Self {
        let mut var = Self::new(span, id);
        var.init = init;
        var
    }
}

#[derive(Default)]
struct Scope {
    method: bool,
    fn_id: Option<usize>,
    vars: HashMap<String, Var, WyHash>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            fn_id: None,
            ..Self::default()
        }
    }

    pub fn with_fn(fn_id: usize) -> Self {
        Self {
            fn_id: Some(fn_id),
            ..Self::default()
        }
    }

    pub fn with_method() -> Self {
        Self {
            fn_id: None,
            method: true,
            ..Self::default()
        }
    }
}

enum Marker {
    Begin(usize),
    End(usize),
}

fn check_used(vars: &HashMap<String, Var, WyHash>) -> Result<(), CompileError> {
    if let Some((name, var)) = vars.iter().find(|(_, var)| !var.used) {
        return Err(ErrorKind::NameUnused(name.to_string()).at(var.span));
    }

    Ok(())
}

fn check_usage(name: &str, var: &mut Var) -> Result<(), CompileError> {
    if !var.init {
        return Err(ErrorKind::NameUninitialized(name.to_string()).at(var.span));
    }

    var.used = true;
    Ok(())
}

pub struct Compiler {
    optimize: bool,
    vars_seq: usize,
    scope: VecDeque<Scope>,
    body: BytesMut,
    consts: Vec<Const>,
    funcs: Vec<Fn>,
    classes: Vec<Class>,
    markers: Vec<Marker>,
    locals: VecDeque<HashMap<String, Var, WyHash>>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            vars_seq: 0,
            body: BytesMut::new(),
            funcs: vec![],
            consts: vec![],
            classes: vec![],
            markers: vec![],
            optimize: true,
            locals: VecDeque::default(),
            scope: VecDeque::from(vec![Scope::new()]),
        }
    }
}

impl Compiler {
    pub fn with_optimize(mut self, optimize: bool) -> Self {
        self.optimize = optimize;
        self
    }

    fn offset(&self) -> usize {
        self.body.len()
    }

    fn pop_scope(&mut self) -> Result<Option<Scope>, CompileError> {
        if let Some(scope) = self.scope.pop_front() {
            check_used(&scope.vars)?;
            return Ok(Some(scope));
        }

        Ok(None)
    }

    fn push_const(&mut self, const_: Const) -> usize {
        if let Some(idx) = self.consts.iter().position(|c| c == &const_) {
            return idx;
        }

        let idx = self.consts.len();
        self.consts.push(const_);
        idx
    }

    fn push_opcode(&mut self, code: Opcode) -> usize {
        let offset = self.body.len();
        code.serialize(&mut self.body);
        offset
    }

    fn set_pos(&mut self, offset: usize, new_offset: usize) {
        let mut buff = &self.body[offset..offset + 8];
        let bits = buff.get_u64();

        let mut buff = &mut self.body[offset..];
        buff.put_u64(bits | (new_offset / 16) as u64);
    }

    fn remote_last(&mut self) {
        self.body.truncate(self.body.len() - 16);
    }

    fn iter(&self) -> impl Iterator<Item = Opcode> + '_ {
        self.body.chunks_exact(16).map(Opcode::deserialize)
    }

    fn push_var(&mut self, span: Span, name: String, init: bool) -> Result<usize, CompileError> {
        let id = self.vars_seq;
        self.vars_seq += 1;

        let len = self.scope.len();
        let scope = &mut self.scope[len - 1];
        scope.vars.insert(name, Var::with_init(span, id, init));

        Ok(id)
    }

    fn call_extern(
        &mut self,
        span: Span,
        name: String,
        arg_count: usize,
    ) -> Result<Fn, CompileError> {
        let idx = self.push_const(Const::Str(name.clone()));
        let code = Opcode::with_code(Op::CallExtern, idx).at(span);

        let mut body = BytesMut::new();
        code.serialize(&mut body);

        Ok(Fn::with_body(name, arg_count, body.freeze()))
    }

    fn call(&mut self, span: Span, callee: Expr, args: Vec<Expr>) -> Result<Opcode, CompileError> {
        let arg_count = args.len();
        self.expr_list(args)?;
        self.expr(callee)?;

        match self.iter().last() {
            Some(opcode) if self.optimize && opcode.op() == Op::LoadFn => {
                let opcode = Opcode::with_code2(Op::CallFn, opcode.code() as u32, arg_count as u32)
                    .at(opcode.span());
                self.remote_last();
                Ok(opcode)
            }
            _ => Ok(Opcode::with_code(Op::Call, arg_count).at(span)),
        }
    }

    fn load_local(&mut self, name: &str) -> Option<&mut Var> {
        if let Some(locals) = self.locals.front_mut() {
            return locals.get_mut(name);
        }

        None
    }

    fn load_var(&mut self, span: Span, name: &str) -> Result<&mut Var, CompileError> {
        self.scope
            .iter_mut()
            .find_map(|scope| scope.vars.get_mut(name))
            .ok_or_else(|| ErrorKind::UnknownName(name.to_string()).at(span))
    }

    fn logical(&mut self, span: Span, rhs: Expr, cond: bool) -> Result<(), CompileError> {
        let offset = self.push_opcode(match cond {
            true => Opcode::new(Op::PushJumpIfTrue).at(span),
            false => Opcode::new(Op::PushJumpIfFalse).at(span),
        });

        self.expr(rhs)?;
        self.set_pos(offset, self.offset());

        Ok(())
    }

    fn expr_list(&mut self, exprs: Vec<Expr>) -> Result<(), CompileError> {
        for expr in exprs {
            self.expr(expr)?;
        }

        Ok(())
    }

    // Load a name based in the following order: self > var > local > class > func
    fn load_name(&mut self, span: Span, name: String) -> Result<Opcode, CompileError> {
        if name == "self" && self.scope.front().map(|s| s.method).unwrap_or(false) {
            return Ok(Opcode::new(Op::LoadSelf));
        }

        match self.load_var(span, &name) {
            Ok(var) => {
                check_usage(&name, var)?;
                Ok(Opcode::with_code(Op::Load, var.id).at(span))
            }
            Err(e) => match self.load_local(&name) {
                Some(var) => {
                    check_usage(&name, var)?;
                    Ok(Opcode::with_code(Op::LoadArg, var.id).at(span))
                }
                None => match self.classes.iter().position(|c| c.name == name) {
                    Some(idx) => Ok(Opcode::with_code(Op::LoadClass, idx).at(span)),
                    None => match self.funcs.iter().position(|f| f.name == name) {
                        Some(idx) => Ok(Opcode::with_code(Op::LoadFn, idx).at(span)),
                        None => Err(e),
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
                let var = self.load_var(rhs.span, &name)?;
                var.init = true;
                let idx = var.id;
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

    fn slice(
        &mut self,
        begin: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
    ) -> Result<Opcode, CompileError> {
        let mut code = 0;

        if let Some(begin) = begin {
            code += 1;
            self.expr(*begin)?;
        }

        if let Some(end) = end {
            code += 2;
            self.expr(*end)?;
        }

        Ok(Opcode::with_code(Op::MakeSlice, code))
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
                let span = elem.span;

                match elem.kind {
                    ExprKind::Range(begin, end) => self.slice(begin, end)?,
                    elem => {
                        self.expr(elem.at(span))?;
                        Opcode::new(Op::LoadElement)
                    }
                }
                .at(span)
            }
            ExprKind::Ident(name) => self.load_name(expr.span, name)?,
            ExprKind::Call(callee, args) => self.call(expr.span, *callee, args)?,
            ExprKind::Literal(lit) => Opcode::with_code(
                Op::LoadConst,
                match lit {
                    Literal::Nil => self.push_const(Const::Nil),
                    Literal::Bool(b) => self.push_const(Const::Bool(b)),
                    Literal::Int(i) => self.push_const(Const::Int(i)),
                    Literal::Float(f) => self.push_const(Const::Float(f)),
                    Literal::String(s) => self.push_const(Const::Str(s)),
                },
            )
            .at(expr.span),
            ExprKind::Range(_, _) => unreachable!(),
        };

        self.push_opcode(code);
        Ok(())
    }

    fn compile_fn_body(
        &mut self,
        scope: Scope,
        args: Vec<FnArg>,
        stmts: Vec<Stmt>,
    ) -> Result<BytesMut, CompileError> {
        let locals = args
            .into_iter()
            .enumerate()
            .map(|(i, n)| (n.name, Var::with_init(n.span, i, true)))
            .collect();

        self.scope.push_front(scope);
        self.locals.push_front(locals);

        let previous = mem::take(&mut self.body);
        self.compile_body(stmts)?;
        let scope = self.pop_scope()?;

        if let Some(locals) = self.locals.pop_front() {
            check_used(&locals)?;
        }

        let body = mem::replace(&mut self.body, previous);
        self.optim(scope.unwrap(), body)
    }

    fn method(
        &mut self,
        span: Span,
        methods: &mut HashMap<String, Fn, WyHash>,
        name: String,
        args: Vec<FnArg>,
        stmts: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        if methods.contains_key(name.as_str()) {
            return Err(ErrorKind::DuplicateMethod(name).at(span));
        }

        let func = Fn::new(name.clone(), args.len()).with_method();
        methods.insert(name.clone(), func);

        let mut body = self.compile_fn_body(Scope::with_method(), args, stmts)?;

        if name == "init" {
            [
                Opcode::new(Op::LoadSelf).at(span),
                Opcode::new(Op::Return).at(span),
            ]
            .into_iter()
            .for_each(|code| code.serialize(&mut body))
        }

        if let Some(method) = methods.get_mut(&name) {
            method.body = body.freeze();
        }

        Ok(())
    }

    fn handle_markers(&mut self, begin: usize, end: usize) {
        while let Some(marker) = self.markers.pop() {
            match marker {
                Marker::Begin(idx) => self.set_pos(idx, begin),
                Marker::End(idx) => self.set_pos(idx, end),
            }
        }
    }

    fn extern_fn_stmt(
        &mut self,
        span: Span,
        name: String,
        args: Vec<FnArg>,
    ) -> Result<(), CompileError> {
        if self.funcs.iter().any(|f| f.name == name) {
            return Err(ErrorKind::DuplicateFn(name).at(span));
        }

        let func = self.call_extern(span, name, args.len())?;
        self.funcs.push(func);

        Ok(())
    }

    fn fn_stmt(
        &mut self,
        span: Span,
        name: String,
        args: Vec<FnArg>,
        stmts: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        if self.funcs.iter().any(|f| f.name == name) {
            return Err(ErrorKind::DuplicateFn(name).at(span));
        }

        let idx = self.funcs.len();
        let func = Fn::new(name, args.len());

        self.funcs.push(func);

        let body = self.compile_fn_body(Scope::with_fn(idx), args, stmts)?;
        self.funcs[idx].body = body.freeze();

        Ok(())
    }

    fn class_stmt(
        &mut self,
        span: Span,
        class_name: String,
        methods: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        if self.classes.iter().any(|c| c.name == class_name) {
            return Err(ErrorKind::DuplicateClass(class_name).at(span));
        }

        let idx = self.classes.len();
        self.classes.push(Class::new(class_name.clone()));

        let mut funcs = HashMap::with_hasher(WyHash::default());

        for method in methods {
            let span = method.span;

            match method.kind {
                StmtKind::Fn(name, args, stmts) => {
                    self.method(span, &mut funcs, name, args, stmts)?
                }
                StmtKind::ExternFn(name, args) => {
                    if funcs.contains_key(&name) {
                        return Err(ErrorKind::DuplicateMethod(name).at(span));
                    }

                    let func = self
                        .call_extern(span, format!("{class_name}.{name}"), args.len())?
                        .with_method();
                    funcs.insert(name, func);
                }
                _ => unreachable!(),
            };
        }

        self.classes[idx].methods = funcs
            .into_iter()
            .map(|(name, func)| (Name::Owned(name), Rc::new(func)))
            .collect();

        Ok(())
    }

    fn for_stmt(&mut self, span: Span, expr: Expr, body: Vec<Stmt>) -> Result<(), CompileError> {
        let begin = self.offset();
        self.expr(expr)?;
        let offset = self.push_opcode(Opcode::new(Op::JumpIfFalse).at(span));
        self.compile_scoped_body(body)?;
        self.push_opcode(Opcode::with_code(Op::Jump, begin / 16));
        self.set_pos(offset, self.offset());
        self.handle_markers(begin, self.offset());

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
        let begin = self.offset();
        self.expr(expr)?;
        let offset = self.push_opcode(Opcode::new(Op::JumpIfFalse).at(span));
        self.compile_scoped_body(body)?;
        let post_idx = self.offset();
        self.expr(post)?;
        self.push_opcode(Opcode::with_code(Op::Jump, begin / 16));
        self.set_pos(offset, self.offset());
        self.handle_markers(post_idx, self.offset());

        Ok(())
    }

    fn if_stmt(&mut self, IfStmt(expr, stmts, alt): IfStmt) -> Result<(), CompileError> {
        let span = expr.as_ref().map(|e| e.span).unwrap_or_default();
        let end_block_offset = if let Some(expr) = expr {
            self.expr(expr)?;
            Some(self.push_opcode(Opcode::new(Op::JumpIfFalse)))
        } else {
            None
        };

        self.compile_scoped_body(stmts)?;

        match alt {
            Some(alt) => {
                let end_stmt_offset = self.push_opcode(Opcode::new(Op::Jump).at(span));

                if let Some(offset) = end_block_offset {
                    self.set_pos(offset, self.offset());
                }

                self.if_stmt(*alt)?;
                self.set_pos(end_stmt_offset, self.offset());
            }
            None => {
                if let Some(idx) = end_block_offset {
                    self.set_pos(idx, self.offset());
                }
            }
        }

        Ok(())
    }

    fn stmt(&mut self, stmt: Stmt) -> Result<(), CompileError> {
        match stmt.kind {
            StmtKind::Import(_path) => unimplemented!(),
            StmtKind::If(if_stmt) => self.if_stmt(if_stmt)?,
            StmtKind::Let(name, expr) => {
                if let Some(expr) = expr {
                    let idx = self.push_var(expr.span, name, true)?;
                    self.expr(expr)?;
                    self.push_opcode(Opcode::with_code(Op::Store, idx).at(stmt.span));
                } else {
                    self.push_var(stmt.span, name, false)?;
                }
            }
            StmtKind::Expr(expr) => {
                let assignment = expr.is_assign();
                self.expr(expr)?;

                if !assignment {
                    self.push_opcode(Opcode::new(Op::Discard).at(stmt.span));
                }
            }
            StmtKind::Return(expr) => {
                self.expr(expr)?;

                match self.iter().last() {
                    Some(opcode) if self.optimize && opcode.op() == Op::LoadArg => {
                        self.remote_last();
                        self.push_opcode(
                            Opcode::with_code(Op::ReturnArg, opcode.code()).at(stmt.span),
                        );
                    }
                    _ => {
                        self.push_opcode(Opcode::new(Op::Return).at(stmt.span));
                    }
                }
            }
            StmtKind::Break => {
                let offset = self.push_opcode(Opcode::new(Op::Jump).at(stmt.span));
                self.markers.push(Marker::End(offset));
            }
            StmtKind::Continue => {
                let offset = self.push_opcode(Opcode::new(Op::Jump).at(stmt.span));
                self.markers.push(Marker::Begin(offset));
            }
            StmtKind::Class(name, methods) => self.class_stmt(stmt.span, name, methods)?,
            StmtKind::ExternFn(name, args) => self.extern_fn_stmt(stmt.span, name, args)?,
            StmtKind::Fn(name, args, stmts) => self.fn_stmt(stmt.span, name, args, stmts)?,
            StmtKind::For(expr, body) => self.for_stmt(stmt.span, expr, body)?,
            StmtKind::ForCond(pre, expr, post, body) => {
                self.for_cond_stmt(stmt.span, *pre, expr, post, body)?
            }
        }

        Ok(())
    }

    fn compile_body(&mut self, stmts: Vec<Stmt>) -> Result<(), CompileError> {
        for stmt in stmts {
            self.stmt(stmt)?;
        }

        Ok(())
    }

    fn compile_scoped_body(&mut self, stmts: Vec<Stmt>) -> Result<(), CompileError> {
        self.scope.push_front(Scope::new());
        self.compile_body(stmts)?;
        self.pop_scope()?;

        Ok(())
    }

    fn optim_tail_call(&mut self, func: usize, body: &mut [u8]) -> Result<(), CompileError> {
        let mut iter = body
            .chunks_exact(16)
            .map(Opcode::deserialize)
            .enumerate()
            .rev();

        while let Some((i, code)) = iter.next() {
            match code.op() {
                Op::Return => continue,
                Op::CallFn if code.code2().0 as usize == func => {
                    let mut buff = &mut body[i * 16..];
                    let (arg_count, _) = code.code2();
                    Opcode::with_code(Op::TailCall, arg_count as usize).serialize(&mut buff);
                    break;
                }
                Op::Call => {
                    let arg_count = code.code();
                    let (i, idx) = match iter.next() {
                        Some((i, code)) if code.op() == Op::LoadFn => (i, code.code()),
                        _ => break,
                    };

                    if idx != func {
                        break;
                    }

                    let mut buff = &mut body[i * 16..];
                    Opcode::with_code(Op::TailCall, arg_count).serialize(&mut buff);
                    break;
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn optim(&mut self, scope: Scope, mut body: BytesMut) -> Result<BytesMut, CompileError> {
        if !self.optimize {
            return Ok(body);
        }

        if let Some(func) = scope.fn_id {
            if body
                .chunks_exact(16)
                .map(Opcode::deserialize)
                .filter(|c| match c.op() {
                    Op::LoadFn => c.code() == func,
                    Op::CallFn => c.code2().0 == func as u32,
                    _ => false,
                })
                .count()
                != 1
            {
                return Ok(body);
            }

            self.optim_tail_call(func, &mut body)?;
        }

        Ok(body)
    }

    pub fn compile(mut self, stmts: Vec<Stmt>) -> Result<Module, CompileError> {
        self.compile_scoped_body(stmts)?;
        self.pop_scope()?;

        Ok(Module {
            body: self.body.freeze(),
            consts: self.consts,
            functions: self.funcs.into_iter().map(Rc::new).collect(),
            classes: self.classes.into_iter().map(Rc::new).collect(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_code() {
        let mut compiler = Compiler::default();
        let code = Opcode::new(Op::Load).at(Span::default());
        let offset = compiler.push_opcode(code.clone());

        assert_eq!(0, offset);
        assert_eq!(code, compiler.iter().last().unwrap());
        assert_eq!(16, compiler.body.len());

        let code2 = Opcode::new(Op::Lt).at(Span::default());
        let offset = compiler.push_opcode(code2.clone());

        assert_eq!(16, offset);
        assert_eq!(code2, compiler.iter().last().unwrap());
        assert_eq!(32, compiler.body.len());
    }

    #[test]
    fn test_replace_code() {
        let mut compiler = Compiler::default();
        let code = Opcode::new(Op::Load).at(Span::default());
        let offset = compiler.push_opcode(code.clone());

        compiler.push_opcode(Opcode::new(Op::Lt).at(Span::default()));
        compiler.set_pos(offset, 1600);

        let code = compiler.iter().next().unwrap();
        assert_eq!(Op::Load, code.op());
        assert_eq!(100, code.code());
    }

    #[test]
    fn test_scope() {
        let mut compiler = Compiler::default();
        let top = compiler
            .push_var(Span::default(), "n".to_string(), true)
            .unwrap();
        compiler.scope.push_front(Scope::new());

        let expected = compiler
            .push_var(Span::default(), "n".to_string(), true)
            .unwrap();
        let actual = compiler.load_var(Span::default(), "n").unwrap();

        assert_eq!(expected, actual.id);
        assert_ne!(top, actual.id);
    }

    #[test]
    fn test_assign() {
        let mut compiler = Compiler::default();
        let ident = |name: &str| ExprKind::Ident(name.to_string()).at(Span::default());

        let expr = ExprKind::Literal(Literal::Int(100)).at(Span::default());
        let idx = compiler
            .push_var(Span::default(), "n".to_string(), true)
            .unwrap();
        let code = compiler
            .assign(Span::default(), None, ident("n"), expr.clone())
            .unwrap();

        assert_eq!(Op::Store, code.op());
        assert_eq!(idx, code.code());

        let result = compiler.assign(Span::default(), None, ident("y"), expr);
        assert!(result.is_err());
    }
}
