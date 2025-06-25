use std::{
    borrow::Cow,
    collections::{HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
    io, mem,
};

use bytes::{Bytes, BytesMut};
use wyhash2::WyHash;

use crate::{
    ast::{self, Expr, ExprKind, FnArg, IfStmt, Literal, MatchArm, Stmt, StmtKind},
    bytecode::{Bytecode, Const, Op, Serializable, Spanned},
    collections::{OrderedMap, OrderedSet},
    error::{IntoSpanned, SpannedError},
    lexer::Span,
    runtime::function::Fn,
};

#[derive(Debug, Default)]
pub struct Class {
    pub name: Cow<'static, str>,
    pub public: bool,
    pub methods: HashMap<Cow<'static, str>, Fn>,
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Class {
    pub fn new(name: impl Into<Cow<'static, str>>) -> Self {
        Self {
            name: name.into(),
            ..Self::default()
        }
    }
}

#[derive(Debug, Default)]
pub struct Package {
    pub body: Bytes,
    pub consts: Vec<Const>,
    pub classes: Vec<Class>,
    pub functions: Vec<Fn>,
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown name '{0}'")]
    UnknownName(String),
    #[error("name '{0}' is already defined")]
    DuplicateName(String),
    #[error("package '{0}' already imported")]
    DuplicateImport(String),
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
    #[error("failed to write bytecode(s): {0}")]
    FailedWriteBytecode(#[from] io::Error),
}

pub type CompileError = SpannedError<ErrorKind>;

#[derive(Debug, Default, PartialEq)]
struct Var {
    id: usize,
    init: bool,
    used: bool,
    span: Span,
}

struct VarBuilder {
    var: Var,
}

impl VarBuilder {
    fn new(id: usize, span: Span) -> Self {
        Self {
            var: Var {
                id,
                init: false,
                used: false,
                span,
            },
        }
    }

    pub fn init(mut self, init: bool) -> Self {
        self.var.init = init;
        self
    }

    pub fn used(mut self, used: bool) -> Self {
        self.var.used = used;
        self
    }

    pub fn build(self) -> Var {
        self.var
    }
}

#[derive(Default)]
struct Scope {
    func: Option<u32>,
    vars: HashMap<String, Var, WyHash>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            func: None,
            ..Self::default()
        }
    }

    pub fn with_fn(fn_id: u32) -> Self {
        Self {
            func: Some(fn_id),
            ..Self::default()
        }
    }
}

enum Marker {
    Begin(usize),
    End(usize),
}

fn check_unused(vars: &HashMap<String, Var, WyHash>) -> Result<(), CompileError> {
    if let Some((name, var)) = vars
        .iter()
        .filter(|(name, _)| name.as_str() != "self" && !name.starts_with("_"))
        .find(|(_, var)| !var.used)
    {
        return Err(ErrorKind::NameUnused(name.to_string()).at(var.span));
    }

    Ok(())
}

#[derive(Clone, Copy)]
enum Symbol {
    Local(u32),
    Var(u32),
    Class(u32),
    Fn(u32),
}

pub struct Context {
    pub atoms: OrderedSet<String>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            atoms: OrderedSet::new([
                "false".to_string(),
                "true".to_string(),
                "nil".to_string(),
                "instance".to_string(),
            ]),
        }
    }
}

pub struct Compiler {
    optimize: bool,
    vars_seq: usize,
    names: HashSet<String>,
    scope: VecDeque<Scope>,
    body: BytesMut,
    consts: OrderedSet<Const>,
    funcs: OrderedMap<String, Fn>,
    classes: OrderedMap<String, Class>,
    markers: Vec<Marker>,
    imports: HashSet<String>,
    locals: VecDeque<HashMap<String, Var, WyHash>>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            vars_seq: 0,
            body: BytesMut::new(),
            funcs: OrderedMap::default(),
            consts: OrderedSet::default(),
            classes: OrderedMap::default(),
            markers: vec![],
            imports: HashSet::default(),
            names: HashSet::default(),
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
            check_unused(&scope.vars)?;
            return Ok(Some(scope));
        }

        Ok(None)
    }

    fn push_bytecode(&mut self, bytecode: Spanned<Bytecode>) -> usize {
        let offset = self.body.len();
        bytecode.serialize(&mut self.body);
        offset
    }

    fn set_offset(&mut self, offset: usize, new_offset: usize) {
        let orig = Spanned::<Bytecode>::deserialize(&mut &self.body[offset..offset + 8]);
        let (code1, _) = orig.code2();
        let mut buff = &mut self.body[offset..];
        let bc = Bytecode::with_code2(orig.op, code1, (new_offset / 8) as u16);
        bc.at(orig.span).serialize(&mut buff);
    }

    fn tail(&self) -> Option<Spanned<Bytecode>> {
        if self.body.is_empty() {
            None
        } else {
            Some(Spanned::<Bytecode>::deserialize(
                &mut &self.body[self.body.len() - 8..],
            ))
        }
    }

    fn remove_tail(&mut self) {
        self.body.truncate(self.body.len() - 8);
    }

    fn push_hidden_var(&mut self, span: Span, init: bool) -> Result<usize, CompileError> {
        let id = self.vars_seq;
        let name = format!("__{}", self.vars_seq);

        self.vars_seq += 1;

        if let Some(scope) = self.scope.front_mut() {
            scope.vars.insert(
                name,
                VarBuilder::new(id, span).init(init).used(true).build(),
            );
        }

        Ok(id)
    }

    fn push_var(&mut self, span: Span, name: String, init: bool) -> Result<usize, CompileError> {
        if self.names.contains(&name) {
            return Err(ErrorKind::DuplicateName(name).at(span));
        }

        let id = self.vars_seq;
        self.vars_seq += 1;

        if let Some(scope) = self.scope.front_mut() {
            scope
                .vars
                .insert(name, VarBuilder::new(id, span).init(init).build());
        }

        Ok(id)
    }

    fn call_extern(
        &mut self,
        span: Span,
        name: String,
        arg_count: usize,
        public: bool,
    ) -> Result<Fn, CompileError> {
        let idx = self.consts.insert(Const::Str(name.clone()));
        let code = Bytecode::with_code(Op::CallExtern, idx).at(span);
        let mut body = BytesMut::new();
        code.serialize(&mut body);

        Ok(Fn::builder()
            .name(name)
            .public(public)
            .arg_count(arg_count as u32)
            .body(body.freeze())
            .build())
    }

    fn call(
        &mut self,
        ctx: &mut Context,
        span: Span,
        callee: Expr,
        args: Vec<Expr>,
    ) -> Result<Spanned<Bytecode>, CompileError> {
        let arg_count = args.len();
        self.expr_list(ctx, args)?;
        self.expr(ctx, callee)?;

        match self.tail() {
            Some(bc) if self.optimize && bc.op == Op::LoadFn => {
                let opcode =
                    Bytecode::with_code2(Op::CallFn, bc.code as u16, arg_count as u16).at(bc.span);
                self.remove_tail();
                Ok(opcode)
            }
            _ => Ok(Bytecode::with_code(Op::Call, arg_count as u32).at(span)),
        }
    }

    fn load_local(&mut self, name: &str) -> Option<&mut Var> {
        if let Some(locals) = self.locals.front_mut() {
            return locals.get_mut(name);
        }

        None
    }

    fn load_var(&mut self, name: &str) -> Option<&mut Var> {
        self.scope
            .iter_mut()
            .find_map(|scope| scope.vars.get_mut(name))
    }

    fn logical(
        &mut self,
        ctx: &mut Context,
        span: Span,
        rhs: Expr,
        cond: bool,
    ) -> Result<(), CompileError> {
        let offset = self.push_bytecode(match cond {
            true => Bytecode::new(Op::PushJumpIfTrue).at(span),
            false => Bytecode::new(Op::PushJumpIfFalse).at(span),
        });

        self.expr(ctx, rhs)?;
        self.set_offset(offset, self.offset());

        Ok(())
    }

    fn expr_list(&mut self, ctx: &mut Context, exprs: Vec<Expr>) -> Result<(), CompileError> {
        for expr in exprs {
            self.expr(ctx, expr)?;
        }

        Ok(())
    }

    fn set_init(&mut self, sym: Symbol) {
        match sym {
            Symbol::Local(id) => {
                if let Some(locals) = self.locals.front_mut() {
                    if let Some(var) = locals.values_mut().find(|local| local.id == id as usize) {
                        var.init = true;
                    }
                }
            }
            Symbol::Var(id) => {
                if let Some(var) = self
                    .scope
                    .iter_mut()
                    .find_map(|scope| scope.vars.values_mut().find(|var| var.id == id as usize))
                {
                    var.init = true;
                }
            }
            Symbol::Class(_) | Symbol::Fn(_) => {}
        }
    }

    // Load a symbol based on the following order: local > var > class > func
    fn load_symbol(
        &mut self,
        span: Span,
        name: &str,
        check_init: bool,
    ) -> Result<Symbol, CompileError> {
        match self.load_local(name) {
            Some(local) => {
                local.used = true;

                if check_init && !local.init {
                    return Err(ErrorKind::NameUninitialized(name.to_string()).at(local.span));
                }

                Ok(Symbol::Local(local.id as u32))
            }
            None => match self.load_var(name) {
                Some(var) => {
                    var.used = true;

                    if check_init && !var.init {
                        return Err(ErrorKind::NameUninitialized(name.to_string()).at(var.span));
                    }

                    Ok(Symbol::Var(var.id as u32))
                }
                None => match self.classes.get(name) {
                    Some(idx) => Ok(Symbol::Class(idx)),
                    None => match self.funcs.get(name) {
                        Some(idx) => Ok(Symbol::Fn(idx)),
                        None => Err(ErrorKind::UnknownName(name.to_string()).at(span)),
                    },
                },
            },
        }
    }

    fn load_name(&mut self, span: Span, name: &str) -> Result<Spanned<Bytecode>, CompileError> {
        Ok(match self.load_symbol(span, name, true)? {
            Symbol::Local(id) => Bytecode::with_code(Op::LoadLocal, id).at(span),
            Symbol::Var(id) => Bytecode::with_code(Op::Load, id).at(span),
            Symbol::Class(id) => Bytecode::with_code(Op::LoadClass, id).at(span),
            Symbol::Fn(id) => Bytecode::with_code(Op::LoadFn, id).at(span),
        })
    }

    fn assign(
        &mut self,
        ctx: &mut Context,
        span: Span,
        op: Option<ast::AssignOp>,
        lhs: Expr,
        mut rhs: Expr,
    ) -> Result<Spanned<Bytecode>, CompileError> {
        if let Some(op) = op {
            rhs = ExprKind::Binary(Box::new(lhs.clone()), op.into(), Box::new(rhs)).at(span);
        }

        Ok(match lhs.kind {
            ExprKind::Ident(name) => {
                let sym = self.load_symbol(span, &name, false)?;
                let bc = match sym {
                    Symbol::Local(id) => Bytecode::with_code(Op::StoreLocal, id).at(span),
                    Symbol::Var(id) => Bytecode::with_code(Op::Store, id).at(span),
                    _ => return Err(ErrorKind::UnknownName(name).at(span)),
                };

                self.expr(ctx, rhs)?;
                self.set_init(sym);

                bc
            }
            ExprKind::Member(object, member) => {
                self.expr(ctx, *object)?;
                self.expr(ctx, rhs)?;
                let idx = ctx.atoms.insert(member);
                Bytecode::with_code(Op::StoreMember, idx).at(span)
            }
            ExprKind::CompMember(object, index) => {
                self.expr(ctx, *object)?;
                self.expr(ctx, *index)?;
                self.expr(ctx, rhs)?;
                Bytecode::new(Op::StoreElement).at(span)
            }
            _ => unimplemented!(),
        })
    }

    fn slice(
        &mut self,
        ctx: &mut Context,
        begin: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
    ) -> Result<Bytecode, CompileError> {
        let mut code = 0;

        if let Some(begin) = begin {
            code += 1;
            self.expr(ctx, *begin)?;
        }

        if let Some(end) = end {
            code += 2;
            self.expr(ctx, *end)?;
        }

        Ok(Bytecode::with_code(Op::MakeSlice, code))
    }

    fn expr(&mut self, ctx: &mut Context, expr: Expr) -> Result<(), CompileError> {
        let code = match expr.kind {
            ExprKind::Unary(op, unary_expr) => match op {
                ast::UnaryOp::Not => {
                    let span = expr.span;
                    self.expr(ctx, *unary_expr)?;
                    Bytecode::new(Op::UnaryNot).at(span)
                }
            },
            ExprKind::Assign(op, lhs, rhs) => self.assign(ctx, expr.span, op, *lhs, *rhs)?,
            ExprKind::Binary(lhs, op, rhs) => {
                self.expr(ctx, *lhs)?;

                let code = match op {
                    ast::BinaryOp::Add => Bytecode::new(Op::Add).at(expr.span),
                    ast::BinaryOp::Sub => Bytecode::new(Op::Sub).at(expr.span),
                    ast::BinaryOp::Mul => Bytecode::new(Op::Mul).at(expr.span),
                    ast::BinaryOp::Rem => Bytecode::new(Op::Rem).at(expr.span),
                    ast::BinaryOp::Div => Bytecode::new(Op::Div).at(expr.span),
                    ast::BinaryOp::Eq => Bytecode::new(Op::Eq).at(expr.span),
                    ast::BinaryOp::Ne => Bytecode::new(Op::Ne).at(expr.span),
                    ast::BinaryOp::Gt => Bytecode::new(Op::Gt).at(expr.span),
                    ast::BinaryOp::Gte => Bytecode::new(Op::Gte).at(expr.span),
                    ast::BinaryOp::Lt => Bytecode::new(Op::Lt).at(expr.span),
                    ast::BinaryOp::Lte => Bytecode::new(Op::Lte).at(expr.span),
                    ast::BinaryOp::Or => return self.logical(ctx, expr.span, *rhs, true),
                    ast::BinaryOp::And => return self.logical(ctx, expr.span, *rhs, false),
                    ast::BinaryOp::BitOr => Bytecode::new(Op::BitwiseOr).at(expr.span),
                    ast::BinaryOp::BitAnd => Bytecode::new(Op::BitwiseAnd).at(expr.span),
                    ast::BinaryOp::ShiftLeft => Bytecode::new(Op::ShiftLeft).at(expr.span),
                    ast::BinaryOp::ShiftRight => Bytecode::new(Op::ShiftRight).at(expr.span),
                    ast::BinaryOp::Xor => Bytecode::new(Op::BitwiseXor).at(expr.span),
                };

                self.expr(ctx, *rhs)?;
                code
            }
            ExprKind::Array(items) => {
                let len = items.len();
                self.expr_list(ctx, items)?;
                Bytecode::with_code(Op::MakeArray, len as u32).at(expr.span)
            }
            ExprKind::Member(object, member) => {
                self.expr(ctx, *object)?;
                Bytecode::with_code(Op::LoadMember, ctx.atoms.insert(member)).at(expr.span)
            }
            ExprKind::CompMember(object, elem) => {
                self.expr(ctx, *object)?;
                let span = elem.span;

                match elem.kind {
                    ExprKind::Range(begin, end) => self.slice(ctx, begin, end)?,
                    elem => {
                        self.expr(ctx, elem.at(span))?;
                        Bytecode::new(Op::LoadElement)
                    }
                }
                .at(span)
            }
            ExprKind::Ident(name) => self.load_name(expr.span, &name)?,
            ExprKind::Call(callee, args) => self.call(ctx, expr.span, *callee, args)?,
            ExprKind::Literal(Literal::Atom(name)) => {
                Bytecode::with_code(Op::LoadAtom, ctx.atoms.insert(name)).at(expr.span)
            }
            ExprKind::Literal(Literal::Int(i)) if i > 0 && u32::try_from(i).is_ok() => {
                Bytecode::with_code(Op::LoadConstInt, i as u32).at(expr.span)
            }
            ExprKind::Literal(lit) => Bytecode::with_code(
                Op::LoadConst,
                match lit {
                    Literal::Int(i) => self.consts.insert(Const::Int(i)),
                    Literal::BigInt(i) => self.consts.insert(Const::BigInt(i)),
                    Literal::Float(f) => self.consts.insert(Const::Float(f)),
                    Literal::String(s) => self.consts.insert(Const::Str(s)),
                    _ => unreachable!(),
                },
            )
            .at(expr.span),
            ExprKind::Match(expr, arms, alt) => {
                self.match_expr(ctx, *expr, arms, alt)?;
                return Ok(());
            }
            ExprKind::Range(_, _) => unreachable!(),
        };

        self.push_bytecode(code);
        Ok(())
    }

    fn match_expr(
        &mut self,
        ctx: &mut Context,
        expr: Expr,
        arms: Vec<MatchArm>,
        alt: Option<Box<Expr>>,
    ) -> Result<(), CompileError> {
        let span = expr.span;
        let id = self.push_hidden_var(expr.span, true)?;

        self.expr(ctx, expr)?;
        self.push_bytecode(Bytecode::with_code(Op::Store, id as u32).at(span));

        let mut offsets = vec![];

        for arm in arms {
            let span = arm.pat.span;

            self.push_bytecode(Bytecode::with_code(Op::Load, id as u32).at(span));
            self.expr(ctx, arm.pat)?;
            self.push_bytecode(Bytecode::new(Op::Eq).at(span));
            let offset = self.push_bytecode(Bytecode::with_code(Op::JumpIfFalse, 0).at(span));
            self.expr(ctx, arm.expr)?;
            offsets.push(self.push_bytecode(Bytecode::with_code(Op::Jump, 0).at(span)));
            self.set_offset(offset, self.offset());
        }

        if let Some(expr) = alt {
            self.expr(ctx, *expr)?;
        }

        let end_offset = self.offset();

        for offset in offsets {
            self.set_offset(offset, end_offset);
        }

        Ok(())
    }

    fn compile_fn_body(
        &mut self,
        ctx: &mut Context,
        scope: Scope,
        args: Vec<FnArg>,
        stmts: Vec<Stmt>,
    ) -> Result<BytesMut, CompileError> {
        let locals = args
            .into_iter()
            .enumerate()
            .map(|(i, n)| (n.name, VarBuilder::new(i, n.span).init(true).build()))
            .collect();

        self.scope.push_front(scope);
        self.locals.push_front(locals);

        let previous = mem::take(&mut self.body);
        self.compile_body(ctx, stmts)?;
        let scope = self.pop_scope()?;

        if let Some(locals) = self.locals.pop_front() {
            check_unused(&locals)?;
        }

        let body = mem::replace(&mut self.body, previous);
        self.optim(scope.unwrap(), body)
    }

    fn method(
        &mut self,
        ctx: &mut Context,
        span: Span,
        methods: &mut HashMap<String, Fn, WyHash>,
        name: String,
        args: Vec<FnArg>,
        stmts: Vec<Stmt>,
        public: bool,
    ) -> Result<(), CompileError> {
        if methods.contains_key(name.as_str()) {
            return Err(ErrorKind::DuplicateMethod(name).at(span));
        }

        methods.insert(
            name.clone(),
            Fn::builder()
                .name(name.clone())
                .arg_count(args.len() as u32)
                .public(public)
                .build(),
        );

        let mut body = self.compile_fn_body(ctx, Scope::new(), args, stmts)?;

        if name == "init" {
            Bytecode::with_code(Op::ReturnLocal, 0).serialize(&mut body);
        }

        if let Some(method) = methods.get_mut(&name) {
            method.body = body.freeze();
        }

        Ok(())
    }

    fn handle_markers(&mut self, begin: usize, end: usize) {
        while let Some(marker) = self.markers.pop() {
            match marker {
                Marker::Begin(idx) => self.set_offset(idx, begin),
                Marker::End(idx) => self.set_offset(idx, end),
            }
        }
    }

    fn extern_fn_stmt(
        &mut self,
        span: Span,
        name: String,
        args: Vec<FnArg>,
        public: bool,
    ) -> Result<(), CompileError> {
        if self.funcs.contains_key(&name) {
            return Err(ErrorKind::DuplicateFn(name).at(span));
        }

        let func = self.call_extern(span, name.clone(), args.len(), public)?;
        self.funcs.insert(name, func);

        Ok(())
    }

    fn fn_stmt(
        &mut self,
        ctx: &mut Context,
        span: Span,
        name: String,
        args: Vec<FnArg>,
        stmts: Vec<Stmt>,
        public: bool,
    ) -> Result<(), CompileError> {
        if self.funcs.contains_key(&name) {
            return Err(ErrorKind::DuplicateFn(name).at(span));
        }

        let idx = self.funcs.insert(
            name.clone(),
            Fn::builder()
                .name(name.clone())
                .public(public)
                .arg_count(args.len() as u32)
                .build(),
        );

        self.funcs[&name].body = self
            .compile_fn_body(ctx, Scope::with_fn(idx), args, stmts)?
            .freeze();

        Ok(())
    }

    fn class_stmt(
        &mut self,
        ctx: &mut Context,
        span: Span,
        name: String,
        methods: Vec<Stmt>,
        public: bool,
    ) -> Result<(), CompileError> {
        if self.classes.contains_key(&name) {
            return Err(ErrorKind::DuplicateClass(name).at(span));
        }

        let mut class = Class::new(name.clone());
        class.public = public;

        self.classes.insert(name.clone(), class);

        let mut funcs = HashMap::with_hasher(WyHash::default());

        for method in methods {
            let span = method.span;

            match method.kind {
                StmtKind::Fn {
                    name,
                    args,
                    body,
                    public,
                } => self.method(ctx, span, &mut funcs, name, args, body, public)?,
                StmtKind::ExternFn(name, args, public) => {
                    if funcs.contains_key(&name) {
                        return Err(ErrorKind::DuplicateMethod(name).at(span));
                    }

                    funcs.insert(
                        name.clone(),
                        self.call_extern(span, name, args.len(), public)?,
                    );
                }
                _ => unreachable!(),
            };
        }

        self.classes[&name].methods = funcs
            .into_iter()
            .map(|(method, func)| (Cow::Owned(method), func))
            .collect();

        Ok(())
    }

    fn for_stmt(
        &mut self,
        ctx: &mut Context,
        span: Span,
        expr: Expr,
        body: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        let begin = self.offset();
        self.expr(ctx, expr)?;
        let offset = self.push_bytecode(Bytecode::new(Op::JumpIfFalse).at(span));
        self.compile_scoped_body(ctx, body)?;
        self.push_bytecode(Bytecode::with_code(Op::Jump, (begin as u32) / 8).at(span));
        self.set_offset(offset, self.offset());
        self.handle_markers(begin, self.offset());

        Ok(())
    }

    fn for_cond_stmt(
        &mut self,
        ctx: &mut Context,
        span: Span,
        pre: Stmt,
        expr: Expr,
        post: Expr,
        body: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        self.stmt(ctx, pre)?;
        let begin = self.offset();
        self.expr(ctx, expr)?;
        let offset = self.push_bytecode(Bytecode::new(Op::JumpIfFalse).at(span));
        self.compile_scoped_body(ctx, body)?;
        let post_idx = self.offset();
        self.expr(ctx, post)?;
        self.push_bytecode(Bytecode::with_code(Op::Jump, (begin as u32) / 8).at(span));
        self.set_offset(offset, self.offset());
        self.handle_markers(post_idx, self.offset());

        Ok(())
    }

    fn if_stmt(
        &mut self,
        ctx: &mut Context,
        IfStmt(expr, stmts, alt): IfStmt,
    ) -> Result<(), CompileError> {
        let span = expr.as_ref().map(|e| e.span).unwrap_or_default();
        let end_block_offset = if let Some(expr) = expr {
            self.expr(ctx, expr)?;
            Some(self.push_bytecode(Bytecode::new(Op::JumpIfFalse).at(span)))
        } else {
            None
        };

        self.compile_scoped_body(ctx, stmts)?;

        match alt {
            Some(alt) => {
                let end_stmt_offset = self.push_bytecode(Bytecode::new(Op::Jump).at(span));

                if let Some(offset) = end_block_offset {
                    self.set_offset(offset, self.offset());
                }

                self.if_stmt(ctx, *alt)?;
                self.set_offset(end_stmt_offset, self.offset());
            }
            None => {
                if let Some(idx) = end_block_offset {
                    self.set_offset(idx, self.offset());
                }
            }
        }

        Ok(())
    }

    fn stmt(&mut self, ctx: &mut Context, stmt: Stmt) -> Result<(), CompileError> {
        match stmt.kind {
            StmtKind::Import(path) => {
                let name = path.join("/");

                if self.imports.contains(&name) {
                    return Err(ErrorKind::DuplicateImport(name).at(stmt.span));
                }

                self.imports.insert(name.clone());

                let idx = self.consts.insert(Const::Str(name.clone()));
                self.push_bytecode(Bytecode::with_code(Op::Import, idx).at(stmt.span));

                let var_name = path.last().cloned().unwrap_or_default();

                let idx = self.push_var(stmt.span, var_name.clone(), true)?;
                self.push_bytecode(Bytecode::with_code(Op::Store, idx as u32).at(stmt.span));

                self.names.insert(var_name);
            }
            StmtKind::If(if_stmt) => self.if_stmt(ctx, if_stmt)?,
            StmtKind::Let(name, expr) => {
                if let Some(expr) = expr {
                    let idx = self.push_var(expr.span, name, true)?;
                    self.expr(ctx, expr)?;
                    self.push_bytecode(Bytecode::with_code(Op::Store, idx as u32).at(stmt.span));
                } else {
                    self.push_var(stmt.span, name, false)?;
                }
            }
            StmtKind::Expr(expr) => {
                let assignment = expr.is_assign();
                self.expr(ctx, expr)?;

                if !assignment {
                    self.push_bytecode(Bytecode::new(Op::Discard).at(stmt.span));
                }
            }
            StmtKind::Return(expr) => {
                self.expr(ctx, expr)?;

                match self.tail() {
                    Some(opcode) if self.optimize && opcode.op == Op::LoadLocal => {
                        self.remove_tail();
                        self.push_bytecode(
                            Bytecode::with_code(Op::ReturnLocal, opcode.code).at(stmt.span),
                        );
                    }
                    _ => {
                        self.push_bytecode(Bytecode::new(Op::Return).at(stmt.span));
                    }
                }
            }
            StmtKind::Break => {
                let offset = self.push_bytecode(Bytecode::new(Op::Jump).at(stmt.span));
                self.markers.push(Marker::End(offset));
            }
            StmtKind::Continue => {
                let offset = self.push_bytecode(Bytecode::new(Op::Jump).at(stmt.span));
                self.markers.push(Marker::Begin(offset));
            }
            StmtKind::Class(name, methods, public) => {
                self.class_stmt(ctx, stmt.span, name, methods, public)?
            }
            StmtKind::ExternFn(name, args, public) => {
                self.extern_fn_stmt(stmt.span, name, args, public)?
            }
            StmtKind::Fn {
                name,
                args,
                body,
                public,
            } => self.fn_stmt(ctx, stmt.span, name, args, body, public)?,
            StmtKind::For(expr, body) => self.for_stmt(ctx, stmt.span, expr, body)?,
            StmtKind::ForCond {
                init,
                cond,
                step,
                body,
            } => self.for_cond_stmt(ctx, stmt.span, *init, cond, step, body)?,
        }

        Ok(())
    }

    fn compile_body(&mut self, ctx: &mut Context, stmts: Vec<Stmt>) -> Result<(), CompileError> {
        for stmt in stmts {
            self.stmt(ctx, stmt)?;
        }

        Ok(())
    }

    fn compile_scoped_body(
        &mut self,
        ctx: &mut Context,
        stmts: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        self.scope.push_front(Scope::new());
        self.compile_body(ctx, stmts)?;
        self.pop_scope()?;

        Ok(())
    }

    fn optim_tail_call(&mut self, func: u32, body: &mut [u8]) -> Result<(), CompileError> {
        let mut iter = body
            .chunks_exact(8)
            .map(|mut chunk| Spanned::<Bytecode>::deserialize(&mut chunk))
            .enumerate()
            .rev();

        while let Some((i, code)) = iter.next() {
            match code.op {
                Op::Return => continue,
                Op::CallFn if code.code2().0 as u32 == func => {
                    let mut buff = &mut body[i * 8..];
                    let (_, arg_count) = code.code2();
                    Bytecode::with_code(Op::TailCall, arg_count as u32).serialize(&mut buff);
                    break;
                }
                Op::Call => {
                    let arg_count = code.code;
                    let (i, idx) = match iter.next() {
                        Some((i, code)) if code.op == Op::LoadFn => (i, code.code),
                        _ => break,
                    };

                    if idx != func {
                        break;
                    }

                    let mut buff = &mut body[i * 8..];
                    Bytecode::with_code(Op::TailCall, arg_count).serialize(&mut buff);
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

        if let Some(func) = scope.func {
            if body
                .chunks_exact(8)
                .map(|mut buff| Spanned::<Bytecode>::deserialize(&mut buff))
                .filter(|c| match c.op {
                    Op::LoadFn => c.code == func,
                    Op::CallFn => c.code2().0 as u32 == func,
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

    pub fn compile(mut self, ctx: &mut Context, stmts: Vec<Stmt>) -> Result<Package, CompileError> {
        // In the root scope, `self` refers to the current package
        self.push_var(Span::default(), "self".to_string(), true)?;
        self.compile_scoped_body(ctx, stmts)?;
        self.pop_scope()?;

        Ok(Package {
            body: self.body.freeze(),
            consts: self.consts.into_vec(),
            functions: self.funcs.into_vec(),
            classes: self.classes.into_vec(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_code() {
        let mut compiler = Compiler::default();
        let bc = Bytecode::new(Op::Load).at(Span::default());
        let offset = compiler.push_bytecode(bc.clone());

        assert_eq!(0, offset);
        assert_eq!(bc, compiler.tail().unwrap());
        assert_eq!(8, compiler.body.len());

        let bc = Bytecode::new(Op::Lt).at(Span::default());
        let offset = compiler.push_bytecode(bc.clone());

        assert_eq!(8, offset);
        assert_eq!(bc, compiler.tail().unwrap());
        assert_eq!(16, compiler.body.len());
    }

    #[test]
    fn test_replace_code() {
        let mut compiler = Compiler::default();
        let code = Bytecode::new(Op::Load).at(Span::default());
        let offset = compiler.push_bytecode(code.clone());

        compiler.push_bytecode(Bytecode::new(Op::Lt).at(Span::default()));
        compiler.set_offset(offset, 800);

        let code = Spanned::<Bytecode>::deserialize(&mut &compiler.body[..8]);
        assert_eq!(Op::Load, code.op);
        assert_eq!(100, code.code);
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
        let actual = compiler.load_var("n").unwrap();

        assert_eq!(expected, actual.id);
        assert_ne!(top, actual.id);
    }

    #[test]
    fn test_assign() {
        let mut ctx = Context::default();
        let mut compiler = Compiler::default();
        let ident = |name: &str| ExprKind::Ident(name.to_string()).at(Span::default());

        let expr = ExprKind::Literal(Literal::Int(100)).at(Span::default());
        let idx = compiler
            .push_var(Span::default(), "n".to_string(), true)
            .unwrap();
        let code = compiler
            .assign(&mut ctx, Span::default(), None, ident("n"), expr.clone())
            .unwrap();

        assert_eq!(Op::Store, code.op);
        assert_eq!(idx, code.code as usize);

        let result = compiler.assign(&mut ctx, Span::default(), None, ident("y"), expr);
        assert!(result.is_err());
    }
}
