use std::{
    borrow::Cow,
    collections::{HashMap, HashSet, VecDeque},
    hash::{Hash, Hasher},
    io, mem,
};

use bytes::{Bytes, BytesMut};
use lazy_static::lazy_static;
use wyhash2::WyHash;

use crate::{
    ast::{
        self, BinaryOp, Expr, ExprKind, FnArg, FnStmt, IfStmt, Literal, MatchArm, Stmt, StmtKind,
    },
    bytecode::{self, Bytecode, Const, Op, Serializable},
    collections::{IntMap, OrderedMap, OrderedSet},
    error::{IntoSpanned, SpannedError},
    lexer::{Span, Spanned},
    runtime::{value, Fn},
};

const PRELUDE: [&'static str; 6] = ["enum", "chunks", "each", "range", "repeat", "println"];

lazy_static! {
    static ref BINARY_OPS: HashMap<ast::BinaryOp, Op> = {
        [
            (ast::BinaryOp::Add, Op::Add),
            (ast::BinaryOp::Sub, Op::Sub),
            (ast::BinaryOp::Mul, Op::Mul),
            (ast::BinaryOp::Rem, Op::Rem),
            (ast::BinaryOp::Div, Op::Div),
            (ast::BinaryOp::Eq, Op::Eq),
            (ast::BinaryOp::Ne, Op::Ne),
            (ast::BinaryOp::Gt, Op::Gt),
            (ast::BinaryOp::Gte, Op::Gte),
            (ast::BinaryOp::Lt, Op::Lt),
            (ast::BinaryOp::Lte, Op::Lte),
            (ast::BinaryOp::TypeAssert, Op::TypeAssert),
            (ast::BinaryOp::BitOr, Op::BitwiseOr),
            (ast::BinaryOp::BitAnd, Op::BitwiseAnd),
            (ast::BinaryOp::ShiftLeft, Op::ShiftLeft),
            (ast::BinaryOp::ShiftRight, Op::ShiftRight),
            (ast::BinaryOp::Xor, Op::BitwiseXor),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

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
    pub offsets: IntMap<usize, Span>,
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
    vars: HashMap<String, Var, WyHash>,
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

pub struct GlobalContext {
    pub atoms: OrderedSet<String>,
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self {
            atoms: OrderedSet::new([
                "false".to_string(),
                "true".to_string(),
                "nil".to_string(),
                "module".to_string(),
            ]),
        }
    }
}

#[derive(Default)]
struct Seq(usize);

impl Seq {
    fn next(&mut self) -> usize {
        let id = self.0;
        self.0 += 1;
        id
    }
}

pub struct Compiler {
    vars: Seq,
    body: BytesMut,
    offsets: IntMap<usize, Span>,
    markers: Vec<Marker>,
    names: HashSet<String>,
    scope: VecDeque<Scope>,
    imports: HashSet<String>,
    consts: OrderedSet<Const>,
    funcs: OrderedMap<String, Fn>,
    classes: OrderedMap<String, Class>,
    locals: VecDeque<HashMap<String, Var, WyHash>>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            vars: Seq::default(),
            body: BytesMut::new(),
            offsets: IntMap::default(),
            markers: vec![],
            names: HashSet::default(),
            funcs: OrderedMap::default(),
            consts: OrderedSet::default(),
            classes: OrderedMap::default(),
            imports: HashSet::default(),
            locals: VecDeque::default(),
            scope: VecDeque::from(vec![Scope::default()]),
        }
    }
}

impl Compiler {
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

    fn push(&mut self, bytecode: Spanned<Bytecode>) -> usize {
        let offset = self.body.len();
        bytecode.inner.serialize(&mut self.body);
        self.offsets.insert(offset, bytecode.span);
        offset
    }

    fn set_offset(&mut self, offset: usize, new_offset: usize) {
        let orig = Bytecode::deserialize(&mut &self.body[offset..offset + bytecode::SIZE]);
        Bytecode::with_code(orig.op, (new_offset / bytecode::SIZE) as u32)
            .serialize(&mut &mut self.body[offset..]);
    }

    fn accept(&mut self, op: Op) -> Option<Spanned<Bytecode>> {
        if self.body.is_empty() {
            return None;
        }

        let offset = self.body.len() - bytecode::SIZE;
        let span = self.offsets.get(&offset).copied().unwrap_or_default();
        let bc = Bytecode::deserialize(&mut &self.body[offset..]);

        if bc.op == op {
            self.body.truncate(offset);
            return Some(bc.at(span));
        }

        None
    }

    fn push_hidden_var(&mut self, span: Span, init: bool) -> Result<usize, CompileError> {
        let id = self.vars.next();

        if let Some(scope) = self.scope.front_mut() {
            scope.vars.insert(
                format!("__{}", id),
                VarBuilder::new(id, span).init(init).used(true).build(),
            );
        }

        Ok(id)
    }

    fn push_var(&mut self, span: Span, name: String, init: bool) -> Result<usize, CompileError> {
        if self.names.contains(&name) {
            return Err(ErrorKind::DuplicateName(name).at(span));
        }

        let id = self.vars.next();

        if let Some(scope) = self.scope.front_mut() {
            scope
                .vars
                .insert(name, VarBuilder::new(id, span).init(init).build());
        }

        Ok(id)
    }

    fn call(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        callee: Expr,
        args: Vec<Expr>,
    ) -> Result<(), CompileError> {
        let arg_count = args.len();
        self.expr_list(ctx, args)?;

        match callee.kind {
            ExprKind::Ident(name) if name.starts_with('@') => {
                let s = self
                    .consts
                    .insert(Const::Str(name.trim_start_matches('@').to_string()));

                self.push(
                    Bytecode::with_code2(Op::CallBuiltin, s as u16, arg_count as u16).at(span),
                )
            }
            _ => {
                self.expr(ctx, callee)?;

                match self.accept(Op::LoadFn) {
                    Some(bc) => self.push(
                        Bytecode::with_code2(Op::CallFn, bc.code as u16, arg_count as u16)
                            .at(bc.span),
                    ),
                    None => self.push(Bytecode::with_code(Op::Call, arg_count as u32).at(span)),
                }
            }
        };

        Ok(())
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
        ctx: &mut GlobalContext,
        span: Span,
        rhs: Expr,
        cond: bool,
    ) -> Result<(), CompileError> {
        let offset = self.push(match cond {
            true => Bytecode::new(Op::PushJumpIfTrue).at(span),
            false => Bytecode::new(Op::PushJumpIfFalse).at(span),
        });

        self.expr(ctx, rhs)?;
        self.set_offset(offset, self.offset());

        Ok(())
    }

    fn expr_list(&mut self, ctx: &mut GlobalContext, exprs: Vec<Expr>) -> Result<(), CompileError> {
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
        if let Some(local) = self.load_local(name) {
            local.used = true;

            if check_init && !local.init {
                return Err(ErrorKind::NameUninitialized(name.to_string()).at(local.span));
            }

            return Ok(Symbol::Local(local.id as u32));
        }

        if let Some(var) = self.load_var(name) {
            var.used = true;

            if check_init && !var.init {
                return Err(ErrorKind::NameUninitialized(name.to_string()).at(var.span));
            }

            return Ok(Symbol::Var(var.id as u32));
        }

        if let Some(idx) = self.classes.get(name) {
            return Ok(Symbol::Class(idx));
        }

        if let Some(idx) = self.funcs.get(name) {
            return Ok(Symbol::Fn(idx));
        }

        Err(ErrorKind::UnknownName(name.to_string()).at(span))
    }

    fn load_prelude(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        name: &str,
    ) -> Result<bool, CompileError> {
        if !self.imports.contains("std") || !PRELUDE.contains(&name) {
            return Ok(false);
        }

        let Some(std) = self.load_var("std") else {
            return Ok(false);
        };

        std.used = true;

        let idx = std.id as u32;
        self.push(Bytecode::with_code(Op::Load, idx).at(span));

        let member = ctx.atoms.insert(name.to_string());
        self.push(Bytecode::with_code(Op::LoadMember, member).at(span));

        Ok(true)
    }

    fn load_name(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        name: &str,
    ) -> Result<(), CompileError> {
        match self.load_symbol(span, name, true) {
            Ok(symbol) => {
                match symbol {
                    Symbol::Local(id) => self.push(Bytecode::with_code(Op::LoadLocal, id).at(span)),
                    Symbol::Var(id) => self.push(Bytecode::with_code(Op::Load, id).at(span)),
                    Symbol::Class(id) => self.push(Bytecode::with_code(Op::LoadClass, id).at(span)),
                    Symbol::Fn(id) => self.push(Bytecode::with_code(Op::LoadFn, id).at(span)),
                };

                Ok(())
            }
            Err(_) if self.load_prelude(ctx, span, name)? => Ok(()),
            Err(e) => Err(e),
        }
    }

    fn assign(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        op: Option<ast::AssignOp>,
        lhs: Expr,
        mut rhs: Expr,
    ) -> Result<(), CompileError> {
        if let Some(op) = op {
            rhs = ExprKind::Binary(Box::new(lhs.clone()), op.into(), Box::new(rhs)).at(span);
        }

        match lhs.kind {
            ExprKind::Ident(name) => {
                let sym = self.load_symbol(span, &name, false)?;
                let bc = match sym {
                    Symbol::Local(id) => Bytecode::with_code(Op::StoreLocal, id).at(span),
                    Symbol::Var(id) => Bytecode::with_code(Op::Store, id).at(span),
                    _ => return Err(ErrorKind::UnknownName(name).at(span)),
                };

                self.expr(ctx, rhs)?;
                self.set_init(sym);
                self.push(bc);
            }
            ExprKind::Member(object, member) => {
                self.expr(ctx, *object)?;
                self.expr(ctx, rhs)?;
                let idx = ctx.atoms.insert(member);
                self.push(Bytecode::with_code(Op::StoreMember, idx).at(span));
            }
            ExprKind::CompMember(object, index) => {
                self.expr(ctx, *object)?;
                self.expr(ctx, *index)?;
                self.expr(ctx, rhs)?;
                self.push(Bytecode::new(Op::StoreElement).at(span));
            }
            _ => unimplemented!(),
        };

        Ok(())
    }

    fn slice(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        begin: Option<Box<Expr>>,
        end: Option<Box<Expr>>,
    ) -> Result<(), CompileError> {
        let mut code = 0;

        if let Some(begin) = begin {
            code += 1;
            self.expr(ctx, *begin)?;
        }

        if let Some(end) = end {
            code += 2;
            self.expr(ctx, *end)?;
        }

        self.push(Bytecode::with_code(Op::MakeSlice, code).at(span));

        Ok(())
    }

    fn not(&mut self, ctx: &mut GlobalContext, span: Span, expr: Expr) -> Result<(), CompileError> {
        self.expr(ctx, expr)?;
        self.push(Bytecode::new(Op::UnaryNot).at(span));

        Ok(())
    }

    fn binary(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        lhs: Expr,
        op: BinaryOp,
        rhs: Expr,
    ) -> Result<(), CompileError> {
        self.expr(ctx, lhs)?;

        match BINARY_OPS.get(&op).copied() {
            Some(op) => {
                self.expr(ctx, rhs)?;
                self.push(Bytecode::new(op).at(span));
            }
            None => match op {
                ast::BinaryOp::Or => self.logical(ctx, span, rhs, true)?,
                ast::BinaryOp::And => self.logical(ctx, span, rhs, false)?,
                _ => unreachable!(),
            },
        };

        Ok(())
    }

    fn array(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        items: Vec<Expr>,
    ) -> Result<(), CompileError> {
        let len = items.len();
        self.expr_list(ctx, items)?;
        self.push(Bytecode::with_code(Op::MakeArray, len as u32).at(span));

        Ok(())
    }

    fn member(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        object: Expr,
        member: String,
    ) -> Result<(), CompileError> {
        self.expr(ctx, object)?;
        self.push(Bytecode::with_code(Op::LoadMember, ctx.atoms.insert(member)).at(span));

        Ok(())
    }

    fn comp_member(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        object: Expr,
        elem: Expr,
    ) -> Result<(), CompileError> {
        self.expr(ctx, object)?;

        match elem.kind {
            ExprKind::Range(begin, end) => self.slice(ctx, span, begin, end)?,
            kind => {
                self.expr(ctx, kind.at(elem.span))?;
                self.push(Bytecode::new(Op::LoadElement).at(elem.span));
            }
        };

        Ok(())
    }

    fn atom(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        name: String,
    ) -> Result<(), CompileError> {
        self.push(Bytecode::with_code(Op::LoadAtom, ctx.atoms.insert(name)).at(span));
        Ok(())
    }

    fn load_const(&mut self, span: Span, c: Const) -> Result<(), CompileError> {
        let code = self.consts.insert(c);
        self.push(Bytecode::with_code(Op::LoadConst, code).at(span));
        Ok(())
    }

    fn lit(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        lit: Literal,
    ) -> Result<(), CompileError> {
        match lit {
            Literal::Atom(name) => self.atom(ctx, span, name),
            Literal::Int(i) if i > 0 && u32::try_from(i).is_ok() => {
                self.push(Bytecode::with_code(Op::LoadConstInt, i as u32).at(span));
                Ok(())
            }
            Literal::Int(i) => self.load_const(span, Const::Int(i)),
            Literal::BigInt(i) => self.load_const(span, Const::BigInt(i)),
            Literal::Float(f) => self.load_const(span, Const::Float(f)),
            Literal::String(s) => self.load_const(span, Const::Str(s)),
        }
    }

    fn expr(&mut self, ctx: &mut GlobalContext, root: Expr) -> Result<(), CompileError> {
        match root.kind {
            ExprKind::Unary(op, unary_expr) => match op {
                ast::UnaryOp::Not => self.not(ctx, root.span, *unary_expr),
            },
            ExprKind::Assign(op, lhs, rhs) => self.assign(ctx, root.span, op, *lhs, *rhs),
            ExprKind::Binary(lhs, op, rhs) => self.binary(ctx, root.span, *lhs, op, *rhs),
            ExprKind::Array(items) => self.array(ctx, root.span, items),
            ExprKind::Member(object, member) => self.member(ctx, root.span, *object, member),
            ExprKind::CompMember(object, elem) => self.comp_member(ctx, root.span, *object, *elem),
            ExprKind::Ident(name) => self.load_name(ctx, root.span, &name),
            ExprKind::Call(callee, args) => self.call(ctx, root.span, *callee, args),
            ExprKind::Literal(lit) => self.lit(ctx, root.span, lit),
            ExprKind::Match(expr, arms, alt) => self.match_expr(ctx, *expr, arms, alt),
            ExprKind::Range(_, _) => unreachable!(),
        }
    }

    fn match_expr(
        &mut self,
        ctx: &mut GlobalContext,
        expr: Expr,
        arms: Vec<MatchArm>,
        alt: Option<Box<Expr>>,
    ) -> Result<(), CompileError> {
        let span = expr.span;
        let id = self.push_hidden_var(expr.span, true)?;

        self.expr(ctx, expr)?;
        self.push(Bytecode::with_code(Op::Store, id as u32).at(span));

        let mut offsets = vec![];

        for arm in arms {
            let span = arm.pat.span;

            self.push(Bytecode::with_code(Op::Load, id as u32).at(span));
            self.expr(ctx, arm.pat)?;
            self.push(Bytecode::new(Op::Eq).at(span));
            let offset = self.push(Bytecode::new(Op::JumpIfFalse).at(span));
            self.expr(ctx, arm.expr)?;
            offsets.push(self.push(Bytecode::new(Op::Jump).at(span)));
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

    fn optim_tail_call(&mut self) -> Result<(), CompileError> {
        let discard = self.accept(Op::Discard);

        match self.accept(Op::CallFn) {
            Some(bc) => {
                let (idx, argc) = bc.code2();
                self.push(Bytecode::with_code(Op::LoadFn, idx as u32).at(bc.span));
                self.push(Bytecode::with_code(Op::TailCall, argc as u32).at(bc.span));

                Ok(())
            }
            None => {
                if let Some(bc) = discard {
                    self.push(bc);
                }

                Ok(())
            }
        }
    }

    fn compile_fn_body(
        &mut self,
        ctx: &mut GlobalContext,
        scope: Scope,
        args: Vec<FnArg>,
        body: Vec<Stmt>,
    ) -> Result<BytesMut, CompileError> {
        self.scope.push_front(scope);
        self.locals.push_front(
            args.into_iter()
                .enumerate()
                .map(|(i, n)| (n.name, VarBuilder::new(i, n.span).init(true).build()))
                .collect(),
        );

        let global = mem::take(&mut self.body);

        self.compile_body(ctx, body)?;
        self.optim_tail_call()?;
        self.pop_scope()?;

        if let Some(locals) = self.locals.pop_front() {
            check_unused(&locals)?;
        }

        let body = mem::replace(&mut self.body, global);

        Ok(body)
    }

    fn method(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        methods: &mut HashMap<String, Fn, WyHash>,
        fn_stmt: FnStmt,
    ) -> Result<(), CompileError> {
        if methods.contains_key(fn_stmt.name.as_str()) {
            return Err(ErrorKind::DuplicateMethod(fn_stmt.name).at(span));
        }

        methods.insert(
            fn_stmt.name.clone(),
            Fn::builder()
                .name(fn_stmt.name.clone())
                .arg_count(fn_stmt.args.len() as u32)
                .public(fn_stmt.public)
                .build(),
        );

        let mut body = self.compile_fn_body(ctx, Scope::default(), fn_stmt.args, fn_stmt.body)?;

        if fn_stmt.name == "init" {
            Bytecode::with_code(Op::ReturnLocal, 0).serialize(&mut body);
        }

        if let Some(method) = methods.get_mut(&fn_stmt.name) {
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

    fn fn_stmt(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        fn_stmt: FnStmt,
    ) -> Result<(), CompileError> {
        if self.funcs.contains_key(&fn_stmt.name) {
            return Err(ErrorKind::DuplicateFn(fn_stmt.name).at(span));
        }

        self.funcs.insert(
            fn_stmt.name.clone(),
            Fn::builder()
                .name(fn_stmt.name.clone())
                .public(fn_stmt.public)
                .resumable(fn_stmt.resumable)
                .arg_count(fn_stmt.args.len() as u32)
                .build(),
        );

        self.funcs[&fn_stmt.name].body = self
            .compile_fn_body(ctx, Scope::default(), fn_stmt.args, fn_stmt.body)?
            .freeze();

        Ok(())
    }

    fn class_stmt(
        &mut self,
        ctx: &mut GlobalContext,
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
                StmtKind::Fn(fn_stmt) => self.method(ctx, span, &mut funcs, fn_stmt)?,
                _ => unreachable!(),
            };
        }

        self.classes[&name].methods = funcs
            .into_iter()
            .map(|(method, func)| (Cow::Owned(method), func))
            .collect();

        Ok(())
    }

    fn loop_head(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        expr: Expr,
        body: Vec<Stmt>,
    ) -> Result<usize, CompileError> {
        self.expr(ctx, expr)?;
        let offset = self.push(Bytecode::new(Op::JumpIfFalse).at(span));
        self.compile_scoped_body(ctx, body)?;

        Ok(offset)
    }

    fn loop_tail(
        &mut self,
        span: Span,
        offset: usize,
        begin: usize,
        begin_marker: usize,
    ) -> Result<(), CompileError> {
        self.push(Bytecode::with_code(Op::Jump, (begin / bytecode::SIZE) as u32).at(span));
        self.set_offset(offset, self.offset());
        self.handle_markers(begin_marker, self.offset());

        Ok(())
    }

    fn for_stmt(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        expr: Expr,
        body: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        let begin = self.offset();
        let offset = self.loop_head(ctx, span, expr, body)?;
        self.loop_tail(span, offset, begin, begin)?;

        Ok(())
    }

    fn for_in(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        lhs: Expr,
        rhs: Expr,
        body: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        let item_id = match lhs.kind {
            ExprKind::Ident(name) => self.push_var(span, name, true)? as u32,
            _ => unreachable!(),
        };

        let resumable_id = self.push_hidden_var(span, true)? as u32;
        self.expr(ctx, rhs)?;
        self.push(Bytecode::with_code(Op::Store, resumable_id).at(span));

        let begin = self.offset();
        self.push(Bytecode::with_code(Op::Load, resumable_id).at(span));
        self.push(Bytecode::with_code(Op::Call, 0).at(span));
        self.push(Bytecode::with_code(Op::Store, item_id).at(span));
        self.push(Bytecode::with_code(Op::Load, item_id).at(span));
        self.push(Bytecode::with_code(Op::LoadAtom, value::NIL).at(span));
        self.push(Bytecode::new(Op::Ne).at(span));
        let offset = self.push(Bytecode::new(Op::JumpIfFalse).at(span));

        self.compile_scoped_body(ctx, body)?;
        self.loop_tail(span, offset, begin, begin)?;

        Ok(())
    }

    fn for_cond_stmt(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        pre: Stmt,
        expr: Expr,
        post: Expr,
        body: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        self.stmt(ctx, pre)?;
        let begin = self.offset();
        let offset = self.loop_head(ctx, span, expr, body)?;
        let post_idx = self.offset();
        self.expr(ctx, post)?;
        self.loop_tail(span, offset, begin, post_idx)?;

        Ok(())
    }

    fn yield_stmt(
        &mut self,
        ctx: &mut GlobalContext,
        span: Span,
        expr: Expr,
    ) -> Result<(), CompileError> {
        self.expr(ctx, expr)?;
        self.push(Bytecode::new(Op::Yield).at(span));

        Ok(())
    }

    fn ret(&mut self, ctx: &mut GlobalContext, span: Span, expr: Expr) -> Result<(), CompileError> {
        self.expr(ctx, expr)?;
        self.optim_tail_call()?;

        match self.accept(Op::LoadLocal) {
            Some(bc) => self.push(Bytecode::with_code(Op::ReturnLocal, bc.code).at(bc.span)),
            _ => self.push(Bytecode::new(Op::Return).at(span)),
        };

        Ok(())
    }

    fn if_stmt(
        &mut self,
        ctx: &mut GlobalContext,
        IfStmt(expr, stmts, alt): IfStmt,
    ) -> Result<(), CompileError> {
        let span = expr.as_ref().map(|e| e.span).unwrap_or_default();
        let end_block_offset = if let Some(expr) = expr {
            self.expr(ctx, expr)?;
            Some(self.push(Bytecode::new(Op::JumpIfFalse).at(span)))
        } else {
            None
        };

        self.compile_scoped_body(ctx, stmts)?;

        match alt {
            Some(alt) => {
                let end_stmt_offset = self.push(Bytecode::new(Op::Jump).at(span));

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

    fn import(&mut self, span: Span, path: Vec<String>) -> Result<(), CompileError> {
        let name = path.join("/");

        if self.imports.contains(&name) {
            return Err(ErrorKind::DuplicateImport(name).at(span));
        }

        self.imports.insert(name.clone());

        let idx = self.consts.insert(Const::Str(name.clone()));
        self.push(Bytecode::with_code(Op::Import, idx).at(span));

        let var_name = path.last().cloned().unwrap_or_default();

        let idx = self.push_var(span, var_name.clone(), true)?;
        self.push(Bytecode::with_code(Op::Store, idx as u32).at(span));

        self.names.insert(var_name);

        Ok(())
    }

    fn stmt(&mut self, ctx: &mut GlobalContext, stmt: Stmt) -> Result<(), CompileError> {
        match stmt.kind {
            StmtKind::Import(path) => self.import(stmt.span, path)?,
            StmtKind::If(if_stmt) => self.if_stmt(ctx, if_stmt)?,
            StmtKind::Let(name, expr) => {
                if let Some(expr) = expr {
                    let idx = self.push_var(expr.span, name, true)?;
                    self.expr(ctx, expr)?;
                    self.push(Bytecode::with_code(Op::Store, idx as u32).at(stmt.span));
                } else {
                    self.push_var(stmt.span, name, false)?;
                }
            }
            StmtKind::Expr(expr) => {
                let assignment = expr.is_assign();
                self.expr(ctx, expr)?;

                if !assignment {
                    self.push(Bytecode::new(Op::Discard).at(stmt.span));
                }
            }
            StmtKind::Yield(expr) => self.yield_stmt(ctx, stmt.span, expr)?,
            StmtKind::Return(expr) => self.ret(ctx, stmt.span, expr)?,
            StmtKind::Break => {
                let offset = self.push(Bytecode::new(Op::Jump).at(stmt.span));
                self.markers.push(Marker::End(offset));
            }
            StmtKind::Continue => {
                let offset = self.push(Bytecode::new(Op::Jump).at(stmt.span));
                self.markers.push(Marker::Begin(offset));
            }
            StmtKind::Class(name, methods, public) => {
                self.class_stmt(ctx, stmt.span, name, methods, public)?
            }
            StmtKind::Fn(fn_stmt) => self.fn_stmt(ctx, stmt.span, fn_stmt)?,
            StmtKind::ForIn(lhs, rhs, body) => self.for_in(ctx, stmt.span, lhs, rhs, body)?,
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

    fn compile_body(
        &mut self,
        ctx: &mut GlobalContext,
        stmts: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        for stmt in stmts {
            self.stmt(ctx, stmt)?;
        }

        Ok(())
    }

    fn compile_scoped_body(
        &mut self,
        ctx: &mut GlobalContext,
        stmts: Vec<Stmt>,
    ) -> Result<(), CompileError> {
        self.scope.push_front(Scope::default());
        self.compile_body(ctx, stmts)?;
        self.pop_scope()?;

        Ok(())
    }

    pub fn compile(
        mut self,
        ctx: &mut GlobalContext,
        stmts: Vec<Stmt>,
    ) -> Result<Package, CompileError> {
        // In the root scope, `self` refers to the current package
        self.push_var(Span::default(), "self".to_string(), true)?;
        self.compile_scoped_body(ctx, stmts)?;
        self.pop_scope()?;

        Ok(Package {
            body: self.body.freeze(),
            offsets: self.offsets,
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
        let offset = compiler.push(bc.clone());

        assert_eq!(0, offset);
        assert_eq!(bytecode::SIZE, compiler.body.len());
        assert_eq!(Some(bc), compiler.accept(Op::Load));
        assert_eq!(0, compiler.body.len());

        let bc = Bytecode::new(Op::Lt).at(Span::default());
        let offset = compiler.push(bc.clone());

        assert_eq!(0, offset);
        assert_eq!(None, compiler.accept(Op::Gt));
        assert_eq!(bytecode::SIZE, compiler.body.len());
    }

    #[test]
    fn test_replace_code() {
        let mut compiler = Compiler::default();
        let code = Bytecode::new(Op::Load).at(Span::default());
        let offset = compiler.push(code.clone());

        compiler.push(Bytecode::new(Op::Lt).at(Span::default()));
        compiler.set_offset(offset, 500);

        let code = Bytecode::deserialize(&mut &compiler.body[..bytecode::SIZE]);
        assert_eq!(Op::Load, code.op);
        assert_eq!(100, code.code);
    }

    #[test]
    fn test_scope() {
        let mut compiler = Compiler::default();
        let top = compiler
            .push_var(Span::default(), "n".to_string(), true)
            .unwrap();
        compiler.scope.push_front(Scope::default());

        let expected = compiler
            .push_var(Span::default(), "n".to_string(), true)
            .unwrap();
        let actual = compiler.load_var("n").unwrap();

        assert_eq!(expected, actual.id);
        assert_ne!(top, actual.id);
    }

    #[test]
    fn test_assign() {
        let mut ctx = GlobalContext::default();
        let mut compiler = Compiler::default();
        let ident = |name: &str| ExprKind::Ident(name.to_string()).at(Span::default());

        let expr = ExprKind::Literal(Literal::Int(100)).at(Span::default());
        let idx = compiler
            .push_var(Span::default(), "n".to_string(), true)
            .unwrap();
        compiler
            .assign(&mut ctx, Span::default(), None, ident("n"), expr.clone())
            .unwrap();
        let code = compiler.accept(Op::Store).unwrap();

        assert_eq!(Op::Store, code.op);
        assert_eq!(idx, code.code as usize);

        let result = compiler.assign(&mut ctx, Span::default(), None, ident("y"), expr);
        assert!(result.is_err());
    }
}
