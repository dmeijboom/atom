use std::{
    collections::{HashMap, VecDeque},
    hash::{Hash, Hasher},
    ops::*,
};

use serde::Serialize;

use crate::{
    backend::GlobalContext,
    collections::OrderedMap,
    error::{IntoSpanned, SpannedError},
    frontend::ast::StmtKind,
    runtime::{consts, ops, BigInt},
};

use super::{
    ast::{
        AssignOp, BinaryOp, Expr, ExprKind, FnArg, FnStmt, IfStmt, Literal, MatchArm, Path, Stmt,
        UnaryOp,
    },
    Span,
};

const PRELUDE: [&str; 12] = [
    "enum", "chunks", "each", "range", "repeat", "println", "Array", "Blob", "Str", "Int", "Float",
    "BigInt",
];

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("unknown name '{0}'")]
    UnknownName(String),
    #[error("invalid/missing scope")]
    InvalidScope,
    // @TODO: re-implement this
    //
    // #[error("name '{0}' is already defined")]
    // DuplicateName(String),
    #[error("package '{0}' already imported")]
    DuplicateImport(String),
    #[error("fn '{0}' already exists")]
    DuplicateFn(String),
    #[error("method '{0}' already exists")]
    DuplicateMethod(String),
    #[error("class '{0}' already exists")]
    DuplicateClass(String),
    #[error("method '{0}' is missing 'self' as the first argument")]
    MethodMissingSelf(String),
    #[error("name '{0}' is not initialized")]
    NameUninitialized(String),
    #[error("name '{0}' is not used")]
    NameUnused(String),
}

pub type IRError = SpannedError<ErrorKind>;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum IRValue {
    Int(i64),
    BigInt(BigInt),
    Float(f64),
    String(String),
    Atom(u32),
}

impl From<bool> for IRValue {
    fn from(value: bool) -> Self {
        IRValue::Atom(if value { consts::TRUE } else { consts::FALSE })
    }
}

macro_rules! impl_from {
    ($($name:ident: $ty:ty),+) => {
        $(
            impl From<$ty> for IRValue {
                fn from(value: $ty) -> Self {
                    IRValue::$name(value)
                }
            }
        )+
    };
}

impl_from!(
    Int: i64,
    BigInt: BigInt,
    Float: f64,
    String: String
);

macro_rules! impl_op {
    ($name:ident) => {
        #[allow(unused)]
        pub fn $name(&self, other: &IRValue) -> IRValue {
            match (self, other) {
                (IRValue::Int(lhs), IRValue::Int(rhs)) => lhs.$name(rhs).into(),
                (IRValue::BigInt(lhs), IRValue::BigInt(rhs)) => lhs.$name(rhs).into(),
                (IRValue::Float(lhs), IRValue::Float(rhs)) => ops::float_op!(lhs, rhs, $name, { unreachable!() }),
                (IRValue::String(lhs), IRValue::String(rhs)) => ops::string_op!(lhs, rhs, $name, { unreachable!() }),
                (IRValue::Atom(lhs), IRValue::Atom(rhs)) => ops::atom_op!(lhs, rhs, $name, { unreachable!() }),
                _ => unreachable!(),
            }
        }
    };

    ($($name:ident),+) => {
        impl IRValue {
            $(impl_op!($name);)+
        }
    };
}

impl_op!(lt, le, gt, ge, eq, ne, add, sub, mul, div, rem, bitand, bitor, bitxor, shl, shr);

impl Hash for IRValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            IRValue::Int(i) => {
                state.write_u8(0);
                state.write_i64(*i);
            }
            IRValue::BigInt(big_int) => {
                state.write_u8(1);
                big_int.hash(state);
            }
            IRValue::Float(f) => {
                state.write_u8(2);
                state.write_u64(f.to_bits());
            }
            IRValue::String(s) => {
                state.write_u8(3);
                s.hash(state);
            }
            IRValue::Atom(a) => {
                state.write_u8(4);
                a.hash(state);
            }
        }
    }
}

#[derive(Debug, Default, Serialize)]
pub struct Block {
    pub children: Vec<IRNode>,
}

impl Block {
    pub fn merge(&mut self, other: &mut Block) {
        self.children.append(&mut other.children);
    }
}

impl Deref for Block {
    type Target = Vec<IRNode>;

    fn deref(&self) -> &Self::Target {
        &self.children
    }
}

impl DerefMut for Block {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.children
    }
}

#[derive(Debug, Serialize)]
pub struct IRNode {
    pub kind: NodeKind,
    pub span: Span,
}

impl IRNode {
    pub fn new(span: Span, kind: NodeKind) -> Self {
        Self { kind, span }
    }

    fn is_noop(&self) -> bool {
        matches!(self.kind, NodeKind::Noop)
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum VariableKind {
    Local,
    Global,
}

#[derive(Debug, Serialize)]
pub struct Loop {
    pub id: usize,
    pub pre: Option<IRNode>,
    pub cond: IRNode,
    pub post: Option<IRNode>,
    pub body: Block,
}

#[derive(Debug, Serialize)]
pub enum NodeKind {
    Noop,
    FnRef(u32),
    ClassRef(u32),
    ImportRef(u32),
    BuiltinRef(u32),
    Break(usize),
    Continue(usize),
    Const(IRValue),
    Loop(Box<Loop>),
    Array(Vec<IRNode>),
    Return(Box<IRNode>),
    Yield(Box<IRNode>),
    Discard(Box<IRNode>),
    Compound(Block, Box<IRNode>),
    Unary(UnaryOp, Box<IRNode>),
    Member(Box<IRNode>, String),
    Call(Box<IRNode>, Vec<IRNode>),
    Store(Box<IRNode>, Box<IRNode>),
    CompMember(Box<IRNode>, Box<IRNode>),
    Condition(Vec<(IRNode, Block)>, Option<Block>),
    Range(Option<Box<IRNode>>, Option<Box<IRNode>>),
    Binary(Box<IRNode>, BinaryOp, Box<IRNode>),
    LoadVariable(VariableKind, usize),
    StoreVariable(VariableKind, usize, Box<IRNode>),
}

impl From<(Span, IRValue)> for IRNode {
    fn from((span, value): (Span, IRValue)) -> Self {
        IRNode::new(span, NodeKind::Const(value))
    }
}

impl From<(Span, IRValue)> for Box<IRNode> {
    fn from((span, value): (Span, IRValue)) -> Self {
        Box::new((span, value).into())
    }
}

struct Variable {
    id: usize,
    kind: VariableKind,
    used: bool,
    initialized: bool,
    name: String,
    value: Option<IRValue>,
}

impl Variable {
    fn new(kind: VariableKind, id: usize, name: String) -> Self {
        Self {
            id,
            kind,
            used: false,
            initialized: false,
            name,
            value: None,
        }
    }
}

#[must_use]
struct VariableBuilder<'a> {
    inner: Variable,
    scope: &'a mut Scope,
}

impl<'a> VariableBuilder<'a> {
    fn new(scope: &'a mut Scope, inner: Variable) -> Self {
        Self { scope, inner }
    }

    fn used(mut self) -> Self {
        self.inner.used = true;
        self
    }

    fn value(mut self, value: IRValue) -> Self {
        self.inner.value = Some(value);
        self
    }

    fn initialized(mut self) -> Self {
        self.inner.initialized = true;
        self
    }

    fn finish(self) -> usize {
        let id = self.inner.id;
        self.scope.vars.push(self.inner);
        id
    }
}

#[derive(Default)]
struct Scope {
    vars: Vec<Variable>,
}

impl Scope {
    fn declare(&mut self, kind: VariableKind, id: usize, name: String) -> VariableBuilder<'_> {
        VariableBuilder::new(self, Variable::new(kind, id, name))
    }

    fn declare_var(&mut self, id: usize, name: String) -> VariableBuilder<'_> {
        self.declare(VariableKind::Global, id, name)
    }

    fn declare_local(&mut self, id: usize, name: String) -> VariableBuilder<'_> {
        self.declare(VariableKind::Local, id, name)
    }
}

#[derive(Default, Serialize)]
pub struct IRFn {
    pub name: String,
    pub arg_count: usize,
    pub body: Block,
    pub public: bool,
    pub resumable: bool,
}

#[derive(Default, Serialize)]
pub struct IRClass {
    pub name: String,
    pub public: bool,
    pub methods: HashMap<String, IRFn>,
}

fn is_const_compat(lhs: &IRValue, op: BinaryOp, rhs: &IRValue) -> bool {
    if matches!(op, BinaryOp::Or | BinaryOp::And) {
        return false;
    }

    match (lhs, rhs) {
        (IRValue::Int(_), IRValue::Int(_)) => true,
        (IRValue::BigInt(_), IRValue::BigInt(_)) => true,
        (IRValue::Float(_), IRValue::Float(_)) => true,
        (IRValue::String(_), IRValue::String(_)) => matches!(op, BinaryOp::Eq | BinaryOp::Ne),
        (IRValue::Atom(_), IRValue::Atom(_)) => matches!(op, BinaryOp::Eq | BinaryOp::Ne),
        _ => false,
    }
}

#[derive(Serialize)]
pub struct Program {
    pub body: Block,
    pub imports: Vec<Path>,
    pub funcs: Vec<IRFn>,
    pub classes: Vec<IRClass>,
}

fn ident(expr: Expr) -> String {
    match expr.kind {
        ExprKind::Ident(name) => name,
        _ => unreachable!(),
    }
}

pub struct IR<'a> {
    vars: usize,
    loops: usize,
    imports: OrderedMap<String, Path>,
    funcs: OrderedMap<String, IRFn>,
    classes: OrderedMap<String, IRClass>,
    scope: VecDeque<Scope>,
    ctx: &'a mut GlobalContext,
}

impl<'a> IR<'a> {
    pub fn new(ctx: &'a mut GlobalContext) -> Self {
        Self {
            vars: 0,
            loops: 0,
            imports: OrderedMap::default(),
            funcs: OrderedMap::default(),
            classes: OrderedMap::default(),
            scope: VecDeque::new(),
            ctx,
        }
    }

    fn next_var(&mut self) -> usize {
        let id = self.vars;
        self.vars += 1;
        id
    }

    fn next_loop(&mut self) -> usize {
        let id = self.loops;
        self.loops += 1;
        id
    }

    fn register(&mut self, stmts: &[Stmt]) -> Result<(), IRError> {
        for stmt in stmts {
            match &stmt.kind {
                StmtKind::Fn(fn_stmt) => {
                    if self.funcs.contains_key(fn_stmt.name.as_str()) {
                        return Err(ErrorKind::DuplicateFn(fn_stmt.name.clone()).at(stmt.span));
                    }

                    self.funcs.insert(fn_stmt.name.clone(), IRFn::default());
                }
                StmtKind::Class(name, _, _) => {
                    if self.classes.contains_key(name.as_str()) {
                        return Err(ErrorKind::DuplicateClass(name.clone()).at(stmt.span));
                    }

                    self.classes.insert(name.clone(), IRClass::default());
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn lookup_var(&mut self, name: &str) -> Option<&mut Variable> {
        self.scope
            .iter_mut()
            .find_map(|scope| scope.vars.iter_mut().rev().find(|var| var.name == name))
    }

    fn declare_var(&mut self, span: Span, name: String) -> Result<VariableBuilder<'_>, IRError> {
        let id = self.next_var();
        let scope = self
            .scope
            .front_mut()
            .ok_or(ErrorKind::InvalidScope.at(span))?;

        Ok(scope.declare_var(id, name))
    }

    fn import(&mut self, span: Span, path: Path) -> Result<(), IRError> {
        let name = path.last().map(ToOwned::to_owned).unwrap_or_default();

        if self.imports.contains_key(&name) {
            return Err(ErrorKind::DuplicateImport(name).at(span));
        }

        self.imports.insert(name, path);

        Ok(())
    }

    fn lit(&mut self, span: Span, literal: Literal) -> Result<IRNode, IRError> {
        Ok(IRNode::new(
            span,
            NodeKind::Const(match literal {
                Literal::Int(i) => IRValue::Int(i),
                Literal::BigInt(i) => IRValue::BigInt(i),
                Literal::Float(f) => IRValue::Float(f),
                Literal::Atom(a) => IRValue::Atom(self.ctx.atoms.insert(a)),
                Literal::String(s) => IRValue::String(s),
            }),
        ))
    }

    fn binary(
        &mut self,
        span: Span,
        lhs: Expr,
        op: BinaryOp,
        rhs: Expr,
    ) -> Result<IRNode, IRError> {
        let lhs = self.expr(lhs)?;
        let rhs = self.expr(rhs)?;

        match (lhs.span, lhs.kind, rhs.span, rhs.kind) {
            (_, NodeKind::Const(lhs), _, NodeKind::Const(rhs))
                if is_const_compat(&lhs, op, &rhs) =>
            {
                let value = match op {
                    BinaryOp::Add => lhs.add(&rhs),
                    BinaryOp::Sub => lhs.sub(&rhs),
                    BinaryOp::Mul => lhs.mul(&rhs),
                    BinaryOp::Rem => lhs.rem(&rhs),
                    BinaryOp::Div => lhs.div(&rhs),
                    BinaryOp::Eq => lhs.eq(&rhs),
                    BinaryOp::Ne => lhs.ne(&rhs),
                    BinaryOp::Gt => lhs.gt(&rhs),
                    BinaryOp::Gte => lhs.ge(&rhs),
                    BinaryOp::Lt => lhs.lt(&rhs),
                    BinaryOp::Lte => lhs.le(&rhs),
                    BinaryOp::BitOr => lhs.bitor(&rhs),
                    BinaryOp::BitAnd => lhs.bitand(&rhs),
                    BinaryOp::ShiftLeft => lhs.shl(&rhs),
                    BinaryOp::ShiftRight => lhs.shr(&rhs),
                    BinaryOp::Xor => lhs.bitxor(&rhs),
                    _ => unreachable!(),
                };

                Ok(IRNode::new(span, NodeKind::Const(value)))
            }
            (lspan, lval, rspan, rval) => Ok(IRNode::new(
                span,
                NodeKind::Binary(
                    Box::new(IRNode::new(lspan, lval)),
                    op,
                    Box::new(IRNode::new(rspan, rval)),
                ),
            )),
        }
    }

    fn prelude(&mut self, span: Span, name: String) -> Result<IRNode, IRError> {
        let std_name = "std".to_string();

        if !self.imports.contains_key(&std_name) {
            self.import(span, std_name.clone().into())?;
        }

        self.member(span, ExprKind::Ident(std_name).at(span), name)
    }

    // Loads a symbol based on the following order: builtin > local/var > class > fn > import > prelude
    fn ident(&mut self, span: Span, mut name: String) -> Result<IRNode, IRError> {
        // Builtins
        if name.starts_with('@') {
            name.remove(0);
            let id = self.ctx.atoms.insert(name);

            return Ok(IRNode::new(span, NodeKind::BuiltinRef(id)));
        }

        // Locals / variables
        if let Some(var) = self.lookup_var(&name) {
            if !var.initialized {
                return Err(ErrorKind::NameUninitialized(name.clone()).at(span));
            }

            var.used = true;

            if let Some(value) = var.value.clone() {
                return Ok(IRNode::new(span, NodeKind::Const(value)));
            }

            return Ok(IRNode::new(span, NodeKind::LoadVariable(var.kind, var.id)));
        }

        // Classes
        if let Some(id) = self.classes.get(&name) {
            return Ok(IRNode::new(span, NodeKind::ClassRef(id)));
        }

        // Functions
        if let Some(id) = self.funcs.get(&name) {
            return Ok(IRNode::new(span, NodeKind::FnRef(id)));
        }

        // Imports
        if let Some(id) = self.imports.get(&name) {
            return Ok(IRNode::new(span, NodeKind::ImportRef(id)));
        }

        // Prelude
        if PRELUDE.contains(&name.as_str()) {
            return self.prelude(span, name);
        }

        Err(ErrorKind::UnknownName(name).at(span))
    }

    fn comp_member(&mut self, span: Span, lhs: Expr, rhs: Expr) -> Result<IRNode, IRError> {
        let lhs = self.expr(lhs)?;
        let rhs = match rhs.kind {
            ExprKind::Range(begin, end) => IRNode::new(
                span,
                NodeKind::Range(
                    begin.map(|b| self.expr(*b)).transpose()?.map(Box::new),
                    end.map(|e| self.expr(*e)).transpose()?.map(Box::new),
                ),
            ),
            _ => self.expr(rhs)?,
        };

        Ok(IRNode::new(
            span,
            NodeKind::CompMember(Box::new(lhs), Box::new(rhs)),
        ))
    }

    fn member(&mut self, span: Span, expr: Expr, name: String) -> Result<IRNode, IRError> {
        Ok(IRNode::new(
            span,
            NodeKind::Member(Box::new(self.expr(expr)?), name),
        ))
    }

    fn call(&mut self, span: Span, expr: Expr, args: Vec<Expr>) -> Result<IRNode, IRError> {
        let callee = self.expr(expr)?;
        let args = self.expr_list(args)?;
        Ok(IRNode::new(span, NodeKind::Call(Box::new(callee), args)))
    }

    fn expr_list(&mut self, exprs: Vec<Expr>) -> Result<Vec<IRNode>, IRError> {
        exprs
            .into_iter()
            .map(|expr| self.expr(expr))
            .collect::<Result<Vec<_>, _>>()
    }

    fn array(&mut self, span: Span, items: Vec<Expr>) -> Result<IRNode, IRError> {
        Ok(IRNode::new(span, NodeKind::Array(self.expr_list(items)?)))
    }

    fn assign_name(&mut self, span: Span, name: String, value: IRNode) -> Result<IRNode, IRError> {
        if let Some(var) = self.lookup_var(&name) {
            match value.kind {
                NodeKind::Const(value) if !var.initialized => {
                    var.initialized = true;
                    var.value = Some(value);

                    return Ok(IRNode::new(span, NodeKind::Noop));
                }
                _ => {
                    // De-optimize variable if it was previously a constant
                    var.value = None;
                    var.initialized = true;

                    return Ok(IRNode::new(
                        span,
                        NodeKind::StoreVariable(var.kind, var.id, Box::new(value)),
                    ));
                }
            }
        }

        Err(ErrorKind::UnknownName(name).at(span))
    }

    fn assign_member(
        &mut self,
        span: Span,
        object: Expr,
        member: String,
        value: IRNode,
    ) -> Result<IRNode, IRError> {
        let object = self.expr(object)?;
        let member = IRNode::new(span, NodeKind::Member(Box::new(object), member));

        Ok(IRNode::new(
            span,
            NodeKind::Store(Box::new(member), Box::new(value)),
        ))
    }

    fn assign_comp_member(
        &mut self,
        span: Span,
        object: Expr,
        index: Expr,
        value: IRNode,
    ) -> Result<IRNode, IRError> {
        let object = self.expr(object)?;
        let index = self.expr(index)?;
        let member = IRNode::new(
            span,
            NodeKind::CompMember(Box::new(object), Box::new(index)),
        );

        Ok(IRNode::new(
            span,
            NodeKind::Store(Box::new(member), Box::new(value)),
        ))
    }

    fn assign(
        &mut self,
        span: Span,
        op: Option<AssignOp>,
        lhs: Expr,
        mut rhs: Expr,
    ) -> Result<IRNode, IRError> {
        // @TODO: refactor
        if let Some(op) = op {
            rhs = ExprKind::Binary(Box::new(lhs.clone()), op.into(), Box::new(rhs)).at(span);
        }

        let value = self.expr(rhs)?;

        match lhs.kind {
            ExprKind::Ident(name) => self.assign_name(span, name, value),
            ExprKind::Member(object, member) => self.assign_member(span, *object, member, value),
            ExprKind::CompMember(object, index) => {
                self.assign_comp_member(span, *object, *index, value)
            }
            _ => unreachable!(),
        }
    }

    fn match_expr(
        &mut self,
        span: Span,
        expr: Expr,
        arms: Vec<MatchArm>,
        alt: Option<Expr>,
    ) -> Result<IRNode, IRError> {
        let id = self.next_var();
        let node = self.expr(expr)?;
        let block = Block {
            children: vec![IRNode::new(
                span,
                NodeKind::StoreVariable(VariableKind::Global, id, Box::new(node)),
            )],
        };

        let mut cases = vec![];

        for arm in arms {
            let cond = IRNode::new(
                span,
                NodeKind::Binary(
                    Box::new(IRNode::new(
                        span,
                        NodeKind::LoadVariable(VariableKind::Global, id),
                    )),
                    BinaryOp::Eq,
                    Box::new(self.expr(arm.pat)?),
                ),
            );

            let body = self.scoped_expr(span, Scope::default(), arm.expr)?;

            cases.push((
                cond,
                Block {
                    children: vec![body],
                },
            ));
        }

        let node = match alt {
            Some(expr) => {
                let block = Block {
                    children: vec![self.scoped_expr(span, Scope::default(), expr)?],
                };

                IRNode::new(span, NodeKind::Condition(cases, Some(block)))
            }
            None => IRNode::new(span, NodeKind::Condition(cases, None)),
        };

        Ok(IRNode::new(span, NodeKind::Compound(block, Box::new(node))))
    }

    fn unary(&mut self, op: UnaryOp, expr: Expr) -> Result<IRNode, IRError> {
        Ok(IRNode::new(
            expr.span,
            NodeKind::Unary(op, Box::new(self.expr(expr)?)),
        ))
    }

    fn expr(&mut self, expr: Expr) -> Result<IRNode, IRError> {
        match expr.kind {
            ExprKind::Ident(name) => self.ident(expr.span, name),
            ExprKind::Array(items) => self.array(expr.span, items),
            ExprKind::Literal(literal) => self.lit(expr.span, literal),
            ExprKind::Unary(op, expr) => self.unary(op, *expr),
            ExprKind::Member(expr, name) => self.member(expr.span, *expr, name),
            ExprKind::Call(expr, args) => self.call(expr.span, *expr, args),
            ExprKind::CompMember(lhs, rhs) => self.comp_member(lhs.span, *lhs, *rhs),
            ExprKind::Binary(lhs, binary_op, rhs) => self.binary(expr.span, *lhs, binary_op, *rhs),
            ExprKind::Assign(assign_op, lhs, rhs) => self.assign(expr.span, assign_op, *lhs, *rhs),
            ExprKind::Match(expr, arms, alt) => {
                self.match_expr(expr.span, *expr, arms, alt.map(|e| *e))
            }
            ExprKind::Range(_, _) => unreachable!(),
        }
    }

    fn let_with_value(
        &mut self,
        block: &mut Block,
        span: Span,
        name: String,
        value: IRNode,
    ) -> Result<(), IRError> {
        block.push(IRNode::new(
            span,
            NodeKind::StoreVariable(
                VariableKind::Global,
                self.declare_var(span, name)?.initialized().finish(),
                Box::new(value),
            ),
        ));

        Ok(())
    }

    fn let_stmt(
        &mut self,
        block: &mut Block,
        span: Span,
        name: String,
        expr: Option<Expr>,
    ) -> Result<(), IRError> {
        if let Some(expr) = expr {
            let node = self.expr(expr)?;

            if let NodeKind::Const(value) = node.kind {
                self.declare_var(span, name)?
                    .initialized()
                    .value(value)
                    .finish();

                return Ok(());
            }

            self.let_with_value(block, span, name, node)
        } else {
            self.declare_var(span, name)?.initialized().finish();
            Ok(())
        }
    }

    fn expr_stmt(&mut self, block: &mut Block, span: Span, expr: Expr) -> Result<(), IRError> {
        let node = self.expr(expr)?;

        if node.is_noop() {
            return Ok(());
        }

        match &node.kind {
            // Assignments are not actually expressions, so we don't discard them
            NodeKind::Store(_, _) | NodeKind::StoreVariable(_, _, _) => block.push(node),
            _ => block.push(IRNode::new(span, NodeKind::Discard(Box::new(node)))),
        }

        Ok(())
    }

    fn class(&mut self, name: String, body: Vec<Stmt>, public: bool) -> Result<(), IRError> {
        let mut methods = HashMap::default();

        for stmt in body {
            if let StmtKind::Fn(fn_stmt) = stmt.kind {
                if methods.contains_key(&fn_stmt.name) {
                    return Err(ErrorKind::DuplicateMethod(fn_stmt.name).at(stmt.span));
                }

                let name = fn_stmt.name.clone();
                methods.insert(name, self.method(stmt.span, fn_stmt)?);
            }
        }

        self.classes.insert(
            name.clone(),
            IRClass {
                name,
                methods,
                public,
            },
        );

        Ok(())
    }

    fn fn_scoped_block(
        &mut self,
        span: Span,
        method: bool,
        args: Vec<FnArg>,
        body: Vec<Stmt>,
    ) -> Result<Block, IRError> {
        let mut scope = Scope::default();

        for (i, arg) in args.into_iter().enumerate() {
            let mut builder = scope.declare_local(i, arg.name).initialized();

            if method && i == 0 {
                // The first argument is `self` in methods
                builder = builder.used();
            }

            builder.finish();
        }

        self.scoped_block(span, scope, body)
    }

    fn method(&mut self, span: Span, fn_stmt: FnStmt) -> Result<IRFn, IRError> {
        if fn_stmt.args.first().map(|arg| arg.name.as_str()) != Some("self") {
            return Err(ErrorKind::MethodMissingSelf(fn_stmt.name.clone()).at(span));
        }

        let arg_count = fn_stmt.args.len();
        let mut body = self.fn_scoped_block(span, true, fn_stmt.args, fn_stmt.body)?;

        // Methods always return `self`
        body.merge(&mut Block {
            children: vec![IRNode::new(
                span,
                NodeKind::Return(Box::new(IRNode::new(
                    span,
                    NodeKind::LoadVariable(VariableKind::Local, 0),
                ))),
            )],
        });

        Ok(IRFn {
            name: fn_stmt.name,
            body,
            arg_count,
            public: fn_stmt.public,
            resumable: false,
        })
    }

    fn fn_stmt(&mut self, span: Span, fn_stmt: FnStmt) -> Result<(), IRError> {
        let arg_count = fn_stmt.args.len();
        let body = self.fn_scoped_block(span, false, fn_stmt.args, fn_stmt.body)?;

        self.funcs.insert(
            fn_stmt.name.clone(),
            IRFn {
                name: fn_stmt.name,
                arg_count,
                body,
                public: fn_stmt.public,
                resumable: fn_stmt.resumable,
            },
        );

        Ok(())
    }

    fn yield_stmt(&mut self, block: &mut Block, span: Span, expr: Expr) -> Result<(), IRError> {
        let node = self.expr(expr)?;
        block.push(IRNode::new(span, NodeKind::Yield(Box::new(node))));

        Ok(())
    }

    fn ret(&mut self, block: &mut Block, span: Span, expr: Expr) -> Result<(), IRError> {
        let node = self.expr(expr)?;
        block.push(IRNode::new(span, NodeKind::Return(Box::new(node))));

        Ok(())
    }

    fn for_cond(
        &mut self,
        block: &mut Block,
        span: Span,
        init: Stmt,
        cond: Expr,
        post: Expr,
        body: Vec<Stmt>,
    ) -> Result<(), IRError> {
        match init.kind {
            // We need to make sure variable assignments aren't optimized
            StmtKind::Let(name, Some(expr)) => {
                let value = self.expr(expr)?;
                self.let_with_value(block, span, name, value)?;
            }
            _ => self.stmt(block, init)?,
        }

        block.push(IRNode::new(
            span,
            NodeKind::Loop(Box::new(Loop {
                id: self.next_loop(),
                pre: None,
                cond: self.expr(cond)?,
                post: Some(self.expr(post)?),
                body: self.block(body)?,
            })),
        ));

        Ok(())
    }

    fn for_in(
        &mut self,
        block: &mut Block,
        span: Span,
        lhs: Expr,
        rhs: Expr,
        body: Vec<Stmt>,
    ) -> Result<(), IRError> {
        let mut scope = Scope::default();
        let item_id = scope
            .declare_var(self.next_var(), ident(lhs))
            .initialized()
            .finish();
        let resumable_id = self.next_var();

        // Prepare resumable
        block.push(IRNode::new(
            span,
            NodeKind::StoreVariable(
                VariableKind::Global,
                resumable_id,
                Box::new(self.expr(rhs)?),
            ),
        ));

        // For every iteration, we load the resumable and store the current item
        let pre = IRNode::new(
            span,
            NodeKind::StoreVariable(
                VariableKind::Global,
                item_id,
                Box::new(IRNode::new(
                    span,
                    NodeKind::Call(
                        Box::new(IRNode::new(
                            span,
                            NodeKind::LoadVariable(VariableKind::Global, resumable_id),
                        )),
                        vec![],
                    ),
                )),
            ),
        );

        // Check if the resumable is completed
        let cond = IRNode::new(
            span,
            NodeKind::Binary(
                Box::new(IRNode::new(
                    span,
                    NodeKind::LoadVariable(VariableKind::Global, item_id),
                )),
                BinaryOp::Ne,
                Box::new(IRNode::new(
                    span,
                    NodeKind::Const(IRValue::Atom(consts::NIL)),
                )),
            ),
        );

        block.push(IRNode::new(
            span,
            NodeKind::Loop(Box::new(Loop {
                id: self.next_loop(),
                pre: Some(pre),
                cond,
                post: None,
                body: self.scoped_block(span, scope, body)?,
            })),
        ));

        Ok(())
    }

    fn for_stmt(
        &mut self,
        block: &mut Block,
        span: Span,
        expr: Expr,
        tree: Vec<Stmt>,
    ) -> Result<(), IRError> {
        let cond = self.expr(expr)?;

        block.push(IRNode::new(
            span,
            NodeKind::Loop(Box::new(Loop {
                id: self.next_loop(),
                pre: None,
                cond,
                post: None,
                body: self.scoped_block(span, Scope::default(), tree)?,
            })),
        ));

        Ok(())
    }

    fn if_stmt(
        &mut self,
        block: &mut Block,
        span: Span,
        mut if_stmt: IfStmt,
    ) -> Result<(), IRError> {
        let mut cases = vec![];

        loop {
            let IfStmt(cond, body, alt) = if_stmt;

            match cond {
                Some(cond) => {
                    let cond = self.expr(cond)?;
                    let body = self.scoped_block(span, Scope::default(), body)?;

                    cases.push((cond, body));

                    if let Some(alt) = alt {
                        if_stmt = *alt;
                        continue;
                    }

                    block.push(IRNode::new(span, NodeKind::Condition(cases, None)));

                    return Ok(());
                }
                None => {
                    let mut body = self.scoped_block(span, Scope::default(), body)?;

                    if cases.is_empty() {
                        block.merge(&mut body);
                    } else {
                        block.push(IRNode::new(span, NodeKind::Condition(cases, Some(body))));
                    }

                    return Ok(());
                }
            };
        }
    }

    fn break_stmt(&mut self, block: &mut Block, span: Span) -> Result<(), IRError> {
        block.push(IRNode::new(span, NodeKind::Break(self.loops - 1)));
        Ok(())
    }

    fn continue_stmt(&mut self, block: &mut Block, span: Span) -> Result<(), IRError> {
        block.push(IRNode::new(span, NodeKind::Continue(self.loops - 1)));
        Ok(())
    }

    fn stmt(&mut self, block: &mut Block, stmt: Stmt) -> Result<(), IRError> {
        match stmt.kind {
            StmtKind::Break => self.break_stmt(block, stmt.span),
            StmtKind::Continue => self.continue_stmt(block, stmt.span),
            StmtKind::If(if_stmt) => self.if_stmt(block, stmt.span, if_stmt),
            StmtKind::Expr(expr) => self.expr_stmt(block, stmt.span, expr),
            StmtKind::Yield(expr) => self.yield_stmt(block, stmt.span, expr),
            StmtKind::Return(expr) => self.ret(block, stmt.span, expr),
            StmtKind::Import(path) => self.import(stmt.span, path),
            StmtKind::Fn(fn_stmt) => self.fn_stmt(stmt.span, fn_stmt),
            StmtKind::Class(name, body, public) => self.class(name, body, public),
            StmtKind::Let(name, expr) => self.let_stmt(block, stmt.span, name, expr),
            StmtKind::For(expr, stmts) => self.for_stmt(block, stmt.span, expr, stmts),
            StmtKind::ForIn(lhs, rhs, body) => self.for_in(block, stmt.span, lhs, rhs, body),
            StmtKind::ForCond {
                init,
                cond,
                step,
                body,
            } => self.for_cond(block, stmt.span, *init, cond, step, body),
        }
    }

    fn enter_scope(&mut self, scope: Scope) {
        self.scope.push_front(scope);
    }

    fn exit_scope(&mut self, span: Span) -> Result<(), IRError> {
        if let Some(scope) = self.scope.pop_front() {
            if let Some(var) = scope.vars.iter().find(|v| !v.used) {
                return Err(ErrorKind::NameUnused(var.name.clone()).at(span));
            }
        }

        Ok(())
    }

    fn scoped_expr(&mut self, span: Span, scope: Scope, expr: Expr) -> Result<IRNode, IRError> {
        self.enter_scope(scope);
        let node = self.expr(expr)?;
        self.exit_scope(span)?;

        Ok(node)
    }

    fn scoped_block(
        &mut self,
        span: Span,
        scope: Scope,
        tree: Vec<Stmt>,
    ) -> Result<Block, IRError> {
        self.enter_scope(scope);
        let block = self.block(tree)?;
        self.exit_scope(span)?;

        Ok(block)
    }

    fn block(&mut self, tree: Vec<Stmt>) -> Result<Block, IRError> {
        let mut block = Block::default();

        for stmt in tree {
            self.stmt(&mut block, stmt)?;
        }

        Ok(block)
    }

    pub fn compile(mut self, tree: Vec<Stmt>) -> Result<Program, IRError> {
        self.register(&tree)?;

        // In the root scope, `self` refers to the current package
        let mut scope = Scope::default();
        scope
            .declare_var(self.next_var(), "self".to_string())
            .initialized()
            .used()
            .finish();

        let block = self.scoped_block(Span::default(), scope, tree)?;

        Ok(Program {
            body: block,
            imports: self.imports.into_vec(),
            funcs: self.funcs.into_vec(),
            classes: self.classes.into_vec(),
        })
    }
}
