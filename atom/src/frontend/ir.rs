use std::{
    collections::VecDeque,
    hash::{Hash, Hasher},
    ops::*,
};

use serde::Serialize;

use crate::{
    backend::GlobalContext,
    collections::{OrderedMap, OrderedSet},
    error::{IntoSpanned, SpannedError},
    frontend::ast::StmtKind,
    runtime::{consts, ops, BigInt},
};

use super::{
    ast::{BinaryOp, Expr, ExprKind, Literal, Path, Stmt, UnaryOp},
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
    #[error("missing scope")]
    MissingScope,
    #[error("name '{0}' is already defined")]
    DuplicateName(String),
    #[error("package '{0}' already imported")]
    DuplicateImport(String),
    #[error("fn '{0}' already exists")]
    DuplicateFn(String),
    // #[error("method '{0}' already exists")]
    // DuplicateMethod(String),
    #[error("class '{0}' already exists")]
    DuplicateClass(String),
    #[error("name '{0}' is not initialized")]
    NameUninitialized(String),
    #[error("name '{0}' is not used")]
    NameUnused(String),
    // #[error("failed to write bytecode(s): {0}")]
    // FailedWriteBytecode(#[from] io::Error),
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
    children: Vec<IRNode>,
}

impl Block {
    pub fn push(&mut self, node: IRNode) {
        self.children.push(node);
    }
}

#[derive(Debug, Serialize)]
pub struct IRNode {
    kind: NodeKind,
    span: Span,
}

impl IRNode {
    pub fn new(span: Span, kind: NodeKind) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub enum VariableKind {
    Local,
    Variable,
}

#[derive(Debug, Serialize)]
pub enum NodeKind {
    FnRef(u32),
    ClassRef(u32),
    ImportRef(u32),
    Const(IRValue),
    Array(Vec<IRNode>),
    Discard(Box<IRNode>),
    Unary(UnaryOp, Box<IRNode>),
    Member(Box<IRNode>, String),
    Call(Box<IRNode>, Vec<IRNode>),
    Branch(Box<IRNode>, Block, Block),
    CompMember(Box<IRNode>, Box<IRNode>),
    Binary(Box<IRNode>, BinaryOp, Box<IRNode>),
    Load(VariableKind, usize),
    Store(VariableKind, usize, Box<IRNode>),
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

#[derive(Default)]
struct Scope {
    vars: Vec<Variable>,
}

#[derive(Default)]
pub struct Fn {
    name: String,
}

#[derive(Default)]
pub struct Class {
    name: String,
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

pub struct IR<'a> {
    vars: usize,
    imports: OrderedSet<String>,
    funcs: OrderedMap<String, Fn>,
    classes: OrderedMap<String, Class>,
    scope: VecDeque<Scope>,
    consts: OrderedSet<IRValue>,
    ctx: &'a mut GlobalContext,
}

impl<'a> IR<'a> {
    pub fn new(ctx: &'a mut GlobalContext) -> Self {
        Self {
            vars: 0,
            imports: OrderedSet::default(),
            funcs: OrderedMap::default(),
            classes: OrderedMap::default(),
            scope: VecDeque::new(),
            consts: OrderedSet::default(),
            ctx,
        }
    }

    fn register(&mut self, stmts: &[Stmt]) -> Result<(), IRError> {
        for stmt in stmts {
            match &stmt.kind {
                StmtKind::Fn(fn_stmt) => {
                    if self.funcs.contains_key(fn_stmt.name.as_str()) {
                        return Err(ErrorKind::DuplicateFn(fn_stmt.name.clone()).at(stmt.span));
                    }

                    self.funcs.insert(fn_stmt.name.clone(), Fn::default());
                }
                StmtKind::Class(name, _, _) => {
                    if self.classes.contains_key(name.as_str()) {
                        return Err(ErrorKind::DuplicateClass(name.clone()).at(stmt.span));
                    }

                    self.classes.insert(name.clone(), Class::default());
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn lookup_var(&mut self, name: &str) -> Option<&mut Variable> {
        self.scope
            .iter_mut()
            .rev()
            .find_map(|scope| scope.vars.iter_mut().rev().find(|var| var.name == name))
    }

    fn declare_var(
        &mut self,
        span: Span,
        name: String,
        initialized: bool,
    ) -> Result<usize, IRError> {
        let id = self.vars;
        self.vars += 1;

        let scope = self
            .scope
            .front_mut()
            .ok_or(ErrorKind::MissingScope.at(span))?;

        let mut var = Variable::new(VariableKind::Variable, id, name);
        var.initialized = initialized;

        scope.vars.push(var);

        Ok(id)
    }

    fn import(&mut self, span: Span, path: Path) -> Result<(), IRError> {
        let name = path.join("/");

        if self.imports.contains(&name) {
            return Err(ErrorKind::DuplicateImport(name).at(span));
        }

        self.imports.insert(name);

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

        if !self.imports.contains(&std_name) {
            self.import(span, vec![std_name.clone()])?;
        }

        return self.member(span, ExprKind::Ident(std_name).at(span), name);
    }

    // Loads a symbol based on the following order: local/var > class > fn > import
    fn ident(&mut self, span: Span, name: String) -> Result<IRNode, IRError> {
        // Locals / variables
        if let Some(var) = self.lookup_var(&name) {
            if !var.initialized {
                return Err(ErrorKind::NameUninitialized(name.clone()).at(span));
            }

            var.used = true;

            if let Some(value) = var.value.clone() {
                return Ok(IRNode::new(span, NodeKind::Const(value)));
            }

            return Ok(IRNode::new(span, NodeKind::Load(var.kind, var.id)));
        }

        // Functions
        if let Some(id) = self.funcs.get(&name) {
            return Ok(IRNode::new(span, NodeKind::FnRef(id)));
        }

        // Imports
        if let Some(id) = self.imports.find(&name) {
            return Ok(IRNode::new(span, NodeKind::ImportRef(id)));
        }

        if PRELUDE.contains(&name.as_str()) {
            return self.prelude(span, name);
        }

        todo!("{name}")
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

    fn expr(&mut self, expr: Expr) -> Result<IRNode, IRError> {
        match expr.kind {
            ExprKind::Ident(name) => self.ident(expr.span, name),
            ExprKind::Array(exprs) => todo!(),
            ExprKind::Literal(literal) => self.lit(expr.span, literal),
            ExprKind::Unary(unary_op, expr) => todo!(),
            ExprKind::Member(expr, name) => self.member(expr.span, *expr, name),
            ExprKind::Call(expr, args) => self.call(expr.span, *expr, args),
            ExprKind::CompMember(expr, expr1) => todo!(),
            ExprKind::Range(expr, expr1) => todo!(),
            ExprKind::Binary(lhs, binary_op, rhs) => self.binary(expr.span, *lhs, binary_op, *rhs),
            ExprKind::Assign(assign_op, expr, expr1) => todo!(),
            ExprKind::Match(expr, match_arms, expr1) => todo!(),
        }
    }

    fn let_stmt(
        &mut self,
        block: &mut Block,
        span: Span,
        name: String,
        expr: Option<Expr>,
    ) -> Result<(), IRError> {
        let id = self.declare_var(span, name, expr.is_some())?;

        if let Some(expr) = expr {
            let node = self.expr(expr)?;

            block.push(IRNode::new(
                span,
                NodeKind::Store(VariableKind::Variable, id, Box::new(node)),
            ));
        }

        Ok(())
    }

    fn expr_stmt(&mut self, block: &mut Block, span: Span, expr: Expr) -> Result<(), IRError> {
        block.push(IRNode::new(
            span,
            NodeKind::Discard(Box::new(self.expr(expr)?)),
        ));

        Ok(())
    }

    fn stmt(&mut self, block: &mut Block, stmt: Stmt) -> Result<(), IRError> {
        match stmt.kind {
            StmtKind::Break => todo!(),
            StmtKind::Continue => todo!(),
            StmtKind::If(if_stmt) => todo!(),
            StmtKind::Expr(expr) => self.expr_stmt(block, stmt.span, expr),
            StmtKind::Yield(expr) => todo!(),
            StmtKind::Return(expr) => todo!(),
            StmtKind::Import(path) => self.import(stmt.span, path),
            StmtKind::Fn(fn_stmt) => todo!(),
            StmtKind::Class(_, stmts, _) => todo!(),
            StmtKind::Let(name, expr) => self.let_stmt(block, stmt.span, name, expr),
            StmtKind::For(expr, stmts) => todo!(),
            StmtKind::ForIn(expr, expr1, stmts) => todo!(),
            StmtKind::ForCond {
                init,
                cond,
                step,
                body,
            } => todo!(),
        }
    }

    fn scoped_block(&mut self, tree: Vec<Stmt>) -> Result<Block, IRError> {
        self.scope.push_front(Scope::default());
        let block = self.block(tree)?;
        self.scope.pop_front();
        Ok(block)
    }

    fn block(&mut self, tree: Vec<Stmt>) -> Result<Block, IRError> {
        let mut block = Block::default();

        for stmt in tree {
            self.stmt(&mut block, stmt)?;
        }

        Ok(block)
    }

    pub fn compile(mut self, tree: Vec<Stmt>) -> Result<Block, IRError> {
        self.register(&tree)?;
        let block = self.scoped_block(tree)?;

        // In the root scope, `self` refers to the current package

        Ok(block)
    }
}
