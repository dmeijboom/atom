use crate::frontend::scope::{Scope, ScopeId};
use crate::frontend::syntax::{BinaryOp, LiteralKind, LogicalOp, Span};
use crate::frontend::Type;

#[derive(Debug)]
pub struct Program {
    pub scopes: Vec<Scope>,
    pub nodes: Vec<Node>,
}

#[derive(Debug)]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
}

impl Node {
    pub fn new(span: Span, kind: NodeKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug)]
pub enum NodeKind {
    FnDef(FnDef),
}

#[derive(Debug)]
pub struct FnDef {
    pub name: String,
    pub body: Vec<Stmt>,
    pub return_type: Type,
    pub scope: ScopeId,
}

#[derive(Debug)]
pub struct Stmt {
    pub span: Span,
    pub scope: ScopeId,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(span: Span, scope: ScopeId, kind: StmtKind) -> Self {
        Self { span, scope, kind }
    }
}

#[derive(Debug)]
pub enum StmtKind {
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    Assign(String, Expr),
    Let(String, Option<Expr>),
    Return(Option<Expr>),
    Expr(Expr),
}

#[derive(Debug)]
pub struct Expr {
    pub ty: Type,
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: Span, ty: Type, kind: ExprKind) -> Self {
        Self { ty, span, kind }
    }
}

#[derive(Debug)]
pub enum ExprKind {
    Fn(String),
    Name(String),
    Literal(LiteralKind),
    Call(Box<Expr>, Vec<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Logical(LogicalOp, Box<Expr>, Box<Expr>),
}
