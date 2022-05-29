use crate::frontend::scope::{Scope, ScopeId};
use crate::frontend::syntax::{BinaryOp, LiteralKind, Span};
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
    Let(String, Expr),
    Return(Expr),
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
    Ident(String),
    Literal(LiteralKind),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
}
