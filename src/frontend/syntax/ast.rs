use std::fmt::{Debug, Display, Formatter};

use crate::frontend::types;

pub trait InferType {
    fn infer_type(&self) -> types::Type;
}

#[derive(Clone, Default, PartialEq)]
pub struct Span {
    pub begin: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    #[cfg(test)]
    pub fn new(line: usize, column: usize, begin: usize, end: usize) -> Self {
        Self {
            line,
            column,
            begin,
            end,
        }
    }

    pub fn ends(mut self, other: &Span) -> Self {
        self.end = other.begin;
        self
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span ({})", self)
    }
}

#[derive(Debug, PartialEq)]
pub struct Node {
    pub span: Span,
    pub kind: NodeKind,
}

impl Node {
    pub fn new(span: Span, kind: NodeKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, PartialEq)]
pub enum NodeKind {
    FnDef(FnDef),
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub name: String,
    pub span: Span,
}

impl Type {
    pub fn new(span: Span, name: String) -> Self {
        Self { span, name }
    }
}

#[derive(Debug, Default, PartialEq)]
pub struct FnSig {
    pub params: Vec<Type>,
    pub return_type: Option<Type>,
}

#[derive(Debug, PartialEq)]
pub struct FnDef {
    pub name: String,
    pub sig: FnSig,
    pub body: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(span: Span, kind: StmtKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, PartialEq)]
pub struct Let {
    pub span: Span,
    pub name: String,
    pub mutable: bool,
    pub ty: Option<Type>,
    pub value: Option<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Alt {
    ElseIf(Box<If>),
    Else(Vec<Stmt>),
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub span: Span,
    pub cond: Expr,
    pub body: Vec<Stmt>,
    pub alt: Option<Alt>,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {
    If(If),
    Let(Let),
    Assign(String, Expr),
    Return(Option<Expr>),
    Expr(Expr),
    ExprEnd(Expr),
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl Expr {
    pub fn new(span: Span, kind: ExprKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub span: Span,
    pub op: BinaryOp,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug, PartialEq)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub struct Logical {
    pub span: Span,
    pub op: LogicalOp,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub span: Span,
    pub callee: Expr,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Call(Box<Call>),
    Ident(String),
    Literal(Literal),
    Binary(Box<Binary>),
    Logical(Box<Logical>),
}

#[derive(Debug, PartialEq)]
pub struct Literal {
    pub span: Span,
    pub kind: LiteralKind,
}

impl Literal {
    pub fn new(span: Span, kind: LiteralKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Int(i64),
    Bool(bool),
    Float(f64),
    String(String),
}

impl InferType for LiteralKind {
    fn infer_type(&self) -> types::Type {
        match self {
            LiteralKind::Int(_) => types::INT,
            LiteralKind::Bool(_) => types::BOOL,
            LiteralKind::Float(_) => types::FLOAT,
            LiteralKind::String(_) => types::STRING,
        }
    }
}
