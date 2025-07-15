use std::ops::{Deref, DerefMut};

use serde::Serialize;

use crate::{frontend::Span, runtime::BigInt};

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Rem,
    Div,
}

impl From<AssignOp> for BinaryOp {
    fn from(op: AssignOp) -> Self {
        match op {
            AssignOp::Add => BinaryOp::Add,
            AssignOp::Sub => BinaryOp::Sub,
            AssignOp::Mul => BinaryOp::Mul,
            AssignOp::Rem => BinaryOp::Rem,
            AssignOp::Div => BinaryOp::Div,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum UnaryOp {
    Not,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq, Hash)]
#[serde(rename_all = "camelCase")]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Rem,
    Div,
    Eq,
    Ne,
    Gt,
    Gte,
    Lt,
    Lte,
    Or,
    And,
    TypeAssert,
    BitOr,
    BitAnd,
    ShiftLeft,
    ShiftRight,
    Xor,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Literal {
    Int(i64),
    BigInt(BigInt),
    Float(f64),
    Atom(String),
    String(String),
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct MatchArm {
    pub pat: Expr,
    pub expr: Expr,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum ExprKind {
    Ident(String),
    Array(Vec<Expr>),
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>),
    Member(Box<Expr>, String),
    Call(Box<Expr>, Vec<Expr>),
    CompMember(Box<Expr>, Box<Expr>),
    Range(Option<Box<Expr>>, Option<Box<Expr>>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Assign(Option<AssignOp>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<MatchArm>, Option<Box<Expr>>),
}

impl ExprKind {
    pub fn at(self, span: Span) -> Expr {
        Expr { span, kind: self }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn is_assign(&self) -> bool {
        matches!(self.kind, ExprKind::Assign(_, _, _))
    }

    pub fn is_ident(&self) -> bool {
        matches!(self.kind, ExprKind::Ident(_))
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IfStmt(pub Option<Expr>, pub Vec<Stmt>, pub Option<Box<IfStmt>>);

#[derive(Debug, Clone, Default, Serialize)]
pub struct Path(pub Vec<String>);

impl Deref for Path {
    type Target = Vec<String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Path {
    fn deref_mut(&mut self) -> &mut Vec<String> {
        &mut self.0
    }
}

impl From<String> for Path {
    fn from(name: String) -> Self {
        Self(vec![name])
    }
}

impl Path {
    pub fn name(&self) -> &str {
        self.0[self.0.len() - 1].as_str()
    }

    pub fn full_name(&self) -> String {
        self.0.join("/")
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FnArg {
    pub name: String,
    pub span: Span,
}

impl FnArg {
    pub fn new(span: Span, name: String) -> Self {
        Self { name, span }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct FnStmt {
    pub name: String,
    pub public: bool,
    pub resumable: bool,
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum StmtKind {
    Break,
    Continue,
    If(IfStmt),
    Expr(Expr),
    Yield(Expr),
    Return(Expr),
    Import(Path),
    Fn(FnStmt),
    Class(String, Vec<Stmt>, bool),
    Let(String, Option<Expr>),
    For(Expr, Vec<Stmt>),
    ForIn(Expr, Expr, Vec<Stmt>),
    ForCond {
        init: Box<Stmt>,
        cond: Expr,
        step: Expr,
        body: Vec<Stmt>,
    },
}

impl StmtKind {
    pub fn at(self, span: Span) -> Stmt {
        Stmt { span, kind: self }
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}
