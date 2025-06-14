use serde::Serialize;

use crate::lexer::Span;

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

#[derive(Debug, Clone, Copy, Serialize)]
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
    BitOr,
    BitAnd,
    Xor,
}

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Literal {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
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
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct IfStmt(pub Option<Expr>, pub Vec<Stmt>, pub Option<Box<IfStmt>>);

pub type Path = Vec<String>;

#[derive(Debug, Serialize)]
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

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub enum StmtKind {
    Break,
    Continue,
    If(IfStmt),
    Expr(Expr),
    Return(Expr),
    Import(Path),
    For(Expr, Vec<Stmt>),
    Class(String, Vec<Stmt>, bool),
    Let(String, Option<Expr>),
    ExternFn(String, Vec<FnArg>, bool),
    Fn(String, Vec<FnArg>, Vec<Stmt>, bool),
    ForCond(Box<Stmt>, Expr, Expr, Vec<Stmt>),
}

impl StmtKind {
    pub fn at(self, span: Span) -> Stmt {
        Stmt { span, kind: self }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}
