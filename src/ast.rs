use crate::lexer::Span;

#[derive(Debug)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Gt,
    Gte,
    Lt,
    Lte,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
}

#[derive(Debug)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub enum ExprKind {
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Array(Vec<Expr>),
    Member(Box<Expr>, String),
    CompMember(Box<Expr>, Box<Expr>),
    Ident(String),
    Call(Box<Expr>, Vec<Expr>),
    Literal(Literal),
}

impl ExprKind {
    pub fn at(self, span: Span) -> Expr {
        Expr { span, kind: self }
    }
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(Expr),
    Return(Expr),
    Let(String, Expr),
    Assign(String, Expr),
    Fn(String, Vec<String>, Vec<Stmt>),
}

impl StmtKind {
    pub fn at(self, span: Span) -> Stmt {
        Stmt { span, kind: self }
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}
