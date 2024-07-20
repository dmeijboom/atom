use crate::lexer::Span;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum UnaryOp {
    Not,
}

#[derive(Debug)]
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
    Assign(Option<AssignOp>, Box<Expr>, Box<Expr>),
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

impl Expr {
    pub fn is_assign(&self) -> bool {
        matches!(self.kind, ExprKind::Assign(_, _, _))
    }
}

#[derive(Debug)]
pub struct IfStmt(pub Option<Expr>, pub Vec<Stmt>, pub Option<Box<IfStmt>>);

#[derive(Debug)]
pub enum StmtKind {
    If(IfStmt),
    Expr(Expr),
    Return(Expr),
    Let(String, Expr),
    For(Expr, Vec<Stmt>),
    ForCond(Box<Stmt>, Expr, Expr, Vec<Stmt>),
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
