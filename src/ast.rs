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
}

#[derive(Debug)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub enum Expr {
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Array(Vec<Expr>),
    Member(Box<Expr>, Box<Expr>),
    Ident(String),
    Call(Box<Expr>, Vec<Expr>),
    Literal(Literal),
}

#[derive(Debug)]
pub enum Stmt {
    Let(String, Expr),
    Expr(Expr),
    Fn(String, Vec<String>, Vec<Stmt>),
}
