#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
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
    Binary(Box<Expr>, Op, Box<Expr>),
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
}
