#[derive(Debug)]
pub enum ComparisonOp {
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
}

#[derive(Debug)]
pub enum ArithmeticOp {
    Mul,
    Div,
    Add,
    Sub,
    BitAnd,
    BitOr,
}

#[derive(Debug)]
pub struct ComparisonExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ComparisonOp,
}

#[derive(Debug)]
pub struct CallExpr {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug)]
pub struct ArithmeticExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ArithmeticOp,
}

#[derive(Debug)]
pub enum Expr {
    Int(i64),
    Ident(String),
    Call(CallExpr),
    Not(Box<Expr>),
    Arithmetic(Box<ArithmeticExpr>),
    Comparison(Box<ComparisonExpr>),
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
}

