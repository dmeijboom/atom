use std::ops::Range;

pub type Pos = Range<usize>;

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
    pub pos: Pos,
}

#[derive(Debug)]
pub struct CallExpr {
    pub name: String,
    pub args: Vec<Expr>,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct ArithmeticExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ArithmeticOp,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct IdentExpr {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug)]
pub struct NotExpr {
    pub expr: Box<Expr>,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
}

#[derive(Debug)]
pub struct LiteralExpr {
    pub literal: Literal,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum Expr {
    Literal(LiteralExpr),
    Ident(IdentExpr),
    Call(CallExpr),
    Not(NotExpr),
    Arithmetic(Box<ArithmeticExpr>),
    Comparison(Box<ComparisonExpr>),
}

impl Expr {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Literal(lit) => lit.pos.clone(),
            Self::Ident(ident) => ident.pos.clone(),
            Self::Call(call) => call.pos.clone(),
            Self::Not(not) => not.pos.clone(),
            Self::Arithmetic(arithmetic) => arithmetic.pos.clone(),
            Self::Comparison(comparison) => comparison.pos.clone(),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
}

