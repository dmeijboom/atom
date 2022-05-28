use std::fmt::{Debug, Display, Formatter};

pub trait InferType {
    fn infer_type(&self) -> Option<Type>;
}

#[derive(Clone, Default, PartialEq)]
pub struct Span {
    pub begin: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} [{}..{}]",
            self.line, self.column, self.begin, self.end
        )
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
}

impl Type {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Debug, PartialEq)]
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
pub enum StmtKind {
    Return(Expr),
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
    BitShiftLeft,
    BitShiftRight,
}

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub span: Span,
    pub op: BinaryOp,
    pub left: Expr,
    pub right: Expr,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Binary(Box<Binary>),
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
    Float(f64),
    String(String),
}

impl InferType for LiteralKind {
    fn infer_type(&self) -> Option<Type> {
        Some(match self {
            LiteralKind::Int(_) => Type::new("int".to_string()),
            LiteralKind::Float(_) => Type::new("float".to_string()),
            LiteralKind::String(_) => Type::new("string".to_string()),
        })
    }
}
