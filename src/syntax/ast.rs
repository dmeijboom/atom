use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
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
            "{}:{}[{}..{}]",
            self.line, self.column, self.begin, self.end
        )
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
pub struct FnSig {}

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
pub enum ExprKind {
    Literal(Literal),
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
