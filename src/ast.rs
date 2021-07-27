use std::ops::Range;

pub type Pos = Range<usize>;

#[derive(Debug, PartialEq)]
pub enum ComparisonOp {
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
}

#[derive(Debug, PartialEq)]
pub enum ArithmeticOp {
    Mul,
    Div,
    Add,
    Sub,
    BitAnd,
    BitOr,
}

#[derive(Debug, PartialEq)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub struct ComparisonExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ComparisonOp,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Expr,
    pub args: Vec<Expr>,
    pub keyword_args: Vec<KeywordArg>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ArithmeticExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ArithmeticOp,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct LogicalExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: LogicalOp,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct IdentExpr {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct NotExpr {
    pub expr: Box<Expr>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ArrayExpr {
    pub items: Vec<Expr>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct KeywordArg {
    pub name: String,
    pub value: Expr,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct KeyValue {
    pub key: Expr,
    pub value: Expr,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct MapExpr {
    pub key_values: Vec<KeyValue>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

#[derive(Debug, PartialEq)]
pub struct LiteralExpr {
    pub literal: Literal,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct MemberExpr {
    pub object: Expr,
    pub member: String,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct IndexExpr {
    pub object: Expr,
    pub index: Expr,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Literal(LiteralExpr),
    Ident(IdentExpr),
    Call(Box<CallExpr>),
    Not(NotExpr),
    Array(ArrayExpr),
    Map(MapExpr),
    Member(Box<MemberExpr>),
    Arithmetic(Box<ArithmeticExpr>),
    Comparison(Box<ComparisonExpr>),
    Logical(Box<LogicalExpr>),
    Index(Box<IndexExpr>),
}

impl Expr {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Literal(lit) => lit.pos.clone(),
            Self::Ident(ident) => ident.pos.clone(),
            Self::Call(call) => call.pos.clone(),
            Self::Not(not) => not.pos.clone(),
            Self::Array(array) => array.pos.clone(),
            Self::Map(map) => map.pos.clone(),
            Self::Member(member) => member.pos.clone(),
            Self::Arithmetic(arithmetic) => arithmetic.pos.clone(),
            Self::Comparison(comparison) => comparison.pos.clone(),
            Self::Logical(logical) => logical.pos.clone(),
            Expr::Index(index) => index.pos.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct LetStmt {
    pub name: String,
    pub value: Expr,
    pub mutable: bool,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct LetDeclStmt {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct FnArg {
    pub name: String,
    pub mutable: bool,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct FnDeclStmt {
    pub name: String,
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct AssignStmt {
    pub left: Expr,
    pub right: Expr,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub expr: Expr,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
    pub cond: Expr,
    pub pos: Pos,
    pub alt: Vec<Stmt>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct Field {
    pub name: String,
    pub mutable: bool,
    pub value: Option<Expr>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ClassDeclStmt {
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<FnDeclStmt>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ModuleStmt {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    If(IfStmt),
    Expr(ExprStmt),
    Let(LetStmt),
    LetDecl(LetDeclStmt),
    FnDecl(FnDeclStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    Module(ModuleStmt),
    ClassDecl(ClassDeclStmt),
}

impl Stmt {
    pub fn pos(&self) -> Pos {
        match self {
            Stmt::If(if_stmt) => if_stmt.pos.clone(),
            Stmt::Expr(expr_stmt) => expr_stmt.pos.clone(),
            Stmt::Let(let_stmt) => let_stmt.pos.clone(),
            Stmt::LetDecl(let_decl_stmt) => let_decl_stmt.pos.clone(),
            Stmt::FnDecl(fn_decl_stmt) => fn_decl_stmt.pos.clone(),
            Stmt::Assign(assign_stmt) => assign_stmt.pos.clone(),
            Stmt::Return(return_stmt) => return_stmt.pos.clone(),
            Stmt::Module(module_stmt) => module_stmt.pos.clone(),
            Stmt::ClassDecl(class_decl_stmt) => class_decl_stmt.pos.clone(),
        }
    }
}
