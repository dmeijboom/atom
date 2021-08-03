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
pub struct MemberCondExpr {
    pub object: Expr,
    pub member: String,
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
pub struct RangeExpr {
    pub from: Expr,
    pub to: Expr,
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
    MemberCond(Box<MemberCondExpr>),
    Arithmetic(Box<ArithmeticExpr>),
    Comparison(Box<ComparisonExpr>),
    Logical(Box<LogicalExpr>),
    Index(Box<IndexExpr>),
    Range(Box<RangeExpr>),
}

impl Expr {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Literal(lit_expr) => lit_expr.pos.clone(),
            Self::Ident(ident_expr) => ident_expr.pos.clone(),
            Self::Call(call_expr) => call_expr.pos.clone(),
            Self::Not(not_expr) => not_expr.pos.clone(),
            Self::Array(array_expr) => array_expr.pos.clone(),
            Self::Map(map_expr) => map_expr.pos.clone(),
            Self::Member(member_expr) => member_expr.pos.clone(),
            Self::MemberCond(member_cond_expr) => member_cond_expr.pos.clone(),
            Self::Arithmetic(arithmetic_expr) => arithmetic_expr.pos.clone(),
            Self::Comparison(comparison_expr) => comparison_expr.pos.clone(),
            Self::Logical(logical_expr) => logical_expr.pos.clone(),
            Self::Index(index_expr) => index_expr.pos.clone(),
            Self::Range(range_expr) => range_expr.pos.clone(),
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
    pub public: bool,
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
    pub public: bool,
    pub value: Option<Expr>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ClassDeclStmt {
    pub name: String,
    pub public: bool,
    pub fields: Vec<Field>,
    pub methods: Vec<FnDeclStmt>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ImportStmt {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ModuleStmt {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct ForStmt {
    pub expr: Expr,
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct BreakStmt {
    pub label: Option<String>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub struct UnsafeStmt {
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    If(IfStmt),
    For(ForStmt),
    Expr(ExprStmt),
    Let(LetStmt),
    Break(BreakStmt),
    LetDecl(LetDeclStmt),
    FnDecl(FnDeclStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    Import(ImportStmt),
    Unsafe(UnsafeStmt),
    Module(ModuleStmt),
    ClassDecl(ClassDeclStmt),
}

impl Stmt {
    pub fn pos(&self) -> Pos {
        match self {
            Stmt::If(if_stmt) => if_stmt.pos.clone(),
            Stmt::For(for_stmt) => for_stmt.pos.clone(),
            Stmt::Break(break_stmt) => break_stmt.pos.clone(),
            Stmt::Expr(expr_stmt) => expr_stmt.pos.clone(),
            Stmt::Let(let_stmt) => let_stmt.pos.clone(),
            Stmt::LetDecl(let_decl_stmt) => let_decl_stmt.pos.clone(),
            Stmt::FnDecl(fn_decl_stmt) => fn_decl_stmt.pos.clone(),
            Stmt::Assign(assign_stmt) => assign_stmt.pos.clone(),
            Stmt::Return(return_stmt) => return_stmt.pos.clone(),
            Stmt::Module(module_stmt) => module_stmt.pos.clone(),
            Stmt::Import(import_stmt) => import_stmt.pos.clone(),
            Stmt::Unsafe(unsafe_stmt) => unsafe_stmt.pos.clone(),
            Stmt::ClassDecl(class_decl_stmt) => class_decl_stmt.pos.clone(),
        }
    }
}
