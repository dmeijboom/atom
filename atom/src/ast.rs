use std::ops::Range;

pub type Pos = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Comment {
    pub content: String,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Name(String),
    Tuple(Vec<String>),
    Array(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComparisonOp {
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArithmeticOp {
    Mul,
    Div,
    Add,
    Sub,
    Exp,
    BitAnd,
    BitOr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOp {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAssertExpr {
    pub left: Expr,
    pub right: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ComparisonOp,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Expr,
    pub args: Vec<Expr>,
    pub keyword_args: Vec<KeywordArg>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    pub type_name: String,
    pub expr: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArithmeticExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ArithmeticOp,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: LogicalOp,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentExpr {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnwrapExpr {
    pub expr: Box<Expr>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NotExpr {
    pub expr: Box<Expr>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub items: Vec<Expr>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeywordArg {
    pub name: String,
    pub value: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValue {
    pub key: Expr,
    pub value: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapExpr {
    pub key_values: Vec<KeyValue>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Byte(u8),
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    Symbol(String),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralExpr {
    pub literal: Literal,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberCondExpr {
    pub object: Expr,
    pub member: String,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpr {
    pub object: Expr,
    pub member: String,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub object: Expr,
    pub index: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpr {
    pub from: Expr,
    pub to: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MakeRefExpr {
    pub expr: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DerefExpr {
    pub expr: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TemplateComponent {
    String(String),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TemplateExpr {
    pub components: Vec<TemplateComponent>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(LiteralExpr),
    Ident(IdentExpr),
    Call(Box<CallExpr>),
    Cast(Box<CastExpr>),
    Not(NotExpr),
    Unwrap(UnwrapExpr),
    Array(ArrayExpr),
    Tuple(TupleExpr),
    Map(MapExpr),
    Member(Box<MemberExpr>),
    MemberCond(Box<MemberCondExpr>),
    Arithmetic(Box<ArithmeticExpr>),
    Comparison(Box<ComparisonExpr>),
    Logical(Box<LogicalExpr>),
    MakeRef(Box<MakeRefExpr>),
    Deref(Box<DerefExpr>),
    Index(Box<IndexExpr>),
    Range(Box<RangeExpr>),
    Template(TemplateExpr),
    TypeAssert(Box<TypeAssertExpr>),
}

impl Expr {
    pub fn pos(&self) -> Pos {
        match self {
            Self::Literal(lit_expr) => lit_expr.pos.clone(),
            Self::Ident(ident_expr) => ident_expr.pos.clone(),
            Self::Call(call_expr) => call_expr.pos.clone(),
            Self::Not(not_expr) => not_expr.pos.clone(),
            Self::Unwrap(unwrap_expr) => unwrap_expr.pos.clone(),
            Self::Array(array_expr) => array_expr.pos.clone(),
            Self::Tuple(tuple_expr) => tuple_expr.pos.clone(),
            Self::Map(map_expr) => map_expr.pos.clone(),
            Self::Member(member_expr) => member_expr.pos.clone(),
            Self::MemberCond(member_cond_expr) => member_cond_expr.pos.clone(),
            Self::Arithmetic(arithmetic_expr) => arithmetic_expr.pos.clone(),
            Self::Comparison(comparison_expr) => comparison_expr.pos.clone(),
            Self::Logical(logical_expr) => logical_expr.pos.clone(),
            Self::MakeRef(make_ref_expr) => make_ref_expr.pos.clone(),
            Self::Deref(deref_expr) => deref_expr.pos.clone(),
            Self::Index(index_expr) => index_expr.pos.clone(),
            Self::Range(range_expr) => range_expr.pos.clone(),
            Self::Cast(cast_expr) => cast_expr.pos.clone(),
            Self::Template(template_expr) => template_expr.pos.clone(),
            Self::TypeAssert(type_assert_expr) => type_assert_expr.pos.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RaiseStmt {
    pub expr: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub name: String,
    pub value: Expr,
    pub mutable: bool,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclStmt {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg {
    pub name: String,
    pub mutable: bool,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDeclStmt {
    pub name: String,
    pub public: bool,
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFnDeclStmt {
    pub name: String,
    pub public: bool,
    pub args: Vec<FnArg>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt {
    pub left: Expr,
    pub right: Expr,
    pub op: Option<AssignOp>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub expr: Expr,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub cond: Expr,
    pub pos: Pos,
    pub alt: Option<Box<Stmt>>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseStmt {
    pub pos: Pos,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub mutable: bool,
    pub public: bool,
    pub value: Option<Expr>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDeclStmt {
    pub name: String,
    pub public: bool,
    pub fields: Vec<Field>,
    pub funcs: Vec<FnDeclStmt>,
    pub extern_funcs: Vec<ExternFnDeclStmt>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceFn {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceDeclStmt {
    pub name: String,
    pub public: bool,
    pub functions: Vec<InterfaceFn>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleStmt {
    pub name: String,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub expr: Option<Expr>,
    pub alias: Option<Variable>,
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStmt {
    pub label: Option<String>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnsafeStmt {
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    If(IfStmt),
    Else(ElseStmt),
    For(ForStmt),
    Expr(ExprStmt),
    Let(LetStmt),
    Break(BreakStmt),
    Raise(RaiseStmt),
    LetDecl(LetDeclStmt),
    FnDecl(FnDeclStmt),
    ExternFnDecl(ExternFnDeclStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    Import(ImportStmt),
    Unsafe(UnsafeStmt),
    Module(ModuleStmt),
    ClassDecl(ClassDeclStmt),
    InterfaceDecl(InterfaceDeclStmt),
}

impl Stmt {
    pub fn pos(&self) -> Pos {
        match self {
            Stmt::If(if_stmt) => if_stmt.pos.clone(),
            Stmt::Else(else_stmt) => else_stmt.pos.clone(),
            Stmt::For(for_stmt) => for_stmt.pos.clone(),
            Stmt::Break(break_stmt) => break_stmt.pos.clone(),
            Stmt::Raise(raise_stmt) => raise_stmt.pos.clone(),
            Stmt::Expr(expr_stmt) => expr_stmt.pos.clone(),
            Stmt::Let(let_stmt) => let_stmt.pos.clone(),
            Stmt::LetDecl(let_decl_stmt) => let_decl_stmt.pos.clone(),
            Stmt::FnDecl(fn_decl_stmt) => fn_decl_stmt.pos.clone(),
            Stmt::ExternFnDecl(extern_decl_stmt) => extern_decl_stmt.pos.clone(),
            Stmt::Assign(assign_stmt) => assign_stmt.pos.clone(),
            Stmt::Return(return_stmt) => return_stmt.pos.clone(),
            Stmt::Module(module_stmt) => module_stmt.pos.clone(),
            Stmt::Import(import_stmt) => import_stmt.pos.clone(),
            Stmt::Unsafe(unsafe_stmt) => unsafe_stmt.pos.clone(),
            Stmt::ClassDecl(class_decl_stmt) => class_decl_stmt.pos.clone(),
            Stmt::InterfaceDecl(interface_decl_stmt) => interface_decl_stmt.pos.clone(),
        }
    }

    // Note that not all statements support storing comments in their type
    pub fn with_comments(mut self, comments: Vec<Comment>) -> Self {
        match &mut self {
            Self::FnDecl(fn_decl_stmt) => {
                fn_decl_stmt.comments = comments;
            }
            Self::InterfaceDecl(interface_decl_stmt) => {
                interface_decl_stmt.comments = comments;
            }
            Self::ClassDecl(class_decl_stmt) => {
                class_decl_stmt.comments = comments;
            }
            _ => {}
        }

        self
    }
}
