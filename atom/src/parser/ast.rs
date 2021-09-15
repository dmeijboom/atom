use std::ops::Range;

use super::visitor::{Visitable, Visitor};

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
pub struct ClosureExpr {
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
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
    Closure(ClosureExpr),
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
            Self::Closure(closure_expr) => closure_expr.pos.clone(),
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

impl<E> Visitable<E> for Expr {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        unreachable!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for ExprStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RaiseStmt {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for RaiseStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub var: Variable,
    pub value: Expr,
    pub mutable: bool,
    pub pos: Pos,
}

impl<E> Visitable<E> for LetStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_var(&self.var)?;
        visitor.visit_expr(&self.value)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclStmt {
    pub name: String,
    pub pos: Pos,
}

impl<E> Visitable<E> for LetDeclStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&self.name)
    }
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

impl<E> Visitable<E> for FnDeclStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&self.name)?;

        for arg in self.args.iter() {
            visitor.visit_fn_arg(arg)?;
        }

        visitor.visit_block(&self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternFnDeclStmt {
    pub name: String,
    pub public: bool,
    pub args: Vec<FnArg>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

impl<E> Visitable<E> for ExternFnDeclStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&self.name)?;

        for arg in self.args.iter() {
            visitor.visit_fn_arg(arg)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt {
    pub left: Expr,
    pub right: Expr,
    pub op: Option<AssignOp>,
    pub pos: Pos,
}

impl<E> Visitable<E> for AssignStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&self.left)?;
        visitor.visit_expr(&self.right)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for ReturnStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub cond: Expr,
    pub pos: Pos,
    pub alt: Option<Box<Stmt>>,
    pub body: Vec<Stmt>,
}

impl<E> Visitable<E> for IfStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_block(&self.body)?;

        if let Some(alt) = &self.alt {
            visitor.visit_stmt(alt)?;
        }

        visitor.visit_expr(&self.cond)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseStmt {
    pub pos: Pos,
    pub body: Vec<Stmt>,
}

impl<E> Visitable<E> for ElseStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_block(&self.body)
    }
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
    pub extends: Vec<String>,
    pub fields: Vec<Field>,
    pub funcs: Vec<FnDeclStmt>,
    pub extern_funcs: Vec<ExternFnDeclStmt>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

impl<E> Visitable<E> for ClassDeclStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&self.name)?;

        for field in self.fields.iter() {
            visitor.visit_name(&field.name)?;
        }

        visitor.visit_fn_decl_stmt_list(&self.funcs)?;
        visitor.visit_extern_fn_decl_stmt_list(&self.extern_funcs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MixinDeclStmt {
    pub name: String,
    pub public: bool,
    pub funcs: Vec<FnDeclStmt>,
    pub extern_funcs: Vec<ExternFnDeclStmt>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

impl<E> Visitable<E> for MixinDeclStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&self.name)?;
        visitor.visit_fn_decl_stmt_list(&self.funcs)?;
        visitor.visit_extern_fn_decl_stmt_list(&self.extern_funcs)
    }
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

impl<E> Visitable<E> for InterfaceDeclStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub name: String,
    pub pos: Pos,
}

impl<E> Visitable<E> for ImportStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleStmt {
    pub name: String,
    pub pos: Pos,
}

impl<E> Visitable<E> for ModuleStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub expr: Option<Expr>,
    pub alias: Option<Variable>,
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

impl<E> Visitable<E> for ForStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        if let Some(expr) = &self.expr {
            visitor.visit_expr(expr)?;
        }

        if let Some(var) = &self.alias {
            visitor.visit_var(var)?;
        }

        visitor.visit_block(&self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStmt {
    pub label: Option<String>,
    pub pos: Pos,
}

impl<E> Visitable<E> for BreakStmt {
    fn walk(&self, _visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnsafeStmt {
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

impl<E> Visitable<E> for UnsafeStmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_block(&self.body)
    }
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
    MixinDecl(MixinDeclStmt),
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
            Stmt::MixinDecl(mixin_decl_stmt) => mixin_decl_stmt.pos.clone(),
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
            Self::MixinDecl(mixin_decl_stmt) => {
                mixin_decl_stmt.comments = comments;
            }
            _ => {}
        }

        self
    }
}

impl<E> Visitable<E> for Stmt {
    fn walk(&self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        match self {
            Self::If(if_stmt) => visitor.visit_if_stmt(if_stmt),
            Self::Else(else_stmt) => visitor.visit_else_stmt(else_stmt),
            Self::For(for_stmt) => visitor.visit_for_stmt(for_stmt),
            Self::Expr(expr_stmt) => visitor.visit_expr_stmt(expr_stmt),
            Self::Let(let_stmt) => visitor.visit_let_stmt(let_stmt),
            Self::Break(break_stmt) => visitor.visit_break_stmt(break_stmt),
            Self::Raise(raise_stmt) => visitor.visit_raise_stmt(raise_stmt),
            Self::LetDecl(let_decl_stmt) => visitor.visit_let_decl_stmt(let_decl_stmt),
            Self::FnDecl(fn_decl_stmt) => visitor.visit_fn_decl_stmt(fn_decl_stmt),
            Self::ExternFnDecl(extern_fn_decl_stmt) => {
                visitor.visit_extern_fn_decl_stmt(extern_fn_decl_stmt)
            }
            Self::Assign(assign_stmt) => visitor.visit_assign_stmt(assign_stmt),
            Self::Return(return_stmt) => visitor.visit_return_stmt(return_stmt),
            Self::Import(import_stmt) => visitor.visit_import_stmt(import_stmt),
            Self::Unsafe(unsafe_stmt) => visitor.visit_unsafe_stmt(unsafe_stmt),
            Self::Module(module_stmt) => visitor.visit_module_stmt(module_stmt),
            Self::ClassDecl(class_decl_stmt) => visitor.visit_class_decl_stmt(class_decl_stmt),
            Self::MixinDecl(mixin_decl_stmt) => visitor.visit_mixin_decl_stmt(mixin_decl_stmt),
            Self::InterfaceDecl(interface_decl_stmt) => {
                visitor.visit_interface_decl_stmt(interface_decl_stmt)
            }
            _ => unreachable!(),
        }
    }
}
