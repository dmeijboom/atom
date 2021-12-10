use std::ops::Range;

use enumflags2::{bitflags, BitFlags};

use crate::compiler::ir::Code;
use crate::syntax::visitor::Visitable;
use crate::syntax::Visitor;

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

impl<E> Visitable<E> for Variable {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        match self {
            Variable::Name(name) => visitor.visit_name(name),
            Variable::Tuple(names) | Variable::Array(names) => {
                for name in names.iter_mut() {
                    visitor.visit_name(name)?;
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComparisonOp {
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Neq,
}

impl From<ComparisonOp> for Code {
    fn from(op: ComparisonOp) -> Self {
        match op {
            ComparisonOp::Lt => Code::ComparisonLt,
            ComparisonOp::Lte => Code::ComparisonLte,
            ComparisonOp::Gt => Code::ComparisonGt,
            ComparisonOp::Gte => Code::ComparisonGte,
            ComparisonOp::Eq => Code::ComparisonEq,
            ComparisonOp::Neq => Code::ComparisonNeq,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ArithmeticOp {
    Mul,
    Div,
    Add,
    Sub,
    Mod,
    Exp,
    BitAnd,
    BitOr,
    BitXor,
    BitShiftLeft,
    BitShiftRight,
}

impl From<ArithmeticOp> for Code {
    fn from(op: ArithmeticOp) -> Self {
        match op {
            ArithmeticOp::Mul => Code::ArithmeticMul,
            ArithmeticOp::Div => Code::ArithmeticDiv,
            ArithmeticOp::Add => Code::ArithmeticAdd,
            ArithmeticOp::Sub => Code::ArithmeticSub,
            ArithmeticOp::Mod => Code::ArithmeticMod,
            ArithmeticOp::Exp => Code::ArithmeticExp,
            ArithmeticOp::BitAnd => Code::ArithmeticBitAnd,
            ArithmeticOp::BitOr => Code::ArithmeticBitOr,
            ArithmeticOp::BitXor => Code::ArithmeticBitXor,
            ArithmeticOp::BitShiftLeft => Code::ArithmeticBitShiftLeft,
            ArithmeticOp::BitShiftRight => Code::ArithmeticBitShiftRight,
        }
    }
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

impl<E> Visitable<E> for TypeAssertExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.left)?;
        visitor.visit_expr(&mut self.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComparisonExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ComparisonOp,
    pub pos: Pos,
}

impl<E> Visitable<E> for ComparisonExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.left)?;
        visitor.visit_expr(&mut self.right)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Expr,
    pub args: Vec<Expr>,
    pub pos: Pos,
}

impl<E> Visitable<E> for CallExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.callee)?;

        for arg in self.args.iter_mut() {
            visitor.visit_expr(arg)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NewExpr {
    pub callee: Expr,
    pub args: Vec<KeywordArg>,
    pub pos: Pos,
}

impl<E> Visitable<E> for NewExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.callee)?;

        for arg in self.args.iter_mut() {
            visitor.visit_keyword_arg(arg)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CastExpr {
    pub type_name: String,
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for CastExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)?;
        visitor.visit_name(&mut self.type_name)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArithmeticExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: ArithmeticOp,
    pub pos: Pos,
}

impl<E> Visitable<E> for ArithmeticExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.left)?;
        visitor.visit_expr(&mut self.right)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr {
    pub left: Expr,
    pub right: Expr,
    pub op: LogicalOp,
    pub pos: Pos,
}

impl<E> Visitable<E> for LogicalExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.left)?;
        visitor.visit_expr(&mut self.right)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentExpr {
    pub name: String,
    pub pos: Pos,
}

impl<E> Visitable<E> for IdentExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TryExpr {
    pub expr: Box<Expr>,
    pub pos: Pos,
}

impl<E> Visitable<E> for TryExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(self.expr.as_mut())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NotExpr {
    pub expr: Box<Expr>,
    pub pos: Pos,
}

impl<E> Visitable<E> for NotExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayExpr {
    pub items: Vec<Expr>,
    pub pos: Pos,
}

impl<E> Visitable<E> for ArrayExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        for item in self.items.iter_mut() {
            visitor.visit_expr(item)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TupleExpr {
    pub items: Vec<Expr>,
    pub pos: Pos,
}

impl<E> Visitable<E> for TupleExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        for item in self.items.iter_mut() {
            visitor.visit_expr(item)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeywordArg {
    pub name: String,
    pub value: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for KeywordArg {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)?;
        visitor.visit_expr(&mut self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValue {
    pub key: Expr,
    pub value: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for KeyValue {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.key)?;
        visitor.visit_expr(&mut self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapExpr {
    pub key_values: Vec<KeyValue>,
    pub pos: Pos,
}

impl<E> Visitable<E> for MapExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        for key_value in self.key_values.iter_mut() {
            visitor.visit_key_value(key_value)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Uint(u64),
    Byte(u8),
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

impl<E> Visitable<E> for LiteralExpr {
    fn accept(&mut self, _visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberExpr {
    pub object: Expr,
    pub member: String,
    pub pos: Pos,
}

impl<E> Visitable<E> for MemberExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.object)?;
        visitor.visit_name(&mut self.member)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub object: Expr,
    pub index: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for IndexExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.object)?;
        visitor.visit_expr(&mut self.index)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RangeExpr {
    pub from: Expr,
    pub to: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for RangeExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.from)?;
        visitor.visit_expr(&mut self.to)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MakeRefExpr {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for MakeRefExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DerefExpr {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for DerefExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)
    }
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

impl<E> Visitable<E> for TemplateExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        for component in self.components.iter_mut() {
            match component {
                // @TODO: call `self.visit_string()` here?
                TemplateComponent::String(_) => continue,
                TemplateComponent::Expr(e) => visitor.visit_expr(e)?,
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureExpr {
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

impl<E> Visitable<E> for ClosureExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        for arg in self.args.iter_mut() {
            visitor.visit_fn_arg(arg)?;
        }

        visitor.visit_stmt_list(&mut self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeOfExpr {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for TypeOfExpr {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(LiteralExpr),
    Ident(IdentExpr),
    New(Box<NewExpr>),
    Call(Box<CallExpr>),
    Cast(Box<CastExpr>),
    Not(NotExpr),
    Try(TryExpr),
    Array(ArrayExpr),
    Tuple(TupleExpr),
    Map(MapExpr),
    TypeOf(Box<TypeOfExpr>),
    Closure(ClosureExpr),
    Member(Box<MemberExpr>),
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
            Self::New(new_expr) => new_expr.pos.clone(),
            Self::Call(call_expr) => call_expr.pos.clone(),
            Self::Not(not_expr) => not_expr.pos.clone(),
            Self::Try(try_expr) => try_expr.pos.clone(),
            Self::Array(array_expr) => array_expr.pos.clone(),
            Self::Tuple(tuple_expr) => tuple_expr.pos.clone(),
            Self::Map(map_expr) => map_expr.pos.clone(),
            Self::TypeOf(typeof_expr) => typeof_expr.pos.clone(),
            Self::Closure(closure_expr) => closure_expr.pos.clone(),
            Self::Member(member_expr) => member_expr.pos.clone(),
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
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        match self {
            Expr::Literal(literal_expr) => visitor.visit_literal_expr(literal_expr),
            Expr::Ident(ident_expr) => visitor.visit_ident_expr(ident_expr),
            Expr::New(new_expr) => visitor.visit_new_expr(new_expr),
            Expr::Call(call_expr) => visitor.visit_call_expr(call_expr),
            Expr::Cast(cast_expr) => visitor.visit_cast_expr(cast_expr),
            Expr::Not(not_expr) => visitor.visit_not_expr(not_expr),
            Expr::Try(try_expr) => visitor.visit_try_expr(try_expr),
            Expr::Array(array_expr) => visitor.visit_array_expr(array_expr),
            Expr::Tuple(tuple_expr) => visitor.visit_tuple_expr(tuple_expr),
            Expr::Map(map_expr) => visitor.visit_map_expr(map_expr),
            Expr::TypeOf(typeof_expr) => visitor.visit_typeof_expr(typeof_expr),
            Expr::Closure(closure_expr) => visitor.visit_closure_expr(closure_expr),
            Expr::Member(member_expr) => visitor.visit_member_expr(member_expr),
            Expr::Arithmetic(arithmetic_expr) => visitor.visit_arithmetic_expr(arithmetic_expr),
            Expr::Comparison(comparison_expr) => visitor.visit_comparison_expr(comparison_expr),
            Expr::Logical(logical_expr) => visitor.visit_logical_expr(logical_expr),
            Expr::MakeRef(make_ref_expr) => visitor.visit_make_ref_expr(make_ref_expr),
            Expr::Deref(deref_expr) => visitor.visit_deref_expr(deref_expr),
            Expr::Index(index_expr) => visitor.visit_index_expr(index_expr),
            Expr::Range(range_expr) => visitor.visit_range_expr(range_expr),
            Expr::Template(template_expr) => visitor.visit_template_expr(template_expr),
            Expr::TypeAssert(type_assert_expr) => visitor.visit_type_assert_expr(type_assert_expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for ExprStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RaiseStmt {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for RaiseStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)
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
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_var(&mut self.var)?;
        visitor.visit_expr(&mut self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclStmt {
    pub name: String,
    pub pos: Pos,
}

impl<E> Visitable<E> for LetDeclStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg {
    pub name: String,
    pub mutable: bool,
    pub pos: Pos,
}

impl<E> Visitable<E> for FnArg {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)
    }
}

#[bitflags]
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Modifier {
    Public,
    Static,
    Extern,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDeclStmt {
    pub name: String,
    pub modifiers: BitFlags<Modifier>,
    pub args: Vec<FnArg>,
    pub body: Vec<Stmt>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

impl<E> Visitable<E> for FnDeclStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)?;

        for arg in self.args.iter_mut() {
            visitor.visit_fn_arg(arg)?;
        }

        visitor.visit_stmt_list(&mut self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt {
    pub left: Expr,
    pub right: Expr,
    pub op: Option<AssignOp>,
    pub pos: Pos,
}

impl AssignStmt {
    pub fn expand(&self) -> Expr {
        if let Some(op) = &self.op {
            Expr::Arithmetic(Box::new(ArithmeticExpr {
                left: self.left.clone(),
                right: self.right.clone(),
                op: match op {
                    AssignOp::Add => ArithmeticOp::Add,
                    AssignOp::Sub => ArithmeticOp::Sub,
                    AssignOp::Mul => ArithmeticOp::Mul,
                    AssignOp::Div => ArithmeticOp::Div,
                },
                pos: self.pos.clone(),
            }))
        } else {
            self.right.clone()
        }
    }
}

impl<E> Visitable<E> for AssignStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.left)?;
        visitor.visit_expr(&mut self.right)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub expr: Expr,
    pub pos: Pos,
}

impl<E> Visitable<E> for ReturnStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)
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
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.cond)?;
        visitor.visit_stmt_list(&mut self.body)?;

        if let Some(alt) = self.alt.as_mut() {
            visitor.visit_stmt(alt)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseStmt {
    pub pos: Pos,
    pub body: Vec<Stmt>,
}

impl<E> Visitable<E> for ElseStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_stmt_list(&mut self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: Expr,
    pub body: Vec<Stmt>,
    pub pos: Pos,
}

impl<E> Visitable<E> for MatchCase {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.pattern)?;
        visitor.visit_stmt_list(&mut self.body)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchStmt {
    pub expr: Expr,
    pub cases: Vec<MatchCase>,
    pub alt: Option<Vec<Stmt>>,
    pub pos: Pos,
}

impl<E> Visitable<E> for MatchStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_expr(&mut self.expr)?;

        for case in self.cases.iter_mut() {
            visitor.visit_match_case(case)?;
        }

        if let Some(alt) = self.alt.as_mut() {
            visitor.visit_stmt_list(alt)?;
        }

        Ok(())
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

impl<E> Visitable<E> for Field {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)?;

        if let Some(value) = self.value.as_mut() {
            visitor.visit_expr(value)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassDeclStmt {
    pub name: String,
    pub public: bool,
    pub extends: Vec<String>,
    pub fields: Vec<Field>,
    pub funcs: Vec<FnDeclStmt>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

impl<E> Visitable<E> for ClassDeclStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)?;

        for field in self.fields.iter_mut() {
            visitor.visit_class_field(field)?;
        }

        for fn_decl_stmt in self.funcs.iter_mut() {
            visitor.visit_fn_decl_stmt(fn_decl_stmt)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MixinDeclStmt {
    pub name: String,
    pub public: bool,
    pub funcs: Vec<FnDeclStmt>,
    pub comments: Vec<Comment>,
    pub pos: Pos,
}

impl<E> Visitable<E> for MixinDeclStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)?;

        for fn_decl_stmt in self.funcs.iter_mut() {
            visitor.visit_fn_decl_stmt(fn_decl_stmt)?;
        }

        Ok(())
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
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportStmt {
    pub path: Vec<String>,
    pub pos: Pos,
}

impl<E> Visitable<E> for ImportStmt {
    fn accept(&mut self, _visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleStmt {
    pub name: String,
    pub pos: Pos,
}

impl<E> Visitable<E> for ModuleStmt {
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        visitor.visit_name(&mut self.name)
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
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        if let Some(alias) = self.alias.as_mut() {
            visitor.visit_var(alias)?;
        }

        if let Some(expr) = self.expr.as_mut() {
            visitor.visit_expr(expr)?;
        }

        visitor.visit_stmt_list(&mut self.body)?;

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStmt {
    pub label: Option<String>,
    pub pos: Pos,
}

impl<E> Visitable<E> for BreakStmt {
    fn accept(&mut self, _visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    If(IfStmt),
    Else(ElseStmt),
    Match(MatchStmt),
    For(ForStmt),
    Expr(ExprStmt),
    Let(LetStmt),
    Break(BreakStmt),
    Raise(RaiseStmt),
    LetDecl(LetDeclStmt),
    FnDecl(FnDeclStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    Import(ImportStmt),
    Module(ModuleStmt),
    ClassDecl(ClassDeclStmt),
    MixinDecl(MixinDeclStmt),
    InterfaceDecl(InterfaceDeclStmt),
}

impl Stmt {
    pub fn pos(&self) -> Pos {
        match self {
            Self::If(if_stmt) => if_stmt.pos.clone(),
            Self::Else(else_stmt) => else_stmt.pos.clone(),
            Self::Match(match_stmt) => match_stmt.pos.clone(),
            Self::For(for_stmt) => for_stmt.pos.clone(),
            Self::Break(break_stmt) => break_stmt.pos.clone(),
            Self::Raise(raise_stmt) => raise_stmt.pos.clone(),
            Self::Expr(expr_stmt) => expr_stmt.pos.clone(),
            Self::Let(let_stmt) => let_stmt.pos.clone(),
            Self::LetDecl(let_decl_stmt) => let_decl_stmt.pos.clone(),
            Self::FnDecl(fn_decl_stmt) => fn_decl_stmt.pos.clone(),
            Self::Assign(assign_stmt) => assign_stmt.pos.clone(),
            Self::Return(return_stmt) => return_stmt.pos.clone(),
            Self::Module(module_stmt) => module_stmt.pos.clone(),
            Self::Import(import_stmt) => import_stmt.pos.clone(),
            Self::ClassDecl(class_decl_stmt) => class_decl_stmt.pos.clone(),
            Self::MixinDecl(mixin_decl_stmt) => mixin_decl_stmt.pos.clone(),
            Self::InterfaceDecl(interface_decl_stmt) => interface_decl_stmt.pos.clone(),
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
    fn accept(&mut self, visitor: &mut impl Visitor<Error = E>) -> Result<(), E> {
        match self {
            Stmt::If(if_stmt) => visitor.visit_if_stmt(if_stmt),
            Stmt::Else(else_stmt) => visitor.visit_else_stmt(else_stmt),
            Stmt::Match(match_stmt) => visitor.visit_match_stmt(match_stmt),
            Stmt::For(for_stmt) => visitor.visit_for_stmt(for_stmt),
            Stmt::Expr(expr_stmt) => visitor.visit_expr_stmt(expr_stmt),
            Stmt::Let(let_stmt) => visitor.visit_let_stmt(let_stmt),
            Stmt::Break(break_stmt) => visitor.visit_break_stmt(break_stmt),
            Stmt::Raise(raise_stmt) => visitor.visit_raise_stmt(raise_stmt),
            Stmt::LetDecl(let_decl_stmt) => visitor.visit_let_decl_stmt(let_decl_stmt),
            Stmt::FnDecl(fn_decl_stmt) => visitor.visit_fn_decl_stmt(fn_decl_stmt),
            Stmt::Assign(assign_stmt) => visitor.visit_assign_stmt(assign_stmt),
            Stmt::Return(return_stmt) => visitor.visit_return_stmt(return_stmt),
            Stmt::Import(import_stmt) => visitor.visit_import_stmt(import_stmt),
            Stmt::Module(module_stmt) => visitor.visit_module_stmt(module_stmt),
            Stmt::ClassDecl(class_decl_stmt) => visitor.visit_class_decl_stmt(class_decl_stmt),
            Stmt::MixinDecl(mixin_decl_stmt) => visitor.visit_mixin_decl_stmt(mixin_decl_stmt),
            Stmt::InterfaceDecl(interface_decl_stmt) => {
                visitor.visit_interface_decl_stmt(interface_decl_stmt)
            }
        }
    }
}
