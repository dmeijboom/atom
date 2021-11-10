use atom_ir::Location;

use crate::ast::{ArithmeticOp, ComparisonOp, FnArg, LogicalOp};
use crate::compiler::mir::{Local, ScopeContext};
use crate::compiler::module::Field;
use crate::compiler::FuncArg;

use super::scope::{LocalId, Scope, ScopeId};

#[derive(Debug)]
pub struct Mir {
    pub scopes: Vec<Scope>,
    pub program: Vec<Decl>,
}

impl Mir {
    pub fn get_local<'m>(&'m self, scope: &'m Scope, id: LocalId) -> &'m Local {
        if let Some((_, local)) = scope.locals.iter().find(|(_, local)| local.id == id) {
            return local;
        }

        if let Some(parent) = scope.parent.and_then(|id| self.scopes.get(id)) {
            return self.get_local(parent, id);
        }

        unreachable!("unable to find local with ID: {}", id)
    }

    pub fn get_function_target<'m>(&'m self, scope: &'m Scope) -> Option<&'m str> {
        if let ScopeContext::Function(name) = &scope.context {
            return Some(name);
        }

        if let Some(parent) = scope.parent.and_then(|id| self.scopes.get(id)) {
            return self.get_function_target(parent);
        }

        None
    }
}

#[derive(Debug)]
pub enum Terminator {
    Break,
    Return,
    Raise,
}

#[derive(Debug)]
pub struct Decl {
    pub public: bool,
    pub kind: DeclKind,
}

impl Decl {
    pub fn new(kind: DeclKind, public: bool) -> Self {
        Self { kind, public }
    }
}

#[derive(Debug)]
pub enum DeclKind {
    Class(Class),
    Function(Function),
    Interface(Interface),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub block: Block,
    pub is_extern: bool,
    pub is_closure: bool,
    pub args: Vec<FuncArg>,
}

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub methods: Vec<Function>,
    pub fields: Vec<Field>,
}

#[derive(Debug)]
pub struct Interface {
    pub name: String,
    pub functions: Vec<String>,
}

#[derive(Debug)]
pub struct Block {
    pub loc: Location,
    pub scope_id: ScopeId,
    pub statements: Vec<Stmt>,
    pub terminator: Option<Terminator>,
}

impl Default for Block {
    fn default() -> Self {
        Self {
            loc: Location::default(),
            scope_id: 0,
            statements: vec![],
            terminator: None,
        }
    }
}

impl Block {
    pub fn new(loc: Location, scope_id: ScopeId) -> Self {
        Self {
            loc,
            scope_id,
            terminator: None,
            statements: vec![],
        }
    }

    pub fn with_terminator(loc: Location, scope_id: ScopeId, terminator: Terminator) -> Self {
        Self {
            loc,
            scope_id,
            terminator: Some(terminator),
            statements: vec![],
        }
    }
}

#[derive(Debug)]
pub struct Cond {
    pub condition: Value,
    pub block: Block,
    pub alt: Option<Block>,
}

#[derive(Debug)]
pub struct Stmt {
    pub loc: Location,
    pub kind: StmtKind,
}

impl Stmt {
    pub fn new(loc: Location, kind: StmtKind) -> Self {
        Self { loc, kind }
    }
}

#[derive(Debug)]
pub enum StmtKind {
    Assign(Assign),
    Cond(Cond),
    Loop(Block),
    Eval(Value),
    Return(Value),
}

#[derive(Debug)]
pub enum AssignLeftHand {
    Local(LocalId),
    Index(Box<Index>),
    Member(Member),
}

#[derive(Debug)]
pub struct Assign {
    pub left: AssignLeftHand,
    pub right: Value,
}

impl Assign {
    pub fn new(left: AssignLeftHand, right: Value) -> Self {
        Self { left, right }
    }
}

#[derive(Debug)]
pub struct Cast {
    pub value: Value,
    pub dest: String,
}

#[derive(Debug)]
pub struct Call {
    pub callee: Value,
    pub args: Vec<Value>,
    pub keywords: Vec<String>,
}

impl Call {
    pub fn new(callee: Value) -> Self {
        Self {
            callee,
            args: vec![],
            keywords: vec![],
        }
    }

    pub fn with_args(callee: Value, args: Vec<Value>) -> Self {
        Self {
            callee,
            args,
            keywords: vec![],
        }
    }
}

#[derive(Debug)]
pub struct KeyValuePair {
    pub key: Value,
    pub value: Value,
}

#[derive(Debug)]
pub struct Closure {
    pub args: Vec<FnArg>,
    pub block: Block,
}

#[derive(Debug)]
pub struct Member {
    pub object: Value,
    pub member: String,
}

impl Member {
    pub fn new(object: Value, member: String) -> Self {
        Self { object, member }
    }
}

#[derive(Debug)]
pub struct Operator<T> {
    pub left: Value,
    pub right: Value,
    pub op: T,
}

#[derive(Debug)]
pub struct Index {
    pub object: Value,
    pub index: Value,
}

impl Index {
    pub fn new(object: Value, index: Value) -> Self {
        Self { object, index }
    }
}

#[derive(Debug)]
pub struct Slice {
    pub object: Value,
    pub begin: Value,
    pub end: Value,
}

#[derive(Debug)]
pub struct Range {
    pub begin: Value,
    pub end: Value,
}

#[derive(Debug)]
pub enum TemplateComponent {
    String(String),
    Value(Value),
}

#[derive(Debug)]
pub struct TypeAssert {
    pub left: Value,
    pub right: Value,
}

#[derive(Debug)]
pub enum Const {
    Int128(i128),
    Int64(i64),
    Uint64(u64),
    Int32(i32),
    Byte(u8),
    Float(f64),
    Bool(bool),
    Char(char),
    Symbol(String),
    String(String),
}

#[derive(Debug)]
pub enum ValueKind {
    Const(Const),
    Local(LocalId),
    // Loads a value by name in the following order: imports, fn, class, interface
    Name(String),
    Receiver,
    Cast(Box<Cast>),
    Call(Box<Call>),
    Unwrap(Box<Value>),
    Array(Vec<Value>),
    Tuple(Vec<Value>),
    Closure(Closure),
    Member(Box<Member>),
    Comparison(Box<Operator<ComparisonOp>>),
    Arithmetic(Box<Operator<ArithmeticOp>>),
    Logical(Box<Operator<LogicalOp>>),
    MakeRef(Box<Value>),
    Deref(Box<Value>),
    Index(Box<Index>),
    Slice(Box<Slice>),
    Range(Box<Range>),
    Template(Vec<TemplateComponent>),
    TypeAssert(Box<TypeAssert>),
}

#[derive(Debug)]
pub struct Value {
    pub loc: Location,
    pub kind: ValueKind,
}

impl Value {
    pub fn new(loc: Location, kind: ValueKind) -> Self {
        Self { loc, kind }
    }
}
