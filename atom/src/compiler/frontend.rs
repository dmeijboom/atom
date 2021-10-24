use std::collections::HashMap;

use indexmap::IndexMap;

use crate::ast::{
    ClassDeclStmt, Expr, ExternFnDeclStmt, FnArg, FnDeclStmt, InterfaceDeclStmt, Literal, Pos,
    Stmt, TemplateComponent, Variable,
};
use crate::compiler::LineNumberOffset;

use super::module::{Class, Field, Func, FuncArg, Interface, Module};
use super::result::{CompileError, Result};
use super::scope::{ForLoopMeta, Scope, ScopeContext, ScopeGraph};
use super::types::{self, MapType, PrimitiveType, Type};

macro_rules! expect {
    ($actual:expr, $expected:pat, $message:expr) => {
        if $actual.is_typed() {
            if !matches!($actual, $expected) {
                return Err(CompileError::new($message));
            }
        }
    };
}

fn is_same_type(types: &[Type]) -> bool {
    if types.len() <= 1 {
        return true;
    }

    let first = types.first();

    types.iter().skip(1).all(|t| first == Some(t))
}

fn parse_type(types: &mut Vec<Type>) -> Type {
    if !types.is_empty() && is_same_type(types) {
        return types.remove(0);
    }

    Type::Unknown
}

fn clone_fn_args(args: &[FnArg]) -> Vec<FuncArg> {
    args.iter()
        .map(|arg| FuncArg::new(arg.name.clone(), arg.mutable))
        .collect()
}

/// FrontendCompiler implements a basic and (optional)type-checking compiler
pub struct FrontendCompiler<'c> {
    pos: Pos,
    scope: ScopeGraph,
    scopes: Vec<Scope>,
    module: &'c mut Module,
    return_types: Vec<Type>,
    line_number_offset: &'c LineNumberOffset,
}

impl<'c> FrontendCompiler<'c> {
    pub fn new(module: &'c mut Module, line_number_offset: &'c LineNumberOffset) -> Self {
        Self {
            module,
            pos: 0..0,
            scopes: vec![],
            return_types: vec![],
            scope: ScopeGraph::new(),
            line_number_offset,
        }
    }

    fn enter_scope(&mut self, context: ScopeContext) {
        let new_scope = Scope::new_child(self.scope.current(), context);

        self.scope.push(new_scope);
    }

    fn exit_scope(&mut self) {
        if let Some(scope) = self.scope.pop() {
            self.scopes.push(scope);
        }
    }

    fn collect_return_type(&mut self) -> Option<Type> {
        if self.return_types.is_empty() {
            return None;
        }
        let mut return_types = self.return_types.drain(..).collect::<Vec<_>>();

        Some(if !return_types.is_empty() && is_same_type(&return_types) {
            return_types.remove(0)
        } else {
            Type::Unknown
        })
    }

    fn compile_items(&mut self, items: &[Expr]) -> Result<Vec<Type>> {
        let mut item_types = vec![];

        for item in items.iter() {
            item_types.push(self.compile_expr(item)?);
        }

        Ok(item_types)
    }

    fn compile_var(&mut self, variable: &Variable, item_type: Type, mutable: bool) -> Result<()> {
        match variable {
            Variable::Name(name) => {
                if self.scope.get_local(name, false).is_some() {
                    return Err(CompileError::new(format!(
                        "unable to redefine name: {}",
                        name
                    )));
                }

                self.scope.set_local(name.clone(), mutable, item_type)?;
            }
            Variable::Tuple(names) | Variable::Array(names) => {
                for name in names {
                    if self.scope.get_local(name, false).is_some() {
                        return Err(CompileError::new(format!(
                            "unable to redefine name: {}",
                            name
                        )));
                    }

                    self.scope.set_local(name.clone(), mutable, Type::Unknown)?;
                }
            }
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Type> {
        self.pos = expr.pos();

        Ok(match expr {
            Expr::Literal(literal_expr) => Type::Primitive(match literal_expr.literal {
                Literal::Int128(_) | Literal::Int64(_) | Literal::Uint64(_) | Literal::Int32(_) => {
                    PrimitiveType::Int
                }
                Literal::Byte(_) => PrimitiveType::Byte,
                Literal::Float(_) => PrimitiveType::Float,
                Literal::Bool(_) => PrimitiveType::Bool,
                Literal::Char(_) => PrimitiveType::Char,
                Literal::Symbol(_) => PrimitiveType::Symbol,
                Literal::String(_) => PrimitiveType::String,
            }),
            Expr::Ident(ident) => {
                if let Some(local) = self.scope.get_local(&ident.name, true) {
                    local.known_type.clone()
                } else if let Some((_, _, import)) = self.module.imports.get_full(&ident.name) {
                    import.known_type.clone()
                } else if let Some(func) = self.module.funcs.get(&ident.name) {
                    Type::Fn(func.name.clone())
                } else if let Some(class) = self.module.classes.get(&ident.name) {
                    Type::Class(class.name.clone())
                } else if let Some(interface) = self.module.interfaces.get(&ident.name) {
                    Type::Interface(interface.name.clone())
                } else {
                    return Err(CompileError::new(format!("no such name: {}", ident.name)));
                }
            }
            Expr::Call(call) => {
                let mut arg_types = vec![];

                for arg in call.args.iter() {
                    arg_types.push(self.compile_expr(arg)?);
                }

                let callee_type = self.compile_expr(&call.callee)?;

                expect!(
                    callee_type,
                    Type::Fn(_) | Type::Class(_),
                    format!("unable to call: {}", callee_type)
                );

                Type::Unknown
            }
            Expr::Cast(_) => Type::Unknown,
            Expr::Not(not) => {
                let value_type = self.compile_expr(&not.expr)?;

                expect!(
                    value_type,
                    types::BOOL,
                    format!("unable to inverse invalid type: {}", value_type)
                );

                types::BOOL.clone()
            }
            Expr::Unwrap(unwrap) => {
                let value_type = self.compile_expr(&unwrap.expr)?;

                expect!(
                    value_type,
                    Type::Option(_),
                    format!("unable to unwrap: {}", value_type)
                );

                if let Type::Option(inner_type) = value_type {
                    *inner_type
                } else {
                    Type::Unknown
                }
            }
            Expr::Array(array) => {
                let mut item_types = self.compile_items(&array.items)?;
                Type::Array(Box::new(parse_type(&mut item_types)))
            }
            Expr::Tuple(tuple) => Type::Tuple(self.compile_items(&tuple.items)?),
            Expr::Map(map) => {
                let mut key_types = vec![];
                let mut value_types = vec![];

                for key_value in map.key_values.iter() {
                    key_types.push(self.compile_expr(&key_value.key)?);
                    value_types.push(self.compile_expr(&key_value.value)?);
                }

                let key_type = parse_type(&mut key_types);
                let value_type = parse_type(&mut value_types);

                Type::Map(Box::new(MapType::new(key_type, value_type)))
            }
            Expr::Closure(_) => unreachable!("not implemented yet"),
            Expr::Member(member) => {
                self.compile_expr(&member.object)?;

                // @TODO: after implementing references to the original class, add extra checks here
                Type::Unknown
            }
            Expr::MemberCond(_) => unreachable!("not implemented yet"),
            Expr::Arithmetic(arithmetic) => {
                let left = self.compile_expr(&arithmetic.left)?;
                let right = self.compile_expr(&arithmetic.right)?;

                expect!(
                    right,
                    Type::Primitive(PrimitiveType::Int | PrimitiveType::Float),
                    format!("unable to {} {}", arithmetic.op.description(), right)
                );

                left
            }
            Expr::Comparison(comparison) => {
                self.compile_expr(&comparison.left)?;
                self.compile_expr(&comparison.right)?;

                types::BOOL.clone()
            }
            Expr::Logical(logical) => {
                self.compile_expr(&logical.left)?;
                self.compile_expr(&logical.right)?;

                types::BOOL.clone()
            }
            Expr::MakeRef(ref_expr) => {
                let inner_type = self.compile_expr(&ref_expr.expr)?;

                Type::Ref(Box::new(inner_type))
            }
            Expr::Deref(deref) => {
                let deref_type = self.compile_expr(&deref.expr)?;

                expect!(
                    deref_type,
                    Type::Ref(_),
                    format!("unable to deref: {}", deref_type)
                );

                if let Type::Ref(inner_type) = deref_type {
                    Type::Ref(inner_type)
                } else {
                    Type::Ref(Box::new(deref_type))
                }
            }
            Expr::Index(index) => {
                let array_type = self.compile_expr(&index.object)?;
                let index_type = self.compile_expr(&index.index)?;

                expect!(
                    array_type,
                    Type::Array(_),
                    format!("unable to index: {}", array_type)
                );

                if let Expr::Range(range) = &index.index {
                    self.compile_expr(&Expr::Range(range.clone()))?;

                    array_type
                } else {
                    expect!(
                        index_type,
                        types::INT,
                        format!("unable to use type '{}' as index", index_type)
                    );

                    if let Type::Array(inner_type) = array_type {
                        *inner_type
                    } else {
                        Type::Unknown
                    }
                }
            }
            Expr::Range(range) => {
                let from = self.compile_expr(&range.from)?;
                let to = self.compile_expr(&range.to)?;

                expect!(
                    from,
                    types::INT,
                    format!("unable to use type '{}..{}' as index", from, to)
                );

                expect!(
                    to,
                    types::INT,
                    format!("unable to use type '{}..{}' as index", from, to)
                );

                Type::Unknown
            }
            Expr::Template(template) => {
                for component in template.components.iter() {
                    if let TemplateComponent::Expr(expr) = component {
                        self.compile_expr(expr)?;
                    }
                }

                types::STRING.clone()
            }
            Expr::TypeAssert(type_assert) => {
                self.compile_expr(&type_assert.left)?;
                self.compile_expr(&type_assert.right)?;

                types::BOOL.clone()
            }
        })
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        self.pos = stmt.pos();

        match stmt {
            Stmt::If(if_stmt) => {
                self.enter_scope(ScopeContext::IfElse);

                let cond = self.compile_expr(&if_stmt.cond)?;

                expect!(
                    cond,
                    types::BOOL,
                    "invalid type for if-statement condition".to_string()
                );

                self.compile_stmt_list(&if_stmt.body)?;

                if let Some(alt) = &if_stmt.alt {
                    self.compile_stmt(alt)?;
                }

                self.exit_scope();
            }
            Stmt::Else(else_stmt) => {
                self.enter_scope(ScopeContext::IfElse);
                self.compile_stmt_list(&else_stmt.body)?;
                self.exit_scope();
            }
            Stmt::For(for_stmt) => {
                self.enter_scope(ScopeContext::ForLoop(ForLoopMeta::default()));

                self.scope.set_local(
                    "__iter__".to_string(),
                    false,
                    Type::Interface("Iterable".to_string()),
                )?;

                if let Some(expr) = &for_stmt.expr {
                    let iter_type = self.compile_expr(expr)?;

                    expect!(
                        iter_type,
                        Type::Array(_) | Type::Object(_),
                        format!("invalid type '{}' in for loop", iter_type)
                    );

                    self.scope
                        .set_local("__item__".to_string(), false, Type::Unknown)?;

                    if let Some(var) = &for_stmt.alias {
                        let item_type = if let Type::Array(inner_type) = iter_type {
                            *inner_type
                        } else {
                            Type::Unknown
                        };

                        self.compile_var(var, item_type, false)?;
                    }
                }

                self.compile_stmt_list(&for_stmt.body)?;
                self.exit_scope();
            }
            Stmt::Expr(expr_stmt) => {
                self.compile_expr(&expr_stmt.expr)?;
            }
            Stmt::Let(let_stmt) => {
                let value_type = self.compile_expr(&let_stmt.value)?;
                self.compile_var(&let_stmt.var, value_type, let_stmt.mutable)?;
            }
            Stmt::Assign(assign) => {
                let right = assign.expand();

                match &assign.left {
                    Expr::Index(index_expr) => {
                        self.compile_expr(&Expr::Index(index_expr.clone()))?;
                        self.compile_expr(&right)?;
                    }
                    Expr::Ident(ident_expr) => {
                        let local_type =
                            if let Some(local) = self.scope.get_local(&ident_expr.name, true) {
                                if !local.mutable {
                                    return Err(CompileError::new(format!(
                                        "name is not mutable: {}",
                                        ident_expr.name,
                                    )));
                                }

                                local.known_type.clone()
                            } else {
                                return Err(CompileError::new(format!(
                                    "unable to assign value to unknown name: {}",
                                    ident_expr.name
                                )));
                            };

                        let value_type = self.compile_expr(&right)?;

                        if matches!(local_type, Type::Option(_)) {
                            expect!(
                                value_type,
                                Type::Option(_),
                                format!(
                                    "unable to assign non-optional value to name: {}",
                                    ident_expr.name
                                )
                            );
                        }

                        if matches!(value_type, Type::Option(_)) {
                            expect!(
                                local_type,
                                Type::Option(_),
                                format!(
                                    "unable to assign optional value to name: {}",
                                    ident_expr.name
                                )
                            );
                        }

                        self.scope
                            .get_local_mut(&ident_expr.name, true)
                            .unwrap()
                            .known_type = value_type;
                    }
                    Expr::Member(member_expr) => {
                        self.compile_expr(&member_expr.object)?;
                        self.compile_expr(&right)?;
                    }
                    _ => {
                        return Err(CompileError::new(
                            "invalid left-hand in assignment".to_string(),
                        ))
                    }
                }
            }
            Stmt::Return(return_stmt) => {
                let return_type = self.compile_expr(&return_stmt.expr)?;
                self.return_types.push(return_type);
            }
            Stmt::Unsafe(unsafe_stmt) => {
                self.enter_scope(ScopeContext::Unsafe);
                self.compile_stmt_list(&unsafe_stmt.body)?;
                self.exit_scope();
            }
            Stmt::Raise(raise) => {
                self.compile_expr(&raise.expr)?;
            }
            Stmt::LetDecl(_)
            | Stmt::Break(_)
            | Stmt::Import(_)
            | Stmt::FnDecl(_)
            | Stmt::Module(_)
            | Stmt::ExternFnDecl(_)
            | Stmt::ClassDecl(_)
            | Stmt::MixinDecl(_)
            | Stmt::InterfaceDecl(_) => {}
        }

        Ok(())
    }

    fn compile_stmt_list(&mut self, stmt_list: &[Stmt]) -> Result<()> {
        for stmt in stmt_list {
            self.compile_stmt(stmt)?;
        }

        Ok(())
    }

    fn compile_fn(&mut self, fn_decl: &FnDeclStmt, class_name: Option<&str>) -> Result<Func> {
        if fn_decl.public {
            self.module
                .exports
                .insert(fn_decl.name.clone(), Type::Fn(fn_decl.name.clone()));
        }

        self.enter_scope(ScopeContext::Function(fn_decl.name.clone()));

        if let Some(class_name) = class_name {
            self.scope.set_local(
                "this".to_string(),
                true,
                match class_name {
                    "Option" if self.module.name == "std.core" => {
                        Type::Option(Box::new(Type::Unknown))
                    }
                    "Array" if self.module.name == "std.core" => {
                        Type::Array(Box::new(Type::Unknown))
                    }
                    _ => Type::Object(class_name.to_string()),
                },
            )?;
        }

        // Register locals for input arguments
        for arg in fn_decl.args.iter() {
            self.scope
                .set_local(arg.name.clone(), arg.mutable, Type::Unknown)?;
        }

        self.compile_stmt_list(&fn_decl.body)?;
        self.exit_scope();

        let return_type = self.collect_return_type();

        Ok(Func {
            name: fn_decl.name.to_string(),
            args: clone_fn_args(&fn_decl.args),
            return_type,
            ..Func::default()
        })
    }

    fn compile_extern_fn(&mut self, extern_fn_decl: &ExternFnDeclStmt) -> Result<Func> {
        if extern_fn_decl.public {
            self.module.exports.insert(
                extern_fn_decl.name.clone(),
                Type::Fn(extern_fn_decl.name.clone()),
            );
        }

        Ok(Func {
            name: extern_fn_decl.name.to_string(),
            args: clone_fn_args(&extern_fn_decl.args),
            is_extern: true,
            ..Func::default()
        })
    }

    fn compile_interface(&mut self, interface_decl: &InterfaceDeclStmt) {
        if interface_decl.public {
            self.module.exports.insert(
                interface_decl.name.clone(),
                Type::Interface(interface_decl.name.clone()),
            );
        }

        self.module.interfaces.insert(
            interface_decl.name.clone(),
            Interface {
                name: interface_decl.name.clone(),
                functions: interface_decl
                    .functions
                    .iter()
                    .map(|func| func.name.clone())
                    .collect(),
                ..Interface::default()
            },
        );
    }

    fn compile_class(&mut self, class_decl: &ClassDeclStmt) -> Result<Class> {
        if class_decl.public {
            self.module.exports.insert(
                class_decl.name.clone(),
                Type::Class(class_decl.name.clone()),
            );
        }

        self.enter_scope(ScopeContext::Class(class_decl.name.clone()));

        let mut fields = IndexMap::new();

        for field in class_decl.fields.iter() {
            fields.insert(field.name.clone(), Field::new(field.mutable, field.public));
        }

        let mut methods = HashMap::new();

        for extern_fn_decl in class_decl.extern_funcs.iter() {
            methods.insert(
                extern_fn_decl.name.clone(),
                self.compile_extern_fn(extern_fn_decl)?,
            );
        }

        for fn_decl in class_decl.funcs.iter() {
            methods.insert(
                fn_decl.name.clone(),
                self.compile_fn(fn_decl, Some(&class_decl.name))?,
            );
        }

        self.exit_scope();

        Ok(Class {
            name: class_decl.name.clone(),
            fields,
            methods,
            ..Class::default()
        })
    }

    fn compile_all(&mut self, tree: &[Stmt]) -> Result<()> {
        for stmt in tree {
            match stmt {
                Stmt::FnDecl(fn_decl) => {
                    let compiled = self.compile_fn(fn_decl, None)?;
                    let func = self.module.funcs.get_mut(&fn_decl.name).unwrap();

                    *func = compiled;
                }
                Stmt::ExternFnDecl(extern_fn_decl) => {
                    let compiled = self.compile_extern_fn(extern_fn_decl)?;
                    let func = self.module.funcs.get_mut(&extern_fn_decl.name).unwrap();

                    *func = compiled;
                }
                Stmt::ClassDecl(class_decl) => {
                    let compiled = self.compile_class(class_decl)?;
                    let class = self.module.classes.get_mut(&class_decl.name).unwrap();

                    *class = compiled;
                }

                // No need to process interfaces any further
                Stmt::InterfaceDecl(_) => {}
                _ => unreachable!(),
            }
        }

        Ok(())
    }

    pub fn compile(mut self, tree: &[Stmt]) -> Result<Vec<Scope>> {
        // Register global types early
        for stmt in tree {
            match stmt {
                Stmt::FnDecl(fn_decl) => {
                    self.module.funcs.insert(
                        fn_decl.name.clone(),
                        Func {
                            name: fn_decl.name.clone(),
                            ..Func::default()
                        },
                    );
                }
                Stmt::ExternFnDecl(extern_fn_decl) => {
                    self.module.funcs.insert(
                        extern_fn_decl.name.clone(),
                        Func {
                            name: extern_fn_decl.name.clone(),
                            is_extern: true,
                            ..Func::default()
                        },
                    );
                }
                Stmt::ClassDecl(class_decl) => {
                    self.module.classes.insert(
                        class_decl.name.clone(),
                        Class {
                            name: class_decl.name.clone(),
                            ..Class::default()
                        },
                    );
                }
                Stmt::InterfaceDecl(interface_decl) => {
                    self.compile_interface(interface_decl);
                }
                _ => unreachable!(),
            }
        }

        self.compile_all(tree)
            .map_err(|e| e.with_location(self.line_number_offset.get_location(&self.pos)))?;

        self.exit_scope();
        self.scopes.sort_unstable_by_key(|scope| scope.id);

        Ok(self.scopes)
    }
}
