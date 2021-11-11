use crate::compiler::ir::Location;

use crate::ast::{
    ArithmeticOp, ClassDeclStmt, ComparisonOp, Expr, FnArg, FnDeclStmt, IfStmt, InterfaceDeclStmt,
    Literal, LogicalOp, Stmt, TemplateComponent, Variable,
};
use crate::compiler::module::Field;
use crate::compiler::result::{CompileError, Result};
use crate::compiler::slugs::Slugs;
use crate::compiler::{FuncArg, LineNumberOffset};

use super::scope::{ForLoopMeta, LocalId, Scope, ScopeContext, ScopeGraph, ScopeId};
use super::types::{self, *};

fn map_fn_args(args: &[FnArg]) -> Vec<FuncArg> {
    args.iter()
        .map(|arg| FuncArg::new(arg.name.clone(), arg.mutable))
        .collect()
}

/// Compiler compiles high-level IR (the AST) to mid-level IR
pub struct Compiler<'c> {
    loc: Location,
    slugs: Slugs,
    scope: ScopeGraph,
    scopes: Vec<Scope>,
    line_number_offset: &'c LineNumberOffset,
}

impl<'c> Compiler<'c> {
    pub fn new(line_number_offset: &'c LineNumberOffset) -> Self {
        Self {
            slugs: Slugs::new(),
            loc: Location::default(),
            scopes: vec![],
            scope: ScopeGraph::new(),
            line_number_offset,
        }
    }

    fn enter_scope(&mut self, context: ScopeContext) -> ScopeId {
        let new_scope = Scope::new_child(self.scope.current(), context);

        self.scope.add(new_scope)
    }

    fn exit_scope(&mut self) {
        if let Some(scope) = self.scope.pop() {
            self.scopes.push(scope);
        }
    }

    fn build_assign_local(&self, local_id: LocalId, value: Value) -> types::Stmt {
        types::Stmt::new(
            self.loc.clone(),
            StmtKind::Assign(Assign::new(AssignLeftHand::Local(local_id), value)),
        )
    }

    fn build_method_call(&self, object: Value, method_name: String) -> Value {
        Value::new(
            self.loc.clone(),
            ValueKind::Call(Box::new(Call::new(Value::new(
                self.loc.clone(),
                ValueKind::Member(Box::new(Member::new(object, method_name))),
            )))),
        )
    }

    fn compile_items(&mut self, items: &[Expr]) -> Result<Vec<Value>> {
        let mut values = vec![];

        for expr in items {
            values.push(self.compile_expr(expr)?);
        }

        Ok(values)
    }

    fn compile_store_var(
        &mut self,
        block: &mut Block,
        value: Value,
        mutable: bool,
        variable: &Variable,
    ) -> Result<()> {
        match variable {
            Variable::Name(name) => {
                if self.scope.get_local(name, false).is_some() {
                    return Err(CompileError::new(format!(
                        "unable to redefine name: {}",
                        name
                    )));
                }

                let local = self.scope.set_local(name.clone(), mutable)?;

                block
                    .statements
                    .push(self.build_assign_local(local.id, value));
            }
            Variable::Tuple(names) | Variable::Array(names) => {
                let tmp = self.scope.set_local(self.slugs.get("tmp"), false)?;

                block
                    .statements
                    .push(self.build_assign_local(tmp.id, value));

                for (i, name) in names.iter().enumerate() {
                    if self.scope.get_local(name, false).is_some() {
                        return Err(CompileError::new(format!(
                            "unable to redefine name: {}",
                            name
                        )));
                    }

                    let local = self.scope.set_local(name.clone(), mutable)?;

                    block.statements.push(self.build_assign_local(
                        local.id,
                        Value::new(
                            self.loc.clone(),
                            ValueKind::Index(Box::new(Index::new(
                                Value::new(self.loc.clone(), ValueKind::Local(tmp.id)),
                                Value::new(
                                    self.loc.clone(),
                                    ValueKind::Const(Const::Int32(i as i32)),
                                ),
                            ))),
                        ),
                    ))
                }
            }
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Value> {
        self.loc = self.line_number_offset.get_location(&expr.pos());

        let kind = match expr {
            Expr::Literal(literal_expr) => ValueKind::Const(match &literal_expr.literal {
                Literal::Int128(val) => Const::Int128(*val),
                Literal::Int64(val) => Const::Int64(*val),
                Literal::Uint64(val) => Const::Uint64(*val),
                Literal::Int32(val) => Const::Int32(*val),
                Literal::Byte(val) => Const::Byte(*val),
                Literal::Float(val) => Const::Float(*val),
                Literal::Bool(val) => Const::Bool(*val),
                Literal::Char(val) => Const::Char(*val),
                Literal::Symbol(val) => Const::Symbol(val.clone()),
                Literal::String(val) => Const::String(val.clone()),
            }),
            Expr::Ident(ident) => {
                if let Some(local) = self.scope.get_local(&ident.name, true) {
                    ValueKind::Local(local.id)
                } else {
                    ValueKind::Name(ident.name.clone())
                }
            }
            Expr::Call(call) => {
                let mut args = vec![];
                let mut keywords = vec![];

                for arg in call.keyword_args.iter() {
                    keywords.push(arg.name.clone());
                    args.push(self.compile_expr(&arg.value)?);
                }

                for arg in call.args.iter() {
                    args.push(self.compile_expr(arg)?);
                }

                let callee = self.compile_expr(&call.callee)?;

                ValueKind::Call(Box::new(Call {
                    callee,
                    args,
                    keywords,
                }))
            }
            Expr::Cast(cast) => {
                let value = self.compile_expr(&cast.expr)?;

                ValueKind::Cast(Box::new(Cast {
                    value,
                    dest: cast.type_name.clone(),
                }))
            }
            Expr::Not(not) => {
                let left = self.compile_expr(&not.expr)?;

                ValueKind::Comparison(Box::new(Operator::<ComparisonOp> {
                    left,
                    right: Value::new(self.loc.clone(), ValueKind::Const(Const::Bool(false))),
                    op: ComparisonOp::Eq,
                }))
            }
            Expr::Unwrap(unwrap) => {
                let value = self.compile_expr(&unwrap.expr)?;

                ValueKind::Unwrap(Box::new(value))
            }
            Expr::Array(array) => {
                let items = self.compile_items(&array.items)?;

                ValueKind::Array(items)
            }
            Expr::Tuple(tuple) => {
                let items = self.compile_items(&tuple.items)?;

                ValueKind::Tuple(items)
            }
            Expr::Map(map) => {
                let mut pairs = vec![];

                for key_value in map.key_values.iter() {
                    let key = self.compile_expr(&key_value.key)?;
                    let value = self.compile_expr(&key_value.value)?;

                    pairs.push(Value::new(
                        self.loc.clone(),
                        ValueKind::Tuple(vec![key, value]),
                    ));
                }

                ValueKind::Call(Box::new(Call::with_args(
                    Value::new(self.loc.clone(), ValueKind::Name("newMap".to_string())),
                    vec![Value::new(self.loc.clone(), ValueKind::Array(pairs))],
                )))
            }
            Expr::Closure(_) => unreachable!("not implemented yet"),
            Expr::Member(member) => {
                let object = self.compile_expr(&member.object)?;

                ValueKind::Member(Box::new(Member {
                    object,
                    member: member.member.clone(),
                }))
            }
            Expr::MemberCond(_) => unreachable!("not implemented yet"),
            Expr::Arithmetic(arithmetic) => {
                let left = self.compile_expr(&arithmetic.left)?;
                let right = self.compile_expr(&arithmetic.right)?;

                ValueKind::Arithmetic(Box::new(Operator::<ArithmeticOp> {
                    left,
                    right,
                    op: arithmetic.op,
                }))
            }
            Expr::Comparison(comparison) => {
                let left = self.compile_expr(&comparison.left)?;
                let right = self.compile_expr(&comparison.right)?;

                ValueKind::Comparison(Box::new(Operator::<ComparisonOp> {
                    left,
                    right,
                    op: comparison.op,
                }))
            }
            Expr::Logical(logical) => {
                let left = self.compile_expr(&logical.left)?;
                let right = self.compile_expr(&logical.right)?;

                ValueKind::Logical(Box::new(Operator::<LogicalOp> {
                    left,
                    right,
                    op: logical.op.clone(),
                }))
            }
            Expr::MakeRef(ref_expr) => {
                let value = self.compile_expr(&ref_expr.expr)?;

                ValueKind::MakeRef(Box::new(value))
            }
            Expr::Deref(deref) => {
                let value = self.compile_expr(&deref.expr)?;

                ValueKind::Deref(Box::new(value))
            }
            Expr::Index(index) => {
                let object = self.compile_expr(&index.object)?;

                if let Expr::Range(range) = &index.index {
                    let begin = self.compile_expr(&range.from)?;
                    let end = self.compile_expr(&range.to)?;

                    ValueKind::Slice(Box::new(Slice { object, begin, end }))
                } else {
                    let index = self.compile_expr(&index.index)?;

                    ValueKind::Index(Box::new(Index { object, index }))
                }
            }
            Expr::Range(range) => {
                let begin = self.compile_expr(&range.from)?;
                let end = self.compile_expr(&range.to)?;

                ValueKind::Range(Box::new(Range { begin, end }))
            }
            Expr::Template(template) => {
                let mut components = vec![];

                for component in template.components.iter() {
                    components.push(match &component {
                        TemplateComponent::String(val) => {
                            types::TemplateComponent::String(val.clone())
                        }
                        TemplateComponent::Expr(expr) => {
                            types::TemplateComponent::Value(self.compile_expr(expr)?)
                        }
                    });
                }

                ValueKind::Template(components)
            }
            Expr::TypeAssert(type_assert) => {
                let left = self.compile_expr(&type_assert.left)?;
                let right = self.compile_expr(&type_assert.right)?;

                ValueKind::TypeAssert(Box::new(TypeAssert { left, right }))
            }
        };

        Ok(Value::new(self.loc.clone(), kind))
    }

    fn compile_if_stmt(&mut self, if_stmt: &IfStmt) -> Result<types::Stmt> {
        let scope_id = self.enter_scope(ScopeContext::IfElse);

        let condition = self.compile_expr(&if_stmt.cond)?;
        let block = self.compile_block(scope_id, &if_stmt.body)?;

        self.exit_scope();

        let alt = if let Some(alt) = &if_stmt.alt {
            Some(match alt.as_ref() {
                Stmt::If(if_stmt) => {
                    let stmt = self.compile_if_stmt(if_stmt)?;
                    let mut block = Block::new(self.loc.clone(), self.scope.current().id);

                    block.statements.push(stmt);
                    block
                }
                Stmt::Else(else_stmt) => {
                    let scope_id = self.enter_scope(ScopeContext::IfElse);
                    let block = self.compile_block(scope_id, &else_stmt.body)?;

                    self.exit_scope();

                    block
                }
                _ => unreachable!(),
            })
        } else {
            None
        };

        Ok(types::Stmt::new(
            self.loc.clone(),
            StmtKind::Cond(Cond {
                condition,
                block,
                alt,
            }),
        ))
    }

    fn compile_stmt(&mut self, root: &mut Block, stmt: &Stmt) -> Result<()> {
        self.loc = self.line_number_offset.get_location(&stmt.pos());

        match stmt {
            Stmt::If(if_stmt) => {
                root.statements.push(self.compile_if_stmt(if_stmt)?);
            }
            Stmt::For(for_stmt) => {
                let meta = ForLoopMeta::new(self.slugs.get("loop_cont"));
                let for_block = if let Some(expr) = &for_stmt.expr {
                    // Step 1. Get the iterator from the value
                    let value = self.compile_expr(expr)?;
                    let iter = self.scope.set_local(self.slugs.get("__iter__"), false)?;

                    root.statements.push(self.build_assign_local(
                        iter.id,
                        self.build_method_call(value, "iter".to_string()),
                    ));

                    let scope_id = self.enter_scope(ScopeContext::ForLoop(meta));
                    let mut block = Block::new(self.loc.clone(), scope_id);

                    // Step 2. Now in the loop, get the next value from the iterator
                    let item = self.scope.set_local(self.slugs.get("__item__"), false)?;

                    block.statements.push(self.build_assign_local(
                        item.id,
                        self.build_method_call(
                            Value::new(self.loc.clone(), ValueKind::Local(iter.id)),
                            "next".to_string(),
                        ),
                    ));

                    // Step 3. Check if it has a value and either continue or stop
                    let scope_id = self.enter_scope(ScopeContext::Block);
                    let exit_loop_block =
                        Block::with_terminator(self.loc.clone(), scope_id, Terminator::Break);

                    self.exit_scope();

                    block.statements.push(types::Stmt::new(
                        self.loc.clone(),
                        StmtKind::Cond(Cond {
                            condition: self.build_method_call(
                                Value::new(self.loc.clone(), ValueKind::Local(item.id)),
                                "isNone".to_string(),
                            ),
                            block: exit_loop_block,
                            alt: None,
                        }),
                    ));

                    // Step 4. Store the item, evaluate the body and so on..
                    if let Some(var) = &for_stmt.alias {
                        self.compile_store_var(
                            &mut block,
                            Value::new(
                                self.loc.clone(),
                                ValueKind::Unwrap(Box::new(Value::new(
                                    self.loc.clone(),
                                    ValueKind::Local(item.id),
                                ))),
                            ),
                            false,
                            var,
                        )?;
                    }

                    self.compile_stmt_list(&mut block, &for_stmt.body)?;
                    self.exit_scope();

                    block
                } else {
                    let scope_id = self.enter_scope(ScopeContext::ForLoop(meta));
                    let block = self.compile_block(scope_id, &for_stmt.body)?;

                    self.exit_scope();

                    block
                };

                root.statements.push(types::Stmt::new(
                    self.loc.clone(),
                    StmtKind::Loop(for_block),
                ));
            }
            Stmt::Expr(expr_stmt) => {
                root.statements.push(types::Stmt::new(
                    self.loc.clone(),
                    StmtKind::Eval(self.compile_expr(&expr_stmt.expr)?),
                ));
            }
            Stmt::Let(let_stmt) => {
                let value = self.compile_expr(&let_stmt.value)?;

                self.compile_store_var(root, value, let_stmt.mutable, &let_stmt.var)?;
            }
            Stmt::Assign(assign) => {
                let right = self.compile_expr(&assign.expand())?;
                let kind = match &assign.left {
                    Expr::Index(index_expr) => {
                        let object = self.compile_expr(&index_expr.object)?;
                        let index = self.compile_expr(&index_expr.index)?;
                        let left = AssignLeftHand::Index(Box::new(Index::new(object, index)));

                        Assign::new(left, right)
                    }
                    Expr::Ident(ident_expr) => {
                        let local_id =
                            if let Some(local) = self.scope.get_local(&ident_expr.name, true) {
                                if !local.mutable {
                                    return Err(CompileError::new(format!(
                                        "name is not mutable: {}",
                                        ident_expr.name,
                                    )));
                                }

                                local.id
                            } else {
                                return Err(CompileError::new(format!(
                                    "unable to assign value to unknown name: {}",
                                    ident_expr.name
                                )));
                            };

                        Assign::new(AssignLeftHand::Local(local_id), right)
                    }
                    Expr::Member(member_expr) => {
                        let object = self.compile_expr(&member_expr.object)?;

                        Assign::new(
                            AssignLeftHand::Member(Member::new(object, member_expr.member.clone())),
                            right,
                        )
                    }
                    _ => {
                        return Err(CompileError::new(
                            "invalid left-hand in assignment".to_string(),
                        ))
                    }
                };

                root.statements
                    .push(types::Stmt::new(self.loc.clone(), StmtKind::Assign(kind)));
            }
            Stmt::Return(return_stmt) => {
                let value = self.compile_expr(&return_stmt.expr)?;

                root.terminator = Some(Terminator::Return);
                root.statements
                    .push(types::Stmt::new(self.loc.clone(), StmtKind::Return(value)));
            }
            Stmt::Raise(raise) => {
                let value = self.compile_expr(&raise.expr)?;

                root.terminator = Some(Terminator::Raise);
                root.statements.push(types::Stmt::new(
                    self.loc.clone(),
                    StmtKind::Eval(Value::new(
                        self.loc.clone(),
                        ValueKind::Call(Box::new(Call::with_args(
                            Value::new(self.loc.clone(), ValueKind::Name("raise".to_string())),
                            vec![value],
                        ))),
                    )),
                ));
            }
            Stmt::Break(_) => {
                root.terminator = Some(Terminator::Break);
            }
            Stmt::Else(_)
            | Stmt::LetDecl(_)
            | Stmt::Import(_)
            | Stmt::FnDecl(_)
            | Stmt::Module(_)
            | Stmt::ExternFnDecl(_)
            | Stmt::ClassDecl(_)
            | Stmt::MixinDecl(_)
            | Stmt::InterfaceDecl(_) => {}
        };

        Ok(())
    }

    fn compile_block(&mut self, scope_id: ScopeId, stmt_list: &[Stmt]) -> Result<Block> {
        let mut block = Block::new(self.loc.clone(), scope_id);

        self.compile_stmt_list(&mut block, stmt_list)?;

        Ok(block)
    }

    fn compile_stmt_list(&mut self, block: &mut Block, stmt_list: &[Stmt]) -> Result<()> {
        for stmt in stmt_list {
            if block.terminator.is_some() {
                break;
            }

            self.compile_stmt(block, stmt)?;
        }

        Ok(())
    }

    fn compile_function(
        &mut self,
        fn_decl: &FnDeclStmt,
        class_name: Option<&str>,
    ) -> Result<Function> {
        let scope_id = self.enter_scope(ScopeContext::Function(fn_decl.name.clone()));

        // Register locals for input arguments
        for arg in fn_decl.args.iter() {
            self.scope.set_local(arg.name.clone(), arg.mutable)?;
        }

        let block = if class_name.is_some() {
            let local = self.scope.set_local("this".to_string(), true)?;

            let mut block = self.compile_block(scope_id, &fn_decl.body)?;

            block.statements.insert(
                0,
                self.build_assign_local(
                    local.id,
                    Value::new(self.loc.clone(), ValueKind::Receiver),
                ),
            );

            block
        } else {
            self.compile_block(scope_id, &fn_decl.body)?
        };

        self.exit_scope();

        Ok(Function {
            name: fn_decl.name.to_string(),
            is_extern: false,
            is_closure: false,
            args: map_fn_args(&fn_decl.args),
            block,
        })
    }

    fn compile_extern_function(&mut self, extern_fn_decl: &FnDeclStmt) -> Result<Function> {
        Ok(Function {
            name: extern_fn_decl.name.to_string(),
            args: map_fn_args(&extern_fn_decl.args),
            is_extern: true,
            is_closure: false,
            block: Block::default(),
        })
    }

    fn compile_interface(&mut self, interface_decl: &InterfaceDeclStmt) -> Interface {
        Interface {
            name: interface_decl.name.clone(),
            functions: interface_decl
                .functions
                .iter()
                .map(|func| func.name.clone())
                .collect(),
        }
    }

    fn compile_class(&mut self, class_decl: &ClassDeclStmt) -> Result<Class> {
        self.enter_scope(ScopeContext::Class(class_decl.name.clone()));

        let mut fields = vec![];

        for field in class_decl.fields.iter() {
            fields.push(Field::new(field.name.clone(), field.mutable, field.public));
        }

        let mut methods = Vec::new();

        for extern_fn_decl in class_decl.extern_funcs.iter() {
            methods.push(self.compile_extern_function(extern_fn_decl)?);
        }

        for fn_decl in class_decl.funcs.iter() {
            methods.push(self.compile_function(fn_decl, Some(&class_decl.name))?);
        }

        self.exit_scope();

        Ok(Class {
            name: class_decl.name.clone(),
            fields,
            methods,
        })
    }

    fn _compile(&mut self, tree: &[Stmt]) -> Result<Vec<Decl>> {
        let mut program = vec![];

        for stmt in tree {
            let decl = match stmt {
                Stmt::FnDecl(fn_decl) => {
                    let function = self.compile_function(fn_decl, None)?;

                    Decl::new(DeclKind::Function(function), fn_decl.public)
                }
                Stmt::ExternFnDecl(fn_decl_stmt) => {
                    let function = self.compile_extern_function(fn_decl_stmt)?;

                    Decl::new(DeclKind::Function(function), fn_decl_stmt.public)
                }
                Stmt::ClassDecl(class_decl) => {
                    let class = self.compile_class(class_decl)?;

                    Decl::new(DeclKind::Class(class), class_decl.public)
                }
                Stmt::InterfaceDecl(interface_decl) => {
                    let interface = self.compile_interface(interface_decl);

                    Decl::new(DeclKind::Interface(interface), interface_decl.public)
                }

                _ => unreachable!(),
            };

            program.push(decl);
        }

        Ok(program)
    }

    pub fn compile(mut self, tree: &[Stmt]) -> Result<Mir> {
        let program = self
            ._compile(tree)
            .map_err(|e| e.with_location(self.loc.clone()))?;

        // Exists the global scope
        self.exit_scope();

        self.scopes.sort_unstable_by_key(|scope| scope.id);

        Ok(Mir {
            scopes: self.scopes,
            program,
        })
    }
}
