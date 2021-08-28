use std::collections::HashMap;
use std::path::{Path, PathBuf};

use indexmap::map::IndexMap;

use atom_ir::{Code, Label, IR};

use crate::ast::{
    ArithmeticExpr, ArithmeticOp, AssignOp, ClassDeclStmt, ComparisonOp, Expr, ExternFnDeclStmt,
    FnDeclStmt, ImportStmt, InterfaceDeclStmt, Literal, LogicalOp, MemberCondExpr, Pos, Stmt,
    TemplateComponent,
};
use crate::compiler::filesystem::AbstractFs;
use crate::parser;
use crate::std::core::DEFAULT_IMPORTS;

use super::filesystem::{Fs, FsWithCache, VirtFs};
use super::module::{Class, Field, Func, FuncArg, Interface, Module, Type, TypeKind};
use super::optimizers::{call_void, load_local_twice_add, pre_compute_labels, Optimizer};
use super::path_finder::PathFinder;
use super::result::{CompileError, Result};
use super::scope::{ForLoopMeta, Local, Scope, ScopeContext};

pub struct Compiler {
    pos: Pos,
    fs: FsWithCache,
    optimize: bool,
    scope_id: usize,
    module: Module,
    tree: Vec<Stmt>,
    scope: Vec<Scope>,
    labels: Vec<String>,
    path_finder: PathFinder,
    optimizers: Vec<Optimizer>,
}

impl Compiler {
    pub fn new(tree: Vec<Stmt>, optimize: bool) -> Self {
        let fs = FsWithCache::new(Fs {}, {
            let mut cache = VirtFs::new();

            cache.add_file("std/io.atom", include_str!("../std/atom/std/io.atom"));
            cache.add_file("std/core.atom", include_str!("../std/atom/std/core.atom"));
            cache.add_file(
                "std/encoding/utf8.atom",
                include_str!("../std/atom/std/encoding/utf8.atom"),
            );

            cache
        });

        Self {
            tree,
            fs,
            optimize,
            pos: 0..0,
            scope_id: 0,
            labels: vec![],
            module: Module::new(),
            optimizers: if optimize {
                vec![
                    call_void::optimize,
                    load_local_twice_add::optimize,
                    //remove_core_validations::optimize,
                    pre_compute_labels::optimize,
                ]
            } else {
                vec![]
            },
            scope: vec![Scope::new()],
            path_finder: PathFinder::new(),
        }
    }

    pub fn add_lookup_path(&mut self, path: impl AsRef<Path>) {
        self.path_finder.add_path(path.as_ref().to_path_buf());
    }

    fn enter_scope(&mut self, context: ScopeContext) {
        self.scope_id += 1;

        let new_scope = Scope::new_child(context, self.scope_id);

        self.scope.push(new_scope);
    }

    fn exit_scope(&mut self) {
        self.scope.pop();
    }

    fn set_local(&mut self, name: String, mutable: bool) -> Result<Local> {
        Scope::set_local(self.scope.as_mut(), name, mutable).map_err(|e| {
            let mut e = e;

            e.pos = self.pos.clone();
            e
        })
    }

    fn fork(&self, module_name: String, tree: Vec<Stmt>) -> Self {
        Self {
            fs: FsWithCache::new(Fs {}, VirtFs::new()),
            pos: 0..0,
            optimize: self.optimize,
            scope_id: 0,
            module: Module::with_name(module_name),
            tree,
            scope: vec![],
            labels: vec![],
            path_finder: self.path_finder.clone(),
            optimizers: self.optimizers.clone(),
        }
    }

    fn compile_member_cond(
        &mut self,
        member_cond_expr: &MemberCondExpr,
        body: Vec<IR>,
    ) -> Result<Vec<IR>> {
        let mut ir = vec![];
        let label_some = self.make_label("cond_some");
        let label_none = self.make_label("cond_none");

        ir.push(self.compile_expr(&member_cond_expr.object)?);
        ir.push(vec![
            IR::new(
                Code::TeeMember("isSome".to_string()),
                member_cond_expr.pos.clone(),
            ),
            IR::new(Code::Call(0), member_cond_expr.pos.clone()),
            IR::new(
                Code::Branch((
                    Label::new(label_some.clone()),
                    Label::new(label_none.clone()),
                )),
                member_cond_expr.pos.clone(),
            ),
            IR::new(Code::SetLabel(label_some), member_cond_expr.pos.clone()),
            IR::new(Code::Unwrap, member_cond_expr.pos.clone()),
            IR::new(
                Code::LoadMember(member_cond_expr.member.to_string()),
                member_cond_expr.pos.clone(),
            ),
        ]);
        ir.push(body);
        ir.push(vec![
            IR::new(self.compile_name("some")?, member_cond_expr.pos.clone()),
            IR::new(Code::Call(1), member_cond_expr.pos.clone()),
            IR::new(Code::SetLabel(label_none), member_cond_expr.pos.clone()),
        ]);

        Ok(ir.concat())
    }

    fn make_label(&mut self, prefix: &str) -> String {
        let mut i: i64 = 0;

        loop {
            let label = if i == 0 {
                prefix.to_string()
            } else {
                format!("{}{}", prefix, i)
            };

            if !self.labels.contains(&label) {
                self.labels.push(label.clone());

                return label;
            }

            i += 1;
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Vec<IR>> {
        let mut ir = vec![];

        self.pos = expr.pos();

        match &expr {
            Expr::Literal(literal_expr) => ir.push(vec![IR::new(
                match &literal_expr.literal {
                    Literal::Int(val) => Code::ConstInt(*val),
                    Literal::Float(val) => Code::ConstFloat(*val),
                    Literal::Bool(val) => Code::ConstBool(*val),
                    Literal::String(val) => Code::ConstString(val.clone()),
                    Literal::Char(val) => Code::ConstChar(*val),
                    Literal::Byte(val) => Code::ConstByte(*val),
                    Literal::Nil => Code::ConstNil,
                },
                literal_expr.pos.clone(),
            )]),
            Expr::Template(template_expr) => {
                for component in template_expr.components.iter() {
                    ir.push(match component {
                        TemplateComponent::String(s) => {
                            vec![IR::new(Code::ConstString(s.clone()), self.pos.clone())]
                        }
                        TemplateComponent::Expr(expr) => self.compile_expr(expr)?,
                    });
                }

                ir.push(vec![IR::new(
                    Code::MakeTemplate(template_expr.components.len()),
                    self.pos.clone(),
                )]);
            }
            Expr::Range(range_expr) => {
                ir.push(self.compile_expr(&range_expr.from)?);
                ir.push(self.compile_expr(&range_expr.to)?);
                ir.push(vec![IR::new(Code::MakeRange, self.pos.clone())]);
            }
            Expr::Ident(ident) => {
                ir.push(vec![IR::new(
                    self.compile_name(&ident.name)?,
                    ident.pos.clone(),
                )]);
            }
            Expr::Cast(cast_expr) => {
                ir.push(self.compile_expr(&cast_expr.expr)?);
                ir.push(vec![IR::new(
                    Code::Cast(cast_expr.type_name.clone()),
                    cast_expr.pos.clone(),
                )]);
            }
            Expr::Call(call_expr) => {
                let mut names = vec![];

                for arg in call_expr.keyword_args.iter() {
                    names.push(arg.name.clone());
                    ir.push(self.compile_expr(&arg.value)?);
                }

                for arg in call_expr.args.iter() {
                    ir.push(self.compile_expr(arg)?);
                }

                let instructions = vec![if names.is_empty() {
                    IR::new(Code::Call(call_expr.args.len()), call_expr.pos.clone())
                } else {
                    // Make sure each keyword argument is unique
                    if !names.is_empty() {
                        let mut unique_keys = vec![];

                        for key in names.iter() {
                            if unique_keys.contains(key) {
                                return Err(CompileError::new(
                                    format!("duplicate keyword argument: {}", key),
                                    self.pos.clone(),
                                ));
                            }

                            unique_keys.push(key.to_string());
                        }
                    }

                    IR::new(
                        Code::CallKeywords((
                            names,
                            call_expr.keyword_args.len() + call_expr.args.len(),
                        )),
                        call_expr.pos.clone(),
                    )
                }];

                if let Expr::MemberCond(member_cond_expr) = &call_expr.callee {
                    ir.push(self.compile_member_cond(member_cond_expr, instructions)?);
                } else {
                    let is_target = || {
                        if let Some((target, is_method)) = Scope::get_function_target(&self.scope) {
                            if !is_method {
                                if let Expr::Ident(ident_expr) = &call_expr.callee {
                                    return ident_expr.name == target;
                                }

                                // @TODO: Support tail calls for methods
                                //if is_method {
                                //    if let Expr::Member(member_expr) = &call_expr.callee {
                                //        if let Expr::Ident(ident_expr) = &member_expr.object {
                                //            return ident_expr.name == "this"
                                //                && member_expr.member == target;
                                //        }
                                //    }
                                //}
                            }
                        }

                        false
                    };

                    if self.optimize && is_target() {
                        ir.push(vec![IR::new(Code::LoadTarget, self.pos.clone())]);
                    } else {
                        ir.push(self.compile_expr(&call_expr.callee)?);
                    }

                    ir.push(instructions);
                }
            }
            Expr::Unwrap(unwrap_expr) => {
                ir.push(self.compile_expr(&unwrap_expr.expr)?);
                ir.push(vec![IR::new(Code::Unwrap, unwrap_expr.pos.clone())]);
            }
            Expr::Not(not_expr) => {
                ir.push(self.compile_expr(&not_expr.expr)?);
                ir.push(vec![IR::new(Code::Not, not_expr.pos.clone())]);
            }
            Expr::Index(index_expr) => {
                if !Scope::in_unsafe_block(&self.scope) {
                    return Err(CompileError::new(
                        "unable to perform index operation outside of an 'unsafe' block"
                            .to_string(),
                        self.pos.clone(),
                    ));
                }

                ir.push(self.compile_expr(&index_expr.object)?);

                let instructions = vec![IR::new(Code::LoadIndex, index_expr.pos.clone())];

                if let Expr::MemberCond(member_cond_expr) = &index_expr.index {
                    ir.push(self.compile_member_cond(member_cond_expr, instructions)?);
                } else {
                    ir.push(self.compile_expr(&index_expr.index)?);
                    ir.push(instructions);
                }
            }
            Expr::Array(array_expr) => {
                for item in array_expr.items.iter() {
                    ir.push(self.compile_expr(item)?);
                }

                ir.push(vec![IR::new(
                    Code::MakeArray(array_expr.items.len()),
                    array_expr.pos.clone(),
                )]);
            }
            Expr::Map(map_expr) => {
                for key_val in map_expr.key_values.iter() {
                    ir.push(self.compile_expr(&key_val.key)?);
                    ir.push(self.compile_expr(&key_val.value)?);
                }

                ir.push(vec![IR::new(
                    Code::MakeMap(map_expr.key_values.len()),
                    map_expr.pos.clone(),
                )]);
            }
            Expr::MemberCond(member_cond_expr) => {
                ir.push(self.compile_member_cond(member_cond_expr, vec![])?);
            }
            Expr::Member(member_expr) => {
                ir.push(self.compile_expr(&member_expr.object)?);
                ir.push(vec![IR::new(
                    Code::LoadMember(member_expr.member.to_string()),
                    member_expr.pos.clone(),
                )]);
            }
            Expr::Arithmetic(arithmetic_expr) => {
                ir.push(self.compile_expr(&arithmetic_expr.left)?);
                ir.push(self.compile_expr(&arithmetic_expr.right)?);
                ir.push(vec![IR::new(
                    match arithmetic_expr.op {
                        ArithmeticOp::Mul => Code::ArithmeticMul,
                        ArithmeticOp::Div => Code::ArithmeticDiv,
                        ArithmeticOp::Add => Code::ArithmeticAdd,
                        ArithmeticOp::Sub => Code::ArithmeticSub,
                        ArithmeticOp::Exp => Code::ArithmeticExp,
                        ArithmeticOp::BitAnd => Code::ArithmeticBitAnd,
                        ArithmeticOp::BitOr => Code::ArithmeticBitOr,
                    },
                    arithmetic_expr.pos.clone(),
                )]);
            }
            Expr::TypeAssert(type_assert_expr) => {
                ir.push(self.compile_expr(&type_assert_expr.left)?);
                ir.push(self.compile_expr(&type_assert_expr.right)?);
                ir.push(vec![IR::new(
                    Code::AssertIsType,
                    type_assert_expr.pos.clone(),
                )]);
            }
            Expr::Comparison(comparison_expr) => {
                ir.push(self.compile_expr(&comparison_expr.left)?);
                ir.push(self.compile_expr(&comparison_expr.right)?);
                ir.push(vec![IR::new(
                    match comparison_expr.op {
                        ComparisonOp::Lt => Code::ComparisonLt,
                        ComparisonOp::Lte => Code::ComparisonLte,
                        ComparisonOp::Gt => Code::ComparisonGt,
                        ComparisonOp::Gte => Code::ComparisonGte,
                        ComparisonOp::Eq => Code::ComparisonEq,
                        ComparisonOp::Neq => Code::ComparisonNeq,
                    },
                    comparison_expr.pos.clone(),
                )]);
            }
            Expr::Logical(logical_expr) => {
                match logical_expr.op {
                    LogicalOp::And => {
                        ir.push(self.compile_expr(&logical_expr.left)?);
                        ir.push(self.compile_expr(&logical_expr.right)?);
                        ir.push(vec![IR::new(Code::LogicalAnd, logical_expr.pos.clone())]);
                    }
                    LogicalOp::Or => {
                        ir.push(self.compile_expr(&logical_expr.left)?);

                        let label = self.make_label("or");

                        ir.push(vec![IR::new(
                            Code::JumpIfTrue(Label::new(label.clone())),
                            logical_expr.pos.clone(),
                        )]);
                        ir.push(self.compile_expr(&logical_expr.right)?);
                        ir.push(vec![IR::new(
                            Code::SetLabel(label),
                            logical_expr.pos.clone(),
                        )]);
                    }
                };
            }
            Expr::MakeRef(make_ref_expr) => {
                ir.push(self.compile_expr(&make_ref_expr.expr)?);
                ir.push(vec![IR::new(Code::MakeRef, self.pos.clone())]);
            }
            Expr::Deref(deref_expr) => {
                ir.push(self.compile_expr(&deref_expr.expr)?);
                ir.push(vec![IR::new(Code::Deref, self.pos.clone())]);
            }
        };

        Ok(ir.concat())
    }

    fn compile_name(&self, name: &str) -> Result<Code> {
        if name == "this" && Scope::in_function_block(&self.scope) {
            Ok(Code::LoadReceiver)
        } else if let Some(local) = Scope::get_local(&self.scope, &name, true) {
            Ok(Code::Load(local.id))
        } else if let Some(id) = self.module.globals.get_index_of(name) {
            Ok(Code::LoadGlobal(id))
        } else if let Some(id) = self.module.funcs.get_index_of(name) {
            Ok(Code::LoadFn(id))
        } else if let Some(id) = self.module.classes.get_index_of(name) {
            Ok(Code::LoadClass(id))
        } else if let Some(id) = self.module.interfaces.get_index_of(name) {
            Ok(Code::LoadInterface(id))
        } else {
            Err(CompileError::new(
                format!("no such name: {}", name),
                self.pos.clone(),
            ))
        }
    }

    fn compile_assign_local(&mut self, name: &str, value: &Expr) -> Result<Vec<IR>> {
        if let Some(local) = Scope::get_local(&self.scope, name, true) {
            if !local.mutable {
                return Err(CompileError::new(
                    format!("name is not mutable: {}", name),
                    self.pos.clone(),
                ));
            }

            return Ok(vec![
                self.compile_expr(value)?,
                vec![IR::new(
                    if local.mutable {
                        Code::StoreMut(local.id)
                    } else {
                        Code::Store(local.id)
                    },
                    self.pos.clone(),
                )],
            ]
            .concat());
        }

        Err(CompileError::new(
            format!("unable to assign value to unknown name: {}", name),
            self.pos.clone(),
        ))
    }

    fn compile_assign_member(
        &mut self,
        object: &Expr,
        member: &str,
        value: &Expr,
    ) -> Result<Vec<IR>> {
        Ok(vec![
            self.compile_expr(value)?,
            self.compile_expr(object)?,
            vec![IR::new(
                Code::StoreMember(member.to_string()),
                self.pos.clone(),
            )],
        ]
        .concat())
    }

    fn compile_assign_index(
        &mut self,
        object: &Expr,
        index: &Expr,
        value: &Expr,
    ) -> Result<Vec<IR>> {
        Ok(vec![
            self.compile_expr(object)?,
            self.compile_expr(index)?,
            self.compile_expr(value)?,
            vec![IR::new(Code::StoreIndex, self.pos.clone())],
        ]
        .concat())
    }

    fn compile_assign(&mut self, left: &Expr, right: &Expr) -> Result<Vec<IR>> {
        match left {
            Expr::Index(index_expr) => {
                self.compile_assign_index(&index_expr.object, &index_expr.index, right)
            }
            Expr::Ident(ident_expr) => self.compile_assign_local(&ident_expr.name, right),
            Expr::Member(member_expr) => {
                self.compile_assign_member(&member_expr.object, &member_expr.member, right)
            }
            _ => Err(CompileError::new(
                "invalid left-hand side in assignment".to_string(),
                self.pos.clone(),
            )),
        }
    }

    fn compile_let(&mut self, mutable: bool, name: &str, value: Option<&Expr>) -> Result<Vec<IR>> {
        let local = if Scope::get_local(&self.scope, name, false).is_some() {
            return Err(CompileError::new(
                format!("name already defined: {}", name),
                self.pos.clone(),
            ));
        } else {
            self.set_local(name.to_string(), mutable)?
        };

        if let Some(expr) = value {
            return Ok(vec![
                self.compile_expr(expr)?,
                vec![IR::new(
                    if mutable {
                        Code::StoreMut(local.id)
                    } else {
                        Code::Store(local.id)
                    },
                    self.pos.clone(),
                )],
            ]
            .concat());
        }

        Ok(vec![])
    }

    fn _compile_stmt_list(&mut self, tree: &[Stmt]) -> Result<Vec<IR>> {
        let mut ir = vec![];

        for stmt in tree.iter() {
            self.pos = stmt.pos();

            match stmt {
                Stmt::If(if_stmt) => {
                    let if_label = self.make_label("if");
                    let else_label = self.make_label("else");
                    let cont_label = self.make_label("if_else_cont");

                    ir.push(self.compile_expr(&if_stmt.cond)?);
                    ir.push(vec![
                        IR::new(
                            Code::Branch((
                                Label::new(if_label.clone()),
                                Label::new(if if_stmt.alt.is_empty() {
                                    cont_label.clone()
                                } else {
                                    else_label.clone()
                                }),
                            )),
                            self.pos.clone(),
                        ),
                        IR::new(Code::SetLabel(if_label), self.pos.clone()),
                    ]);
                    ir.push(self.compile_stmt_list(ScopeContext::IfElse, &if_stmt.body)?);

                    if !if_stmt.alt.is_empty() {
                        ir.push(vec![IR::new(
                            Code::Jump(Label::new(cont_label.clone())),
                            self.pos.clone(),
                        )]);
                        ir.push(vec![IR::new(Code::SetLabel(else_label), self.pos.clone())]);
                        ir.push(self.compile_stmt_list(ScopeContext::IfElse, &if_stmt.alt)?);
                        ir.push(vec![IR::new(
                            Code::Jump(Label::new(cont_label.clone())),
                            self.pos.clone(),
                        )]);
                    }

                    ir.push(vec![IR::new(Code::SetLabel(cont_label), self.pos.clone())]);
                }
                Stmt::Expr(expr_stmt) => {
                    ir.push(self.compile_expr(&expr_stmt.expr)?);
                    ir.push(vec![IR::new(Code::Discard, self.pos.clone())]);
                }
                Stmt::Let(let_stmt) => ir.push(self.compile_let(
                    let_stmt.mutable,
                    &let_stmt.name,
                    Some(&let_stmt.value),
                )?),
                Stmt::LetDecl(let_decl_stmt) => {
                    ir.push(self.compile_let(false, &let_decl_stmt.name, None)?)
                }
                Stmt::Assign(assign_stmt) => {
                    if let Some(op) = &assign_stmt.op {
                        ir.push(
                            self.compile_assign(
                                &assign_stmt.left,
                                &Expr::Arithmetic(
                                    ArithmeticExpr {
                                        left: assign_stmt.left.clone(),
                                        right: assign_stmt.right.clone(),
                                        op: match op {
                                            AssignOp::Mul => ArithmeticOp::Mul,
                                            AssignOp::Div => ArithmeticOp::Div,
                                            AssignOp::Add => ArithmeticOp::Add,
                                            AssignOp::Sub => ArithmeticOp::Sub,
                                        },
                                        pos: assign_stmt.pos.clone(),
                                    }
                                    .into(),
                                ),
                            )?,
                        );
                    } else {
                        ir.push(self.compile_assign(&assign_stmt.left, &assign_stmt.right)?)
                    }
                }
                Stmt::For(for_stmt) => {
                    let for_label = self.make_label("for");
                    let body_label = self.make_label("for_body");
                    let cont_label = self.make_label("for_cont");

                    if let Some(expr) = &for_stmt.expr {
                        let iter = self.set_local("__iter__".to_string(), false)?;

                        ir.push(self.compile_expr(expr)?);

                        self.enter_scope(ScopeContext::ForLoop(ForLoopMeta {
                            continue_label: cont_label.clone(),
                        }));

                        let local = self.set_local(
                            match &for_stmt.alias {
                                None => "__item__".to_string(),
                                Some(name) => name.clone(),
                            },
                            false,
                        )?;

                        ir.push(
                            vec![
                                // Step 1. Get the iterator from the object
                                self.compile_name("Iterable")?,
                                Code::Validate,
                                Code::LoadMember("iter".to_string()),
                                Code::Call(0),
                                Code::Store(iter.id),
                                // Step 2. Now in the loop, get the next value from the iterator
                                Code::SetLabel(for_label.clone()),
                                Code::Load(iter.id),
                                Code::LoadMember("next".to_string()),
                                Code::Call(0),
                                Code::Store(local.id),
                                // Step 3. Check if it has a value and either continue or stop
                                Code::Load(local.id),
                                Code::LoadMember("isSome".to_string()),
                                Code::Call(0),
                                Code::Branch((
                                    Label::new(body_label.clone()),
                                    Label::new(cont_label.clone()),
                                )),
                                // Step 4. Evaluate the body and so on..
                                Code::SetLabel(body_label.clone()),
                            ]
                            .into_iter()
                            .map(|code| IR::new(code, self.pos.clone()))
                            .collect::<Vec<_>>(),
                        );

                        // Only store the current item when requested
                        if for_stmt.alias.is_some() {
                            ir.push(vec![
                                IR::new(Code::Load(local.id), self.pos.clone()),
                                IR::new(Code::Unwrap, self.pos.clone()),
                                IR::new(Code::Store(local.id), self.pos.clone()),
                            ]);
                        }

                        ir.push(self._compile_stmt_list(&for_stmt.body)?);
                    } else {
                        self.enter_scope(ScopeContext::ForLoop(ForLoopMeta {
                            continue_label: cont_label.clone(),
                        }));

                        ir.push(vec![
                            IR::new(Code::SetLabel(for_label.clone()), self.pos.clone()),
                            IR::new(Code::SetLabel(body_label), self.pos.clone()),
                        ]);
                        ir.push(self.compile_stmt_list(
                            ScopeContext::ForLoop(ForLoopMeta {
                                continue_label: cont_label.clone(),
                            }),
                            &for_stmt.body,
                        )?);
                    }

                    ir.push(vec![
                        IR::new(Code::Jump(Label::new(for_label)), self.pos.clone()),
                        IR::new(Code::SetLabel(cont_label), self.pos.clone()),
                    ]);

                    self.exit_scope();
                }
                Stmt::Break(break_stmt) => {
                    if break_stmt.label.is_some() {
                        unreachable!();
                    }

                    if let Some(meta) = Scope::get_for_loop(&self.scope) {
                        ir.push(vec![IR::new(
                            Code::Jump(Label::new(meta.continue_label.clone())),
                            self.pos.clone(),
                        )]);
                    } else {
                        return Err(CompileError::new(
                            "unable to break outside of a loop".to_string(),
                            self.pos.clone(),
                        ));
                    }
                }
                Stmt::Raise(raise_stmt) => {
                    ir.push(self.compile_expr(&raise_stmt.expr)?);
                    ir.push(vec![IR::new(Code::Raise, raise_stmt.pos.clone())]);
                }
                Stmt::Return(return_stmt) => {
                    ir.push(self.compile_expr(&return_stmt.expr)?);
                    ir.push(vec![IR::new(Code::Return, self.pos.clone())]);
                }
                Stmt::Unsafe(unsafe_stmt) => {
                    ir.push(self.compile_stmt_list(ScopeContext::Unsafe, &unsafe_stmt.body)?);
                }
                // ignore top level statements
                Stmt::FnDecl(_)
                | Stmt::ExternFnDecl(_)
                | Stmt::ClassDecl(_)
                | Stmt::InterfaceDecl(_)
                | Stmt::Module(_)
                | Stmt::Import(_) => {}
            }
        }

        let mut instructions = ir.concat();

        for optimizer in self.optimizers.iter() {
            optimizer(&mut instructions);
        }

        Ok(instructions)
    }

    fn compile_stmt_list(&mut self, context: ScopeContext, tree: &[Stmt]) -> Result<Vec<IR>> {
        self.enter_scope(context);

        let instructions = self._compile_stmt_list(tree)?;

        self.exit_scope();

        Ok(instructions)
    }

    fn has_no_side_effects(&self, code: &Code) -> bool {
        matches!(
            code,
            Code::ArithmeticAdd
                | Code::ArithmeticSub
                | Code::ArithmeticMul
                | Code::ArithmeticDiv
                | Code::ArithmeticExp
                | Code::Return
                | Code::ArithmeticBitAnd
                | Code::ArithmeticBitOr
                | Code::ComparisonEq
                | Code::ComparisonNeq
                | Code::ComparisonGt
                | Code::ComparisonGte
                | Code::ComparisonLt
                | Code::ComparisonLte
                | Code::ConstString(_)
                | Code::ConstBool(_)
                | Code::ConstByte(_)
                | Code::ConstChar(_)
                | Code::ConstFloat(_)
                | Code::ConstInt(_)
                | Code::ConstNil
                | Code::Discard
        )
    }

    fn compile_extern_fn(&mut self, extern_fn_decl: &ExternFnDeclStmt) -> Func {
        Func {
            public: extern_fn_decl.public,
            name: extern_fn_decl.name.clone(),
            body: vec![],
            is_void: false,
            is_extern: true,
            // @TODO: this can be implemented when varargs are working
            args: vec![],
            pos: extern_fn_decl.pos.clone(),
        }
    }

    fn compile_fn(&mut self, fn_decl: &FnDeclStmt, is_method: bool) -> Result<Func> {
        self.enter_scope(ScopeContext::Function((fn_decl.name.clone(), is_method)));

        for arg in fn_decl.args.iter() {
            self.set_local(arg.name.clone(), false)?;
        }

        let mut body = self._compile_stmt_list(&fn_decl.body)?;

        self.exit_scope();

        // Let's check if we can optimize tail recursion call
        if self.optimize {
            loop {
                let index = || {
                    for (i, ir) in body.iter().enumerate() {
                        if ir.code == Code::LoadTarget {
                            let next_code = body.get(i + 1).map(|ir| &ir.code);

                            if let Some(Code::Call(arg_count)) = next_code {
                                if !body
                                    .iter()
                                    .skip(i + 2)
                                    .any(|ir| !self.has_no_side_effects(&ir.code))
                                {
                                    return Some((i, *arg_count));
                                }
                            }
                        }
                    }

                    None
                };

                match index() {
                    // No more tail recursions calls were found
                    None => break,

                    // Replace the two instructions with a tail-recursion-call one
                    Some((index, arg_count)) => {
                        let call = &mut body[index];
                        call.code = Code::TailCall(arg_count);

                        body.remove(index + 1);
                    }
                }
            }
        }

        Ok(Func {
            pos: fn_decl.pos.clone(),
            name: fn_decl.name.clone(),
            public: fn_decl.public,
            is_void: !body.iter().any(|ir| ir.code == Code::Return),
            is_extern: false,
            body,
            args: fn_decl
                .args
                .iter()
                .map(|arg| FuncArg {
                    mutable: arg.mutable,
                    name: arg.name.clone(),
                })
                .collect::<Vec<_>>(),
        })
    }

    fn compile_class(&mut self, class_decl: &ClassDeclStmt) -> Result<Class> {
        self.enter_scope(ScopeContext::Class);

        let mut fields = IndexMap::new();

        for field in class_decl.fields.iter() {
            if fields.contains_key(&field.name) {
                return Err(CompileError::new(
                    format!(
                        "unable to redefine field: {}.{}",
                        class_decl.name, field.name
                    ),
                    self.pos.clone(),
                ));
            }

            fields.insert(
                field.name.clone(),
                Field {
                    mutable: field.mutable,
                    public: field.public,
                },
            );
        }

        let mut funcs = HashMap::new();

        for extern_fn_decl in class_decl.extern_funcs.iter() {
            funcs.insert(
                extern_fn_decl.name.clone(),
                self.compile_extern_fn(extern_fn_decl),
            );
        }

        for fn_decl in class_decl.funcs.iter() {
            funcs.insert(fn_decl.name.clone(), self.compile_fn(fn_decl, true)?);
        }

        self.exit_scope();

        Ok(Class {
            name: class_decl.name.clone(),
            public: class_decl.public,
            fields,
            funcs,
        })
    }

    fn compile_interface(&mut self, interface_decl: &InterfaceDeclStmt) -> Result<Interface> {
        Ok(Interface {
            name: interface_decl.name.clone(),
            public: interface_decl.public,
            functions: interface_decl
                .functions
                .iter()
                .map(|func| func.name.clone())
                .collect(),
        })
    }

    fn setup_prelude(&mut self) -> Result<()> {
        for name in DEFAULT_IMPORTS {
            self.compile_import(ImportStmt {
                name: name.to_string(),
                pos: 0..0,
            })?;
        }

        Ok(())
    }

    fn parse_and_compile(&self, filename: PathBuf, name: String) -> Result<Module> {
        let content = self.fs.read_file(&filename).map_err(|e| {
            CompileError::new(
                format!("failed to read '{}': {}", filename.to_str().unwrap(), e),
                self.pos.clone(),
            )
        })?;
        let tree = parser::parse(content.as_str()).map_err(|e| {
            CompileError::new(
                format!("failed to parse '{}': {}", filename.to_str().unwrap(), e),
                self.pos.clone(),
            )
        })?;

        self.fork(name, tree).compile()
    }

    fn compile_import(&mut self, import_stmt: ImportStmt) -> Result<()> {
        self.pos = import_stmt.pos;

        let mut components = import_stmt.name.split('.').collect::<Vec<_>>();

        if components.len() < 2 {
            return Err(CompileError::new(
                format!("invalid import path: {}", import_stmt.name),
                self.pos.clone(),
            ));
        }

        let name = components.pop().unwrap();
        let module_name = components.join(".");

        if !self.module.modules.contains_key(&module_name) {
            let filename = self
                .path_finder
                .find_path(&self.fs, &components)
                .ok_or_else(|| {
                    CompileError::new(
                        format!("failed to find module: {}", module_name),
                        self.pos.clone(),
                    )
                })?;

            self.module.modules.insert(
                module_name.clone(),
                self.parse_and_compile(filename, name.to_string())?,
            );
        }

        let module = self.module.modules.get(&module_name).unwrap();
        let (type_name, type_kind, is_public) = if let Some(func) = module.funcs.get(name) {
            ("function", TypeKind::Fn, func.public)
        } else if let Some(class) = module.classes.get(name) {
            ("class", TypeKind::Class, class.public)
        } else if let Some(interface) = module.interfaces.get(name) {
            ("interface", TypeKind::Interface, interface.public)
        } else {
            return Err(CompileError::new(
                format!(
                    "failed to import unknown name '{}' from: {}",
                    name, module.name
                ),
                self.pos.clone(),
            ));
        };

        if !is_public {
            return Err(CompileError::new(
                format!("unable to import private {}: {}", type_name, name,),
                self.pos.clone(),
            ));
        }

        if self.module.globals.contains_key(name) {
            return Err(CompileError::new(
                format!("unable to redefine global: {}", name),
                self.pos.clone(),
            ));
        }

        self.module.globals.insert(
            name.to_string(),
            Type::new(type_kind, module.name.clone(), name.to_string()),
        );

        Ok(())
    }

    pub fn compile(mut self) -> Result<Module> {
        if let Some(Stmt::Module(module_stmt)) = self.tree.get(0) {
            self.module.name = module_stmt.name.clone();

            self.tree.remove(0);
        }

        // The core module shouldn't include the prelude as that would create an infinite loop
        if self.module.name != "std.core" {
            self.setup_prelude()?;
        }

        while !self.tree.is_empty() {
            let stmt = self.tree.remove(0);

            match stmt {
                Stmt::FnDecl(fn_decl) => {
                    let func = self.compile_fn(&fn_decl, false)?;

                    self.module.funcs.insert(fn_decl.name, func);
                }
                Stmt::ExternFnDecl(extern_fn_decl) => {
                    let func = self.compile_extern_fn(&extern_fn_decl);

                    self.module.funcs.insert(extern_fn_decl.name.clone(), func);
                }
                Stmt::ClassDecl(class_decl) => {
                    let class = self.compile_class(&class_decl)?;

                    self.module.classes.insert(class_decl.name, class);
                }
                Stmt::InterfaceDecl(interface_decl) => {
                    let interface = self.compile_interface(&interface_decl)?;

                    self.module
                        .interfaces
                        .insert(interface_decl.name.clone(), interface);
                }
                Stmt::Module(module_stmt) => {
                    return Err(CompileError::new(
                        "module statement must be the first statement in a file".to_string(),
                        module_stmt.pos,
                    ));
                }
                Stmt::Import(import_stmt) => self.compile_import(import_stmt)?,
                _ => unreachable!(),
            }
        }

        Ok(self.module)
    }
}
