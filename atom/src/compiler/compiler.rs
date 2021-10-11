use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;

use indexmap::map::IndexMap;

use atom_ir::{Code, Label, Location, IR};

use crate::ast::{
    ArithmeticExpr, ArithmeticOp, AssignOp, ClassDeclStmt, ClosureExpr, ComparisonOp, Expr,
    ExternFnDeclStmt, FnDeclStmt, ImportStmt, InterfaceDeclStmt, Literal, LogicalOp,
    MemberCondExpr, MixinDeclStmt, Pos, Stmt, TemplateComponent, Variable,
};
use crate::compiler::filesystem::{FileSystem, FileSystemCache};
use crate::compiler::optimizers::remove_core_validations;
use crate::parser;
use crate::std::core::DEFAULT_IMPORTS;

use super::module::{Class, Field, Func, FuncArg, Interface, Module, Type, TypeKind};
use super::optimizers::{call_void, load_local_twice_add, pre_compute_labels, Optimizer};
use super::result::{CompileError, Result};
use super::scope::{ForLoopMeta, Local, Scope, ScopeContext};

pub fn parse_line_numbers_offset(source: &str) -> Vec<usize> {
    let mut i = 0;
    let mut line_numbers_offset = vec![];

    while i < source.len() {
        if let Some(offset) = source[i + 1..].find('\n') {
            line_numbers_offset.push(offset + i);

            i += offset + 1;

            continue;
        }

        break;
    }

    line_numbers_offset
}

const STD_SOURCES: [(&str, &str); 4] = [
    ("std.core", include_str!("../std/atom/std/core.atom")),
    ("std.io", include_str!("../std/atom/std/io.atom")),
    (
        "std.encoding.utf8",
        include_str!("../std/atom/std/encoding/utf8.atom"),
    ),
    (
        "std.encoding.json",
        include_str!("../std/atom/std/encoding/json.atom"),
    ),
];

enum Imported {
    Global(TypeKind),
    Mixin(MixinDeclStmt),
}

pub struct Compiler {
    pos: Pos,
    fs: FileSystem,
    optimize: bool,
    scope_id: usize,
    module: Module,
    tree: Vec<Stmt>,
    scope: Vec<Scope>,
    labels: Vec<String>,
    optimizers: Vec<Optimizer>,
    // Unfortunately the parser doesn't expose line or column information so we're using a map of
    // newline positions to calculate the line number and column instead
    line_numbers_offset: Vec<usize>,
}

impl Compiler {
    pub fn new(tree: Vec<Stmt>, line_numbers_offset: Vec<usize>, optimize: bool) -> Self {
        let mut cache = FileSystemCache::new();

        for (module_name, source) in STD_SOURCES {
            cache.add_module(module_name.to_string(), source);
        }

        let fs = FileSystem::new(cache);

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
                    remove_core_validations::optimize,
                    pre_compute_labels::optimize,
                ]
            } else {
                vec![]
            },
            scope: vec![Scope::new()],
            line_numbers_offset,
        }
    }

    pub fn add_lookup_path(&mut self, path: impl AsRef<Path>) {
        self.fs.add_path(path.as_ref().to_path_buf());
    }

    fn get_location_by_offset(&self, offset: &Range<usize>) -> Location {
        let index = self
            .line_numbers_offset
            .iter()
            .position(|start| offset.start < *start);

        if let Some(index) = index {
            let length = self.line_numbers_offset.len();

            if length > 0 && index > 0 {
                let start = self.line_numbers_offset[index - 1];

                return Location::new(offset.clone(), index + 1, offset.start - start + 1);
            }
        }

        Location::new(offset.clone(), 1, offset.start + 1)
    }

    #[inline(always)]
    fn get_location(&self) -> Location {
        self.get_location_by_offset(&self.pos)
    }

    #[inline(always)]
    fn enter_scope(&mut self, context: ScopeContext) {
        self.scope_id += 1;

        let new_scope = Scope::new_child(context, self.scope_id);

        self.scope.push(new_scope);
    }

    #[inline(always)]
    fn exit_scope(&mut self) {
        self.scope.pop();
    }

    #[inline(always)]
    fn set_local(&mut self, name: String, mutable: bool) -> Result<Local> {
        Scope::set_local(self.scope.as_mut(), name, mutable).map_err(|e| {
            let mut e = e;

            e.location = self.get_location();
            e
        })
    }

    fn fork(&self, module_name: String, tree: Vec<Stmt>, line_numbers_offset: Vec<usize>) -> Self {
        Self {
            fs: self.fs.clone(),
            pos: 0..0,
            optimize: self.optimize,
            scope_id: 0,
            module: Module::with_name(module_name),
            tree,
            scope: vec![],
            labels: vec![],
            line_numbers_offset,
            optimizers: self.optimizers.clone(),
        }
    }

    fn get_mixin(&mut self, name: &str) -> Result<MixinDeclStmt> {
        if let Some(mixin) = self.module.mixins.get(name) {
            return Ok(mixin.clone());
        }

        let index = self.tree.iter().position(
            |stmt| matches!(stmt, Stmt::MixinDecl(mixin_decl_stmt) if mixin_decl_stmt.name == name),
        );

        if let Some(index) = index {
            let stmt = self.tree.remove(index);

            self.compile_top_level_stmt(stmt)?;

            return self.get_mixin(name);
        }

        Err(CompileError::new(
            format!("no such mixin: {}", name),
            self.get_location(),
        ))
    }

    #[inline(always)]
    fn compile_store(&self, local: &Local) -> IR {
        IR::new(
            if local.mutable {
                Code::StoreMut(local.id)
            } else {
                Code::Store(local.id)
            },
            self.get_location(),
        )
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
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
            IR::new(
                Code::Call(0),
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
            IR::new(
                Code::Branch((
                    Label::new(label_some.clone()),
                    Label::new(label_none.clone()),
                )),
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
            IR::new(
                Code::SetLabel(label_some),
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
            IR::new(
                Code::Unwrap,
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
            IR::new(
                Code::LoadMember(member_cond_expr.member.to_string()),
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
        ]);
        ir.push(body);
        ir.push(vec![
            IR::new(
                self.compile_name("some")?,
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
            IR::new(
                Code::Call(1),
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
            IR::new(
                Code::SetLabel(label_none),
                self.get_location_by_offset(&member_cond_expr.pos),
            ),
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
                    Literal::Byte(val) => Code::ConstByte(*val),
                    Literal::Int(val) => Code::ConstInt(*val),
                    Literal::Float(val) => Code::ConstFloat(*val),
                    Literal::Bool(val) => Code::ConstBool(*val),
                    Literal::Char(val) => Code::ConstChar(*val),
                    Literal::Symbol(name) => Code::ConstSymbol(name.clone()),
                    Literal::String(val) => Code::ConstString(val.clone()),
                },
                self.get_location(),
            )]),
            Expr::Template(template_expr) => {
                for component in template_expr.components.iter() {
                    ir.push(match component {
                        TemplateComponent::String(s) => {
                            vec![IR::new(Code::ConstString(s.clone()), self.get_location())]
                        }
                        TemplateComponent::Expr(expr) => self.compile_expr(expr)?,
                    });
                }

                ir.push(vec![IR::new(
                    Code::MakeTemplate(template_expr.components.len()),
                    self.get_location(),
                )]);
            }
            Expr::Range(range_expr) => {
                ir.push(self.compile_expr(&range_expr.from)?);
                ir.push(self.compile_expr(&range_expr.to)?);
                ir.push(vec![IR::new(Code::MakeRange, self.get_location())]);
            }
            Expr::Ident(ident) => {
                ir.push(vec![IR::new(
                    self.compile_name(&ident.name)?,
                    self.get_location(),
                )]);
            }
            Expr::Cast(cast_expr) => {
                ir.push(self.compile_expr(&cast_expr.expr)?);
                ir.push(vec![IR::new(
                    Code::Cast(cast_expr.type_name.clone()),
                    self.get_location(),
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
                    IR::new(Code::Call(call_expr.args.len()), self.get_location())
                } else {
                    // Make sure each keyword argument is unique
                    if !names.is_empty() {
                        let mut unique_keys = vec![];

                        for key in names.iter() {
                            if unique_keys.contains(key) {
                                return Err(CompileError::new(
                                    format!("duplicate keyword argument: {}", key),
                                    self.get_location(),
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
                        self.get_location(),
                    )
                }];

                if let Expr::MemberCond(member_cond_expr) = &call_expr.callee {
                    ir.push(self.compile_member_cond(member_cond_expr, instructions)?);
                } else {
                    let find_target_match = || {
                        if let Some((target, is_method)) = Scope::get_function_target(&self.scope) {
                            if !is_method {
                                if let Expr::Ident(ident_expr) = &call_expr.callee {
                                    return (
                                        ident_expr.name == target,
                                        self.module.get_fn_by_name(&target).is_none(),
                                    );
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

                        (false, false)
                    };

                    let (is_target, force_optimize) = find_target_match();

                    // If the target refers to a closure it can't be referenced by it's name (at least not globally).
                    // So we have to use the 'LoadTarget' instruction instead even without optimizations enabled.
                    // Otherwise it won't work
                    if (self.optimize || force_optimize) && is_target {
                        ir.push(vec![IR::new(Code::LoadTarget, self.get_location())]);
                    } else {
                        ir.push(self.compile_expr(&call_expr.callee)?);
                    }

                    ir.push(instructions);
                }
            }
            Expr::Unwrap(unwrap_expr) => {
                ir.push(self.compile_expr(&unwrap_expr.expr)?);
                ir.push(vec![IR::new(Code::Unwrap, self.get_location())]);
            }
            Expr::Not(not_expr) => {
                ir.push(self.compile_expr(&not_expr.expr)?);
                ir.push(vec![IR::new(Code::Not, self.get_location())]);
            }
            Expr::Index(index_expr) => {
                if !Scope::in_unsafe_block(&self.scope) {
                    return Err(CompileError::new(
                        "unable to perform index operation outside of an 'unsafe' block"
                            .to_string(),
                        self.get_location(),
                    ));
                }

                ir.push(self.compile_expr(&index_expr.object)?);

                let instructions = vec![IR::new(Code::LoadIndex, self.get_location())];

                if let Expr::MemberCond(member_cond_expr) = &index_expr.index {
                    ir.push(self.compile_member_cond(member_cond_expr, instructions)?);
                } else {
                    ir.push(self.compile_expr(&index_expr.index)?);
                    ir.push(instructions);
                }
            }
            Expr::Tuple(tuple_expr) => {
                for item in tuple_expr.items.iter() {
                    ir.push(self.compile_expr(item)?);
                }

                ir.push(vec![IR::new(
                    Code::MakeTuple(tuple_expr.items.len()),
                    self.get_location(),
                )]);
            }
            Expr::Array(array_expr) => {
                for item in array_expr.items.iter() {
                    ir.push(self.compile_expr(item)?);
                }

                ir.push(vec![IR::new(
                    Code::MakeArray(array_expr.items.len()),
                    self.get_location(),
                )]);
            }
            Expr::Map(map_expr) => {
                for key_val in map_expr.key_values.iter() {
                    ir.push(self.compile_expr(&key_val.key)?);
                    ir.push(self.compile_expr(&key_val.value)?);
                }

                ir.push(vec![IR::new(
                    Code::MakeMap(map_expr.key_values.len()),
                    self.get_location(),
                )]);
            }
            Expr::Closure(closure_expr) => {
                ir.push(vec![self.compile_closure(closure_expr, None)?]);
            }
            Expr::MemberCond(member_cond_expr) => {
                ir.push(self.compile_member_cond(member_cond_expr, vec![])?);
            }
            Expr::Member(member_expr) => {
                ir.push(self.compile_expr(&member_expr.object)?);
                ir.push(vec![IR::new(
                    Code::LoadMember(member_expr.member.to_string()),
                    self.get_location(),
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
                    self.get_location(),
                )]);
            }
            Expr::TypeAssert(type_assert_expr) => {
                ir.push(self.compile_expr(&type_assert_expr.left)?);
                ir.push(self.compile_expr(&type_assert_expr.right)?);
                ir.push(vec![IR::new(Code::AssertIsType, self.get_location())]);
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
                    self.get_location(),
                )]);
            }
            Expr::Logical(logical_expr) => {
                match logical_expr.op {
                    LogicalOp::And => {
                        ir.push(self.compile_expr(&logical_expr.left)?);
                        ir.push(self.compile_expr(&logical_expr.right)?);
                        ir.push(vec![IR::new(Code::LogicalAnd, self.get_location())]);
                    }
                    LogicalOp::Or => {
                        ir.push(self.compile_expr(&logical_expr.left)?);

                        let label = self.make_label("or");

                        ir.push(vec![IR::new(
                            Code::JumpIfTrue(Label::new(label.clone())),
                            self.get_location(),
                        )]);
                        ir.push(self.compile_expr(&logical_expr.right)?);
                        ir.push(vec![IR::new(Code::SetLabel(label), self.get_location())]);
                    }
                };
            }
            Expr::MakeRef(make_ref_expr) => {
                ir.push(self.compile_expr(&make_ref_expr.expr)?);
                ir.push(vec![IR::new(Code::MakeRef, self.get_location())]);
            }
            Expr::Deref(deref_expr) => {
                ir.push(self.compile_expr(&deref_expr.expr)?);
                ir.push(vec![IR::new(Code::Deref, self.get_location())]);
            }
        };

        Ok(ir.concat())
    }

    fn compile_name(&mut self, name: &str) -> Result<Code> {
        if name == "this" && Scope::in_function_block(&self.scope) {
            Ok(Code::LoadReceiver)
        } else if let Some(local) = Scope::get_local(&self.scope, name, true) {
            Ok(Code::Load(local.id))
        } else if let Some(id) = self.module.globals.get_index_of(name) {
            Ok(Code::LoadGlobal(id))
        } else if let Some(id) = self.module.funcs.iter().position(|func| func.name == name) {
            Ok(Code::LoadFn(id))
        } else if let Some(id) = self.module.classes.get_index_of(name) {
            Ok(Code::LoadClass(id))
        } else if let Some(id) = self.module.interfaces.get_index_of(name) {
            Ok(Code::LoadInterface(id))
        } else {
            let index = self.tree.iter().position(|stmt| match stmt {
                Stmt::FnDecl(fn_decl_stmt) if fn_decl_stmt.name == name => true,
                Stmt::ExternFnDecl(extern_fn_decl) if extern_fn_decl.name == name => true,
                Stmt::ClassDecl(class_decl) if class_decl.name == name => true,
                Stmt::InterfaceDecl(interface_decl) if interface_decl.name == name => true,
                _ => false,
            });

            if let Some(index) = index {
                let stmt = self.tree.remove(index);

                self.compile_top_level_stmt(stmt)?;

                return self.compile_name(name);
            }

            Err(CompileError::new(
                format!("no such name: {}", name),
                self.get_location(),
            ))
        }
    }

    fn compile_closure(
        &mut self,
        closure_expr: &ClosureExpr,
        target: Option<String>,
    ) -> Result<IR> {
        let mut args = vec![];

        self.enter_scope(ScopeContext::Function((target.unwrap_or_default(), false)));

        // As the vm will copy all the locals of the parent function we need to pretend like we set the
        // locals of the parent function
        if let Some(parent) = self.scope.get(self.scope.len() - 2) {
            self.scope.last_mut().unwrap().local_id = parent.local_id;
        }

        for arg in closure_expr.args.iter() {
            self.set_local(arg.name.clone(), arg.mutable)?;

            args.push(FuncArg {
                mutable: arg.mutable,
                name: arg.name.clone(),
            });
        }

        let body = self._compile_stmt_list(&closure_expr.body)?;

        self.exit_scope();

        let id = self.module.funcs.len();

        self.module.funcs.push(Func {
            public: true,
            is_void: !body.iter().any(|ir| ir.code == Code::Return),
            body,
            args,
            location: self.get_location(),
            name: "<closure>".to_string(),
            is_closure: true,
            is_extern: false,
        });

        Ok(IR::new(Code::MakeClosure(id), self.get_location()))
    }

    fn compile_assign_local(&mut self, name: &str, value: &Expr) -> Result<Vec<IR>> {
        if let Some(local) = Scope::get_local(&self.scope, name, true) {
            if !local.mutable {
                return Err(CompileError::new(
                    format!("name is not mutable: {}", name),
                    self.get_location(),
                ));
            }

            return Ok(vec![
                self.compile_name_value(name, value)?,
                vec![self.compile_store(&local)],
            ]
            .concat());
        }

        Err(CompileError::new(
            format!("unable to assign value to unknown name: {}", name),
            self.get_location(),
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
                self.get_location(),
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
            vec![IR::new(Code::StoreIndex, self.get_location())],
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
                self.get_location(),
            )),
        }
    }

    fn declare_local(&mut self, mutable: bool, name: &str) -> Result<Local> {
        if Scope::get_local(&self.scope, name, false).is_some() {
            Err(CompileError::new(
                format!("name already defined: {}", name),
                self.get_location(),
            ))
        } else {
            Ok(self.set_local(name.to_string(), mutable)?)
        }
    }

    fn compile_name_value(&mut self, name: &str, value: &Expr) -> Result<Vec<IR>> {
        if let Expr::Closure(closure_expr) = value {
            return Ok(vec![
                self.compile_closure(closure_expr, Some(name.to_string()))?
            ]);
        }

        self.compile_expr(value)
    }

    fn compile_stmt(&mut self, stmt: &Stmt, ir: &mut Vec<Vec<IR>>) -> Result<()> {
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
                            Label::new(if if_stmt.alt.is_none() {
                                cont_label.clone()
                            } else {
                                else_label.clone()
                            }),
                        )),
                        self.get_location(),
                    ),
                    IR::new(Code::SetLabel(if_label), self.get_location()),
                ]);
                ir.push(self.compile_stmt_list(ScopeContext::IfElse, &if_stmt.body)?);

                if let Some(alt) = &if_stmt.alt {
                    ir.push(vec![IR::new(
                        Code::Jump(Label::new(cont_label.clone())),
                        self.get_location(),
                    )]);
                    ir.push(vec![IR::new(
                        Code::SetLabel(else_label),
                        self.get_location(),
                    )]);

                    self.enter_scope(ScopeContext::IfElse);
                    self.compile_stmt(alt, ir)?;
                    self.exit_scope();

                    ir.push(vec![IR::new(
                        Code::Jump(Label::new(cont_label.clone())),
                        self.get_location(),
                    )]);
                }

                ir.push(vec![IR::new(
                    Code::SetLabel(cont_label),
                    self.get_location(),
                )]);
            }
            Stmt::Else(else_stmt) => ir.push(self._compile_stmt_list(&else_stmt.body)?),
            Stmt::Expr(expr_stmt) => {
                ir.push(self.compile_expr(&expr_stmt.expr)?);
                ir.push(vec![IR::new(Code::Discard, self.get_location())]);
            }
            Stmt::Let(let_stmt) => match &let_stmt.var {
                Variable::Name(name) => {
                    ir.push(self.compile_name_value(name, &let_stmt.value)?);

                    let local = self.declare_local(let_stmt.mutable, name)?;

                    ir.push(vec![self.compile_store(&local)]);
                }
                Variable::Tuple(names) | Variable::Array(names) => {
                    ir.push(self.compile_expr(&let_stmt.value)?);

                    for (i, name) in names.iter().enumerate() {
                        let local = self.declare_local(let_stmt.mutable, name)?;

                        ir.push(vec![
                            IR::new(Code::ConstInt(i as i64), self.get_location()),
                            // We need to keep the current value until the last item was processed
                            IR::new(
                                if i == names.len() - 1 {
                                    Code::LoadIndex
                                } else {
                                    Code::TeeIndex
                                },
                                self.get_location(),
                            ),
                            self.compile_store(&local),
                        ]);
                    }
                }
            },
            Stmt::LetDecl(let_decl_stmt) => {
                self.declare_local(true, &let_decl_stmt.name)?;
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
                            Some(Variable::Name(name)) => name.clone(),
                            _ => "__item__".to_string(),
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
                            Code::SetLabel(body_label),
                        ]
                        .into_iter()
                        .map(|code| IR::new(code, self.get_location()))
                        .collect::<Vec<_>>(),
                    );

                    // Only store the current item when requested
                    if let Some(var) = &for_stmt.alias {
                        ir.push(vec![
                            IR::new(Code::Load(local.id), self.get_location()),
                            IR::new(Code::Unwrap, self.get_location()),
                        ]);

                        match var {
                            // If it's a name we can re-use the local slot
                            Variable::Name(_) => {
                                ir.push(vec![IR::new(Code::Store(local.id), self.get_location())]);
                            }
                            Variable::Tuple(names) | Variable::Array(names) => {
                                for (i, name) in names.iter().enumerate() {
                                    let local = self.set_local(name.clone(), false)?;

                                    ir.push(vec![
                                        IR::new(Code::ConstInt(i as i64), self.get_location()),
                                        // We need to keep the current value until the last item was processed
                                        IR::new(
                                            if i == names.len() - 1 {
                                                Code::LoadIndex
                                            } else {
                                                Code::TeeIndex
                                            },
                                            self.get_location(),
                                        ),
                                        IR::new(Code::Store(local.id), self.get_location()),
                                    ]);
                                }
                            }
                        };
                    }

                    ir.push(self._compile_stmt_list(&for_stmt.body)?);
                } else {
                    self.enter_scope(ScopeContext::ForLoop(ForLoopMeta {
                        continue_label: cont_label.clone(),
                    }));

                    ir.push(vec![
                        IR::new(Code::SetLabel(for_label.clone()), self.get_location()),
                        IR::new(Code::SetLabel(body_label), self.get_location()),
                    ]);
                    ir.push(self.compile_stmt_list(
                        ScopeContext::ForLoop(ForLoopMeta {
                            continue_label: cont_label.clone(),
                        }),
                        &for_stmt.body,
                    )?);
                }

                ir.push(vec![
                    IR::new(Code::Jump(Label::new(for_label)), self.get_location()),
                    IR::new(Code::SetLabel(cont_label), self.get_location()),
                ]);

                self.exit_scope();
            }
            Stmt::Break(break_stmt) => {
                if break_stmt.label.is_some() {
                    unreachable!();
                }

                if let Some(meta) = Scope::get_for_loop(&self.scope) {
                    ir.push(vec![IR::new(
                        Code::Jump(Label::new(meta.continue_label)),
                        self.get_location(),
                    )]);
                } else {
                    return Err(CompileError::new(
                        "unable to break outside of a loop".to_string(),
                        self.get_location(),
                    ));
                }
            }
            Stmt::Raise(raise_stmt) => {
                ir.push(self.compile_expr(&raise_stmt.expr)?);
                ir.push(vec![IR::new(Code::Raise, self.get_location())]);
            }
            Stmt::Return(return_stmt) => {
                ir.push(self.compile_expr(&return_stmt.expr)?);
                ir.push(vec![IR::new(Code::Return, self.get_location())]);
            }
            Stmt::Unsafe(unsafe_stmt) => {
                ir.push(self.compile_stmt_list(ScopeContext::Unsafe, &unsafe_stmt.body)?);
            }
            // ignore top level statements
            Stmt::FnDecl(_)
            | Stmt::ExternFnDecl(_)
            | Stmt::ClassDecl(_)
            | Stmt::MixinDecl(_)
            | Stmt::InterfaceDecl(_)
            | Stmt::Module(_)
            | Stmt::Import(_) => {}
        }

        Ok(())
    }

    fn _compile_stmt_list(&mut self, tree: &[Stmt]) -> Result<Vec<IR>> {
        let mut ir = vec![];

        for stmt in tree.iter() {
            self.compile_stmt(stmt, &mut ir)?;
        }

        let mut instructions = ir.concat();

        for optimizer in self.optimizers.iter() {
            optimizer(&self.module, &mut instructions);
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
                | Code::ConstSymbol(_)
                | Code::ConstByte(_)
                | Code::ConstChar(_)
                | Code::ConstFloat(_)
                | Code::ConstInt(_)
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
            is_closure: false,
            // @TODO: this can be implemented when varargs are working
            args: vec![],
            location: self.get_location_by_offset(&extern_fn_decl.pos),
        }
    }

    fn compile_fn(&mut self, fn_decl: &FnDeclStmt, is_method: bool) -> Result<Func> {
        if self.module.get_fn_by_name(&fn_decl.name).is_some() {
            return Err(CompileError::new(
                format!("unable to redefine function: {}", fn_decl.name),
                self.get_location(),
            ));
        }

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
            location: self.get_location_by_offset(&fn_decl.pos),
            name: fn_decl.name.clone(),
            public: fn_decl.public,
            is_void: !body.iter().any(|ir| ir.code == Code::Return),
            is_extern: false,
            is_closure: false,
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

    fn compile_class(&mut self, mut class_decl: ClassDeclStmt) -> Result<()> {
        if self.module.classes.contains_key(&class_decl.name) {
            return Err(CompileError::new(
                format!("unable to redefine class: {}", class_decl.name),
                self.get_location(),
            ));
        }

        // Register class early so that it can be referenced in one of it's methods
        self.module.classes.insert(
            class_decl.name.clone(),
            Class {
                name: class_decl.name.clone(),
                public: class_decl.public,
                funcs: HashMap::new(),
                fields: IndexMap::new(),
            },
        );

        for name in class_decl.extends.iter() {
            let mut mixin = self.get_mixin(name)?;

            class_decl.funcs.append(&mut mixin.funcs);
            class_decl.extern_funcs.append(&mut mixin.extern_funcs);
        }

        self.enter_scope(ScopeContext::Class(class_decl.name.clone()));

        let mut fields = IndexMap::new();

        for field in class_decl.fields.iter() {
            if fields.contains_key(&field.name) {
                return Err(CompileError::new(
                    format!(
                        "unable to redefine field: {}.{}",
                        class_decl.name, field.name
                    ),
                    self.get_location(),
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
            if fields.contains_key(&extern_fn_decl.name) {
                return Err(CompileError::new(
                    format!("unable to define extern function '{}.{}(...)' because a field with the same name exists", class_decl.name, extern_fn_decl.name),
                    self.get_location_by_offset(&extern_fn_decl.pos),
                ));
            }

            funcs.insert(
                extern_fn_decl.name.clone(),
                self.compile_extern_fn(extern_fn_decl),
            );
        }

        for fn_decl in class_decl.funcs.iter() {
            if fields.contains_key(&fn_decl.name) {
                return Err(CompileError::new(
                    format!("unable to define extern function '{}.{}(...)' because a field with the same name exists", class_decl.name, fn_decl.name),
                    self.get_location_by_offset(&fn_decl.pos),
                ));
            }

            funcs.insert(fn_decl.name.clone(), self.compile_fn(fn_decl, true)?);
        }

        self.exit_scope();

        // Update the already-registered class with it's fields and methods
        if let Some(class) = self.module.classes.get_mut(&class_decl.name) {
            class.fields = fields;
            class.funcs = funcs;
        }

        Ok(())
    }

    fn compile_interface(&mut self, interface_decl: &InterfaceDeclStmt) -> Result<Interface> {
        if self.module.interfaces.contains_key(&interface_decl.name) {
            return Err(CompileError::new(
                format!("unable to redefine interface: {}", interface_decl.name),
                self.get_location(),
            ));
        }

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

    fn parse_and_compile(&self, name: String) -> Result<Module> {
        let file = self.fs.read_file(&name).map_err(|e| {
            CompileError::new(
                format!("failed to read '{}': {}", name, e),
                self.get_location(),
            )
        })?;
        let tree = parser::parse(file.source()).map_err(|e| {
            CompileError::new(
                format!("failed to parse module '{}': {}", name, e),
                self.get_location(),
            )
        })?;

        let line_numbers_offset = parse_line_numbers_offset(file.source());
        let mut module = self.fork(name, tree, line_numbers_offset).compile()?;

        module.filename = file
            .name()
            .and_then(|name| name.to_str().map(|filename| filename.to_string()));

        Ok(module)
    }

    fn compile_import(&mut self, import_stmt: ImportStmt) -> Result<()> {
        self.pos = import_stmt.pos;

        let mut components = import_stmt.name.split('.').collect::<Vec<_>>();

        if components.len() < 2 {
            return Err(CompileError::new(
                format!("invalid import path: {}", import_stmt.name),
                self.get_location(),
            ));
        }

        let name = components.pop().unwrap();
        let module_name = components.join(".");

        if !self.module.modules.contains_key(&module_name) {
            self.module.modules.insert(
                module_name.clone(),
                self.parse_and_compile(module_name.to_string())?,
            );
        }

        let module = self.module.modules.get(&module_name).unwrap();
        let (type_name, imported, is_public) = if let Some(func) = module.get_fn_by_name(name) {
            ("function", Imported::Global(TypeKind::Fn), func.public)
        } else if let Some(class) = module.classes.get(name) {
            ("class", Imported::Global(TypeKind::Class), class.public)
        } else if let Some(interface) = module.interfaces.get(name) {
            (
                "interface",
                Imported::Global(TypeKind::Interface),
                interface.public,
            )
        } else if let Some(mixin) = module.mixins.get(name) {
            ("mixin", Imported::Mixin(mixin.clone()), mixin.public)
        } else {
            return Err(CompileError::new(
                format!(
                    "failed to import unknown name '{}' from: {}",
                    name, module.name
                ),
                self.get_location(),
            ));
        };

        if !is_public {
            return Err(CompileError::new(
                format!("unable to import private {}: {}", type_name, name,),
                self.get_location(),
            ));
        }

        if self.module.globals.contains_key(name) {
            return Err(CompileError::new(
                format!("unable to redefine global: {}", name),
                self.get_location(),
            ));
        }

        match imported {
            Imported::Global(type_kind) => {
                self.module.globals.insert(
                    name.to_string(),
                    Type::new(type_kind, module.name.clone(), name.to_string()),
                );
            }
            Imported::Mixin(mixin) => {
                if self.module.mixins.contains_key(&mixin.name) {
                    return Err(CompileError::new(
                        format!("unable to redefine mixin: {}", name),
                        self.get_location(),
                    ));
                }

                // As mixins are being used at compile time we need to import them instead of leaving that to the vm
                self.module.mixins.insert(mixin.name.clone(), mixin);
            }
        }

        Ok(())
    }

    fn compile_top_level_stmt(&mut self, stmt: Stmt) -> Result<()> {
        match stmt {
            Stmt::FnDecl(fn_decl) => {
                let func = self.compile_fn(&fn_decl, false)?;

                self.module.funcs.push(func);
            }
            Stmt::ExternFnDecl(extern_fn_decl) => {
                let func = self.compile_extern_fn(&extern_fn_decl);

                self.module.funcs.push(func);
            }
            Stmt::MixinDecl(mixin_decl) => {
                self.module
                    .mixins
                    .insert(mixin_decl.name.clone(), mixin_decl);
            }
            Stmt::ClassDecl(class_decl) => {
                self.compile_class(class_decl)?;
            }
            Stmt::InterfaceDecl(interface_decl) => {
                let interface = self.compile_interface(&interface_decl)?;

                self.module
                    .interfaces
                    .insert(interface_decl.name, interface);
            }
            Stmt::Module(module_stmt) => {
                return Err(CompileError::new(
                    "module statement must be the first statement in a file".to_string(),
                    self.get_location_by_offset(&module_stmt.pos),
                ));
            }
            Stmt::Import(import_stmt) => self.compile_import(import_stmt)?,
            _ => unreachable!(),
        }

        Ok(())
    }

    pub fn compile(mut self) -> Result<Module> {
        if let Some(Stmt::Module(module_stmt)) = self.tree.get(0) {
            self.module.name = module_stmt.name.clone();

            self.tree.remove(0);
        }

        // The std.core module shouldn't include the prelude as that would create an infinite loop
        if self.module.name != "std.core" {
            self.setup_prelude()?;
        }

        while !self.tree.is_empty() {
            let stmt = self.tree.remove(0);

            self.compile_top_level_stmt(stmt)?;
        }

        Ok(self.module)
    }
}
