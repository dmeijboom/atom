use std::collections::HashMap;
use std::mem;

use wyhash2::WyHash;

use crate::ast::{Expr, FnDeclStmt, Literal, LogicalOp, Stmt, TemplateComponent, Variable};
use crate::compiler::optimizers::Optimizer;
use atom_ir::{Code, Label, Location, IR};

use super::line_number_offset::LineNumberOffset;
use super::module::Module;
use super::result::{CompileError, Result};
use super::scope::{ForLoopMeta, Local, Scope, ScopeContext};

struct ScopeCursor {
    pos: usize,
    scopes: Vec<Scope>,
}

impl ScopeCursor {
    pub fn new(scopes: Vec<Scope>) -> Self {
        Self { pos: 0, scopes }
    }

    pub fn next(&mut self, context: ScopeContext) -> Result<()> {
        self.pos += 1;

        let current_context = self.current().map(|scope| &scope.context);

        if current_context != Some(&context) {
            return Err(CompileError::new(format!(
                "invalid scope: expected '{:?}', found: {:?}",
                Some(&context),
                current_context
            )));
        }

        Ok(())
    }

    pub fn current(&self) -> Option<&Scope> {
        self.scopes.get(self.pos)
    }

    pub fn get_for_loop(&self) -> Option<&ForLoopMeta> {
        let mut id = self.pos;

        loop {
            if let Some(scope) = self.scopes.get(id) {
                if let ScopeContext::ForLoop(meta) = &scope.context {
                    return Some(meta);
                }

                if let Some(parent) = scope.parent {
                    id = parent;
                    continue;
                }
            }

            break;
        }

        None
    }

    pub fn get_local(&self, name: &str) -> Result<Local> {
        let mut id = self.pos;

        loop {
            if let Some(scope) = self.scopes.get(id) {
                if let Some(local) = scope.locals.get(name) {
                    return Ok(local.clone());
                }

                if let Some(parent) = scope.parent {
                    id = parent;
                    continue;
                }
            }

            break;
        }

        Err(CompileError::new(format!("unable to find local: {}", name)))
    }
}

pub struct BackendCompiler<'c> {
    ir: IR,
    scope: ScopeCursor,
    module: &'c mut Module,
    optimizers: Vec<Optimizer>,
    labels: HashMap<String, bool, WyHash>,
    line_number_offset: &'c LineNumberOffset,
}

impl<'c> BackendCompiler<'c> {
    pub fn new(
        module: &'c mut Module,
        scopes: Vec<Scope>,
        line_number_offset: &'c LineNumberOffset,
        optimizers: Vec<Optimizer>,
    ) -> Self {
        Self {
            ir: IR::new(),
            module,
            optimizers,
            line_number_offset,
            scope: ScopeCursor::new(scopes),
            labels: HashMap::with_hasher(WyHash::default()),
        }
    }

    fn make_label(&mut self, prefix: &str) -> String {
        let mut i: i64 = 0;

        loop {
            let label = if i == 0 {
                prefix.to_string()
            } else {
                format!("{}{}", prefix, i)
            };

            if !self.labels.contains_key(&label) {
                self.labels.insert(label.clone(), true);

                return label;
            }

            i += 1;
        }
    }

    fn compile_name(&self, name: &str) -> Result<Code> {
        Ok(if let Ok(local) = self.scope.get_local(name) {
            Code::Load(local.id)
        } else if let Some(id) = self.module.imports.get_index_of(name) {
            Code::LoadGlobal(id)
        } else if let Some(id) = self.module.funcs.get_index_of(name) {
            Code::LoadFn(id)
        } else if let Some(id) = self.module.classes.get_index_of(name) {
            Code::LoadClass(id)
        } else if let Some(id) = self.module.interfaces.get_index_of(name) {
            Code::LoadInterface(id)
        } else {
            unreachable!("no such name: {}", name)
        })
    }

    fn compile_var(&mut self, var: &Variable, location: Option<&Location>) -> Result<()> {
        match var {
            // If it's a name we can re-use the local slot
            Variable::Name(name) => {
                let local = self.scope.get_local(name)?;
                self.ir.add(Code::Store(local.id), location);
            }
            Variable::Tuple(names) | Variable::Array(names) => {
                for (i, name) in names.iter().enumerate() {
                    let local = self.scope.get_local(name)?;

                    self.ir.add(Code::ConstUint64(i as u64), location);
                    self.ir.add(
                        if i == names.len() - 1 {
                            Code::LoadIndex
                        } else {
                            Code::TeeIndex
                        },
                        location,
                    );
                    self.ir.add(Code::Store(local.id), location);
                }
            }
        };

        Ok(())
    }

    fn compile_expr_list(&mut self, expr_list: &[Expr]) -> Result<()> {
        for expr in expr_list {
            self.compile_expr(expr)?;
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<()> {
        let location = self.line_number_offset.get_location(&expr.pos());
        let location = Some(&location);

        match expr {
            Expr::Literal(literal_expr) => {
                self.ir.add(
                    match &literal_expr.literal {
                        Literal::Int128(val) => Code::ConstInt128(*val),
                        Literal::Int64(val) => Code::ConstInt64(*val),
                        Literal::Uint64(val) => Code::ConstUint64(*val),
                        Literal::Int32(val) => Code::ConstInt32(*val),
                        Literal::Byte(val) => Code::ConstByte(*val),
                        Literal::Float(val) => Code::ConstFloat(*val),
                        Literal::Bool(val) => Code::ConstBool(*val),
                        Literal::Char(val) => Code::ConstChar(*val),
                        Literal::Symbol(val) => Code::ConstSymbol(val.clone()),
                        Literal::String(val) => Code::ConstString(val.clone()),
                    },
                    location,
                );
            }
            Expr::Ident(ident) => {
                let code = self.compile_name(&ident.name)?;
                self.ir.add(code, location);
            }
            Expr::Call(call) => {
                let mut names = vec![];

                for arg in call.keyword_args.iter() {
                    names.push(arg.name.clone());
                    self.compile_expr(&arg.value)?;
                }

                for arg in call.args.iter() {
                    self.compile_expr(arg)?;
                }

                self.compile_expr(&call.callee)?;
                self.ir.add(
                    if names.is_empty() {
                        Code::Call(call.args.len())
                    } else {
                        Code::CallKeywords((names, call.keyword_args.len() + call.args.len()))
                    },
                    location,
                );
            }
            Expr::Cast(cast) => {
                self.compile_expr(&cast.expr)?;
                self.ir
                    .add(Code::Cast(cast.type_name.to_string()), location);
            }
            Expr::Not(not) => {
                self.compile_expr(&not.expr)?;
                self.ir.add(Code::Not, location);
            }
            Expr::Unwrap(unwrap) => {
                self.compile_expr(&unwrap.expr)?;
                self.ir.add(Code::Unwrap, location);
            }
            Expr::Array(array) => {
                self.compile_expr_list(&array.items)?;
                self.ir.add(Code::MakeArray(array.items.len()), location);
            }
            Expr::Tuple(tuple) => {
                self.compile_expr_list(&tuple.items)?;
                self.ir.add(Code::MakeTuple(tuple.items.len()), location);
            }
            Expr::Map(_) => unreachable!("not implemented"),
            Expr::Closure(_) => unreachable!("not implemented"),
            Expr::Member(member) => {
                self.compile_expr(&member.object)?;
                self.ir
                    .add(Code::LoadMember(member.member.clone()), location);
            }
            Expr::MemberCond(_) => unreachable!("not implemented"),
            Expr::Arithmetic(arithmetic) => {
                self.compile_expr(&arithmetic.left)?;
                self.compile_expr(&arithmetic.right)?;
                self.ir.add(arithmetic.op.into(), location);
            }
            Expr::Comparison(comparison) => {
                self.compile_expr(&comparison.left)?;
                self.compile_expr(&comparison.right)?;
                self.ir.add(comparison.op.into(), location);
            }
            Expr::Logical(logical) => {
                self.compile_expr(&logical.left)?;

                match logical.op {
                    LogicalOp::And => {
                        self.compile_expr(&logical.right)?;
                        self.ir.add(Code::LogicalAnd, location);
                    }
                    LogicalOp::Or => {
                        let label = self.make_label("or");

                        self.ir
                            .add(Code::JumpIfTrue(Label::new(label.clone())), location);
                        self.compile_expr(&logical.right)?;
                        self.ir.add(Code::SetLabel(label), location);
                    }
                }
            }
            Expr::MakeRef(make_ref) => {
                self.compile_expr(&make_ref.expr)?;
                self.ir.add(Code::MakeRef, location);
            }
            Expr::Deref(deref) => {
                self.compile_expr(&deref.expr)?;
                self.ir.add(Code::Deref, location);
            }
            Expr::Index(index) => {
                self.compile_expr(&index.object)?;

                match &index.index {
                    Expr::Range(range) => {
                        self.compile_expr(&range.from)?;
                        self.compile_expr(&range.to)?;
                        self.ir.add(Code::MakeSlice, location);
                    }
                    _ => {
                        self.compile_expr(&index.index)?;
                        self.ir.add(Code::LoadIndex, location);
                    }
                }
            }
            Expr::Range(range) => {
                self.compile_expr(&range.from)?;
                self.compile_expr(&range.to)?;
                self.ir.add(Code::MakeRange, location);
            }
            Expr::Template(template) => {
                for component in template.components.iter() {
                    match component {
                        TemplateComponent::String(val) => {
                            self.ir.add(Code::ConstString(val.clone()), location)
                        }
                        TemplateComponent::Expr(expr) => self.compile_expr(expr)?,
                    }
                }

                self.ir
                    .add(Code::MakeTemplate(template.components.len()), location);
            }
            Expr::TypeAssert(type_assert) => {
                self.compile_expr(&type_assert.left)?;
                self.compile_expr(&type_assert.right)?;
                self.ir.add(Code::AssertIsType, location);
            }
        }

        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        let location = self.line_number_offset.get_location(&stmt.pos());
        let location = Some(&location);

        match stmt {
            Stmt::If(if_stmt) => {
                self.scope.next(ScopeContext::IfElse)?;

                let if_label = self.make_label("if");
                let else_label = self.make_label("else");
                let cont_label = self.make_label("if_else_cont");

                self.compile_expr(&if_stmt.cond)?;

                self.ir.add(
                    Code::Branch((
                        Label::new(if_label.clone()),
                        Label::new(if if_stmt.alt.is_none() {
                            cont_label.clone()
                        } else {
                            else_label.clone()
                        }),
                    )),
                    location,
                );

                self.ir.add(Code::SetLabel(if_label), location);

                self.compile_stmt_list(&if_stmt.body)?;

                if let Some(alt) = &if_stmt.alt {
                    self.ir
                        .add(Code::Jump(Label::new(cont_label.clone())), location);
                    self.ir.add(Code::SetLabel(else_label), location);

                    self.compile_stmt(alt)?;

                    self.ir
                        .add(Code::Jump(Label::new(cont_label.clone())), location);
                }

                self.ir.add(Code::SetLabel(cont_label), location);
            }
            Stmt::Else(else_stmt) => {
                self.scope.next(ScopeContext::IfElse)?;
                self.compile_stmt_list(&else_stmt.body)?;
            }
            Stmt::For(for_stmt) => {
                let for_label = self.make_label("for");
                let body_label = self.make_label("for_body");
                let cont_label = self.make_label("for_cont");

                self.scope.next(ScopeContext::ForLoop(ForLoopMeta {
                    continue_label: cont_label.clone(),
                }))?;

                if let Some(expr) = &for_stmt.expr {
                    self.compile_expr(expr)?;

                    let iter = self.scope.get_local("__iter__")?;
                    let local = self.scope.get_local("__item__")?;

                    // Step 1. Get the iterator from the object
                    self.ir.add(self.compile_name("Iterable")?, location);
                    self.ir.add(Code::Validate, location);
                    self.ir.add(Code::LoadMember("iter".to_string()), location);
                    self.ir.add(Code::Call(0), location);
                    self.ir.add(Code::Store(iter.id), location);
                    // Step 2. Now in the loop, get the next value from the iterator
                    self.ir.add(Code::SetLabel(for_label), location);
                    self.ir.add(Code::Load(iter.id), location);
                    self.ir.add(Code::LoadMember("next".to_string()), location);
                    self.ir.add(Code::Call(0), location);
                    self.ir.add(Code::Store(local.id), location);
                    // Step 3. Check if it has a value and either continue or stop
                    self.ir.add(Code::Load(local.id), location);
                    self.ir
                        .add(Code::LoadMember("isSome".to_string()), location);
                    self.ir.add(Code::Call(0), location);
                    self.ir.add(
                        Code::Branch((Label::new(body_label.clone()), Label::new(cont_label))),
                        location,
                    );
                    // Step 4. Evaluate the body and so on..
                    self.ir.add(Code::SetLabel(body_label), location);

                    // Only store the current item when requested
                    if let Some(var) = &for_stmt.alias {
                        self.ir.add(Code::Load(local.id), location);
                        self.ir.add(Code::Unwrap, location);

                        self.compile_var(var, location)?;
                    }
                }

                self.compile_stmt_list(&for_stmt.body)?;
            }
            Stmt::Expr(expr_stmt) => {
                self.compile_expr(&expr_stmt.expr)?;
                self.ir.add(Code::Discard, location);
            }
            Stmt::Let(let_stmt) => {
                self.compile_expr(&let_stmt.value)?;
                self.compile_var(&let_stmt.var, location)?;
            }
            Stmt::Raise(raise) => {
                self.compile_expr(&raise.expr)?;
                self.ir.add(Code::Raise, location);
            }
            Stmt::Assign(assign) => {
                let right = assign.expand();

                match &assign.left {
                    Expr::Index(index) => {
                        self.compile_expr(&index.object)?;
                        self.compile_expr(&index.index)?;
                        self.compile_expr(&right)?;
                        self.ir.add(Code::StoreIndex, location);
                    }
                    Expr::Ident(ident) => {
                        let local = self.scope.get_local(&ident.name)?;

                        self.compile_expr(&right)?;
                        self.ir.add(local.store_instr(), location);
                    }
                    Expr::Member(member) => {
                        self.compile_expr(&right)?;
                        self.compile_expr(&member.object)?;
                        self.ir
                            .add(Code::StoreMember(member.member.clone()), location);
                    }
                    _ => unreachable!(),
                }
            }
            Stmt::Return(return_stmt) => {
                self.compile_expr(&return_stmt.expr)?;
                self.ir.add(Code::Return, location);
            }
            Stmt::Unsafe(unsafe_stmt) => {
                self.scope.next(ScopeContext::Unsafe)?;
                self.compile_stmt_list(&unsafe_stmt.body)?;
            }
            Stmt::Break(_) => {
                let meta = self.scope.get_for_loop().ok_or_else(|| {
                    CompileError::new("unable to break outside of for-loop".to_string())
                })?;

                self.ir.add(
                    Code::Jump(Label::new(meta.continue_label.clone())),
                    location,
                );
            }
            Stmt::LetDecl(_)
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

    fn compile_stmt_list(&mut self, body: &[Stmt]) -> Result<()> {
        for stmt in body {
            self.compile_stmt(stmt)?;
        }

        Ok(())
    }

    fn collect_instr(&mut self) -> IR {
        let mut ir = IR::new();
        mem::swap(&mut ir, &mut self.ir);

        for optimizer in self.optimizers.iter() {
            optimizer(self.module, &mut ir);
        }

        ir
    }

    fn compile_fn(&mut self, fn_decl: &FnDeclStmt) -> Result<IR> {
        self.scope
            .next(ScopeContext::Function(fn_decl.name.clone()))?;
        self.compile_stmt_list(&fn_decl.body)?;

        Ok(self.collect_instr())
    }

    pub fn compile(mut self, tree: &[Stmt]) -> Result<()> {
        for stmt in tree {
            match stmt {
                Stmt::FnDecl(fn_decl) => {
                    let body = self.compile_fn(fn_decl)?;

                    if let Some(func) = self.module.funcs.get_mut(&fn_decl.name) {
                        func.body = body;
                    }
                }
                Stmt::ClassDecl(class_decl) => {
                    self.scope
                        .next(ScopeContext::Class(class_decl.name.clone()))?;

                    for fn_decl in class_decl.funcs.iter() {
                        let body = self.compile_fn(fn_decl)?;

                        if let Some(class) = self.module.classes.get_mut(&class_decl.name) {
                            if let Some(method) = class.methods.get_mut(&fn_decl.name) {
                                method.body = body;
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }
}
