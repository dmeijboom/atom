use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

use indexmap::map::IndexMap;

use crate::ast::{
    ArithmeticOp, ClassDeclStmt, ComparisonOp, Expr, FnDeclStmt, Literal, LogicalOp, Pos, Stmt,
};
use crate::compiler::ir::Code;
use crate::compiler::module::{Class, Field};
use crate::compiler::scope::{Local, Scope};
use crate::compiler::{Func, FuncArg, LocalId, Module, IR};

#[derive(Debug)]
pub struct CompileError {
    pub pos: Pos,
    pub message: String,
}

impl CompileError {
    pub fn new(message: String, pos: Pos) -> Self {
        Self { message, pos }
    }
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at {}..{}",
            self.message, self.pos.start, self.pos.end
        )
    }
}

pub type Result<T> = std::result::Result<T, CompileError>;

pub struct Compiler {
    pos: Pos,
    scope_id: usize,
    tree: Vec<Stmt>,
    labels: Vec<String>,
    scope: Rc<RefCell<Scope>>,
}

impl Compiler {
    pub fn new(tree: Vec<Stmt>) -> Self {
        Self {
            tree,
            pos: 0..0,
            scope_id: 0,
            labels: vec![],
            scope: Rc::new(RefCell::new(Scope::new())),
        }
    }

    fn set_local(&mut self, name: String, mutable: bool) {
        let mut scope = self.scope.borrow_mut();

        scope.set_local(Local { mutable, name });
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
                },
                literal_expr.pos.clone(),
            )]),
            Expr::Range(range_expr) => {
                ir.push(self.compile_expr(&range_expr.from)?);
                ir.push(self.compile_expr(&range_expr.to)?);
                ir.push(vec![IR::new(Code::MakeRange, self.pos.clone())]);
            }
            Expr::Ident(ident) => {
                ir.push(vec![IR::new(
                    Code::Load(
                        if let Some((_, scope_id)) =
                            Scope::get_local(&self.scope, &ident.name, true)
                        {
                            LocalId::new_in_scope(ident.name.clone(), scope_id)
                        } else {
                            LocalId::new(ident.name.clone())
                        },
                    ),
                    ident.pos.clone(),
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

                ir.push(self.compile_expr(&call_expr.callee)?);
                ir.push(vec![IR::new(
                    Code::Call((names, call_expr.keyword_args.len() + call_expr.args.len())),
                    call_expr.pos.clone(),
                )]);
            }
            Expr::Not(not_expr) => {
                ir.push(self.compile_expr(&not_expr.expr)?);
                ir.push(vec![IR::new(Code::Not, not_expr.pos.clone())]);
            }
            Expr::Index(index_expr) => {
                ir.push(self.compile_expr(&index_expr.object)?);
                ir.push(self.compile_expr(&index_expr.index)?);
                ir.push(vec![IR::new(Code::LoadIndex, index_expr.pos.clone())]);
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
                for keyval in map_expr.key_values.iter() {
                    ir.push(self.compile_expr(&keyval.key)?);
                    ir.push(self.compile_expr(&keyval.value)?);
                }

                ir.push(vec![IR::new(
                    Code::MakeMap(map_expr.key_values.len()),
                    map_expr.pos.clone(),
                )]);
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
                        ArithmeticOp::BitAnd => Code::ArithmeticBitAnd,
                        ArithmeticOp::BitOr => Code::ArithmeticBitOr,
                    },
                    arithmetic_expr.pos.clone(),
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
                            Code::JumpIfTrue(label.clone()),
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
        };

        Ok(ir.concat())
    }

    fn compile_assign_local(&mut self, name: &String, value: &Expr) -> Result<Vec<IR>> {
        if let Some((local, scope_id)) = Scope::get_local(&self.scope, name, true) {
            if !local.mutable {
                return Err(CompileError::new(
                    format!("name is not mutable: {}", name),
                    self.pos.clone(),
                ));
            }

            let local_id = LocalId::new_in_scope(name.to_string(), scope_id);

            return Ok(vec![
                self.compile_expr(value)?,
                vec![IR::new(
                    if local.mutable {
                        Code::StoreMut(local_id)
                    } else {
                        Code::Store(local_id)
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
        member: &String,
        value: &Expr,
    ) -> Result<Vec<IR>> {
        let mut ir = vec![];

        ir.push(self.compile_expr(&object)?);
        ir.push(vec![IR::new(
            Code::LoadMemberPtr(member.to_string()),
            self.pos.clone(),
        )]);
        ir.push(self.compile_expr(value)?);
        ir.push(vec![IR::new(Code::StorePtr, self.pos.clone())]);

        Ok(ir.concat())
    }

    fn compile_assign(&mut self, left: &Expr, right: &Expr) -> Result<Vec<IR>> {
        match left {
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
        if Scope::get_local(&self.scope, name, false).is_some() {
            return Err(CompileError::new(
                format!("name already defined: {}", name),
                self.pos.clone(),
            ));
        } else {
            self.set_local(name.to_string(), mutable);
        }

        if let Some(expr) = value {
            let local_id = LocalId::new_in_scope(name.to_string(), self.scope.borrow().id);

            return Ok(vec![
                self.compile_expr(expr)?,
                vec![IR::new(
                    if mutable {
                        Code::StoreMut(local_id)
                    } else {
                        Code::Store(local_id)
                    },
                    self.pos.clone(),
                )],
            ]
            .concat());
        }

        Ok(vec![])
    }

    fn compile_stmt_list(&mut self, tree: &Vec<Stmt>) -> Result<Vec<IR>> {
        let mut ir = vec![];

        {
            self.scope_id += 1;
            let new_scope = Scope::new_with_parent(Rc::clone(&self.scope), self.scope_id);
            self.scope = Rc::new(RefCell::new(new_scope));
        }

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
                                if_label.clone(),
                                if if_stmt.alt.is_empty() {
                                    cont_label.clone()
                                } else {
                                    else_label.clone()
                                },
                            )),
                            self.pos.clone(),
                        ),
                        IR::new(Code::SetLabel(if_label), self.pos.clone()),
                    ]);
                    ir.push(self.compile_stmt_list(&if_stmt.body)?);

                    if if_stmt.alt.is_empty() {
                        ir.push(vec![IR::new(Code::SetLabel(cont_label), self.pos.clone())]);
                    } else {
                        ir.push(vec![IR::new(
                            Code::Jump(cont_label.clone()),
                            self.pos.clone(),
                        )]);
                        ir.push(vec![IR::new(Code::SetLabel(else_label), self.pos.clone())]);
                        ir.push(self.compile_stmt_list(&if_stmt.alt)?);
                        ir.push(vec![IR::new(
                            Code::Jump(cont_label.clone()),
                            self.pos.clone(),
                        )]);
                        ir.push(vec![IR::new(Code::SetLabel(cont_label), self.pos.clone())]);
                    }
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
                    ir.push(self.compile_assign(&assign_stmt.left, &assign_stmt.right)?)
                }
                Stmt::For(for_stmt) => {
                    let for_label = self.make_label("for");
                    let body_label = self.make_label("for_body");
                    let cont_label = self.make_label("for_cont");
                    let iter_id = LocalId::new_in_scope("#iter".to_string(), self.scope_id);
                    let iter_current_id =
                        LocalId::new_in_scope("#iter.current".to_string(), self.scope_id);

                    self.set_local(".".to_string(), false);

                    ir.push(self.compile_expr(&for_stmt.expr)?);
                    ir.push(
                        vec![
                            // step 1. Get the iterator from the object
                            Code::LoadMember("iter".to_string()),
                            Code::Call((vec![], 0)),
                            Code::Store(iter_id.clone()),
                            // step 2. Now in the loop, get the next value from the iterator
                            Code::SetLabel(for_label.clone()),
                            Code::Load(iter_id),
                            Code::LoadMember("next".to_string()),
                            Code::Call((vec![], 0)),
                            // step 3. Store the current value
                            Code::Store(iter_current_id.clone()),
                            Code::Load(iter_current_id.clone()),
                            // step 4. Check if it has a value and either continue or stop
                            Code::LoadMember("isSome".to_string()),
                            Code::Call((vec![], 0)),
                            Code::Branch((body_label.clone(), cont_label.clone())),
                            // step 5. Evaluate the body and so on..
                            Code::SetLabel(body_label.clone()),
                            Code::Load(iter_current_id),
                            Code::LoadMember("value".to_string()),
                            Code::Call((vec![], 0)),
                            Code::Store(LocalId::new_in_scope(".".to_string(), self.scope_id)),
                        ]
                        .into_iter()
                        .map(|code| IR::new(code, self.pos.clone()))
                        .collect::<Vec<_>>(),
                    );
                    ir.push(self.compile_stmt_list(&for_stmt.body)?);
                    ir.push(vec![
                        IR::new(Code::Jump(for_label), self.pos.clone()),
                        IR::new(Code::SetLabel(cont_label), self.pos.clone()),
                    ]);
                }
                Stmt::Return(return_stmt) => {
                    ir.push(self.compile_expr(&return_stmt.expr)?);
                    ir.push(vec![IR::new(Code::Return, self.pos.clone())]);
                }
                _ => unreachable!(),
            }
        }

        let parent_scope = Rc::clone(self.scope.borrow().parent.as_ref().unwrap());
        self.scope = parent_scope;

        Ok(ir.concat())
    }

    fn compile_fn(&mut self, fn_decl: &FnDeclStmt) -> Result<Func> {
        if Scope::get_local(&self.scope, &fn_decl.name, true).is_some() {
            return Err(CompileError::new(
                format!("unable to redefine function: {}", fn_decl.name),
                self.pos.clone(),
            ));
        }

        self.set_local(fn_decl.name.clone(), false);

        let body = self.compile_stmt_list(&fn_decl.body)?;

        Ok(Func {
            pos: fn_decl.pos.clone(),
            name: fn_decl.name.clone(),
            is_void: !body.iter().any(|ir| ir.code == Code::Return),
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
        if Scope::get_local(&self.scope, &class_decl.name, true).is_some() {
            return Err(CompileError::new(
                format!("unable to redefine class: {}", class_decl.name),
                self.pos.clone(),
            ));
        }

        self.set_local(class_decl.name.clone(), false);

        {
            self.scope_id += 1;
            let new_scope = Scope::new_with_parent(Rc::clone(&self.scope), self.scope_id);
            self.scope = Rc::new(RefCell::new(new_scope));
        }

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
                },
            );

            self.set_local(field.name.to_string(), field.mutable);
        }

        let mut funcs = HashMap::new();

        for fn_decl in class_decl.methods.iter() {
            funcs.insert(fn_decl.name.clone(), self.compile_fn(fn_decl)?);
        }

        let parent_scope = Rc::clone(self.scope.borrow().parent.as_ref().unwrap());
        self.scope = parent_scope;

        Ok(Class {
            name: class_decl.name.clone(),
            fields,
            funcs,
        })
    }

    pub fn compile(mut self) -> Result<Module> {
        let mut module_is_set = false;
        let mut module = Module::new("main");

        while !self.tree.is_empty() {
            let stmt = self.tree.remove(0);

            match stmt {
                Stmt::FnDecl(fn_decl) => {
                    module
                        .funcs
                        .insert(fn_decl.name.clone(), self.compile_fn(&fn_decl)?);
                }
                Stmt::ClassDecl(class_decl) => {
                    module
                        .classes
                        .insert(class_decl.name.clone(), self.compile_class(&class_decl)?);
                }
                Stmt::Module(module_stmt) => {
                    if module_is_set {
                        return Err(CompileError::new(
                            "unable to set module more than once".to_string(),
                            module_stmt.pos,
                        ));
                    }

                    module.name = module_stmt.name.clone();
                    module_is_set = true;
                }
                _ => unreachable!(),
            }
        }

        Ok(module)
    }
}
