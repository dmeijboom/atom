use std::cell::RefCell;
use std::error::Error;
use std::fmt;
use std::rc::Rc;

use crate::ast::{ArithmeticOp, ComparisonOp, Expr, FnDeclStmt, Literal, LogicalOp, Pos, Stmt};
use crate::compiler::{Func, FuncArg, IR, Module};
use crate::compiler::scope::{Local, Scope};

#[derive(Debug)]
pub struct CompileError {
    pub pos: Pos,
    pub message: String,
}

impl CompileError {
    pub fn new(message: String, pos: Pos) -> Self {
        Self {
            message,
            pos,
        }
    }
}

impl Error for CompileError {}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}..{}", self.message, self.pos.start, self.pos.end)
    }
}

pub type Result<T> = std::result::Result<T, CompileError>;

pub struct Compiler {
    pos: Pos,
    tree: Vec<Stmt>,
    scope: Rc<RefCell<Scope>>,
}

impl Compiler {
    pub fn new(tree: Vec<Stmt>) -> Self {
        Self {
            pos: 0..0,
            tree,
            scope: Rc::new(RefCell::new(Scope::new())),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<Vec<IR>> {
        let mut ir = vec![];

        self.pos = expr.pos();

        match &expr {
            Expr::Literal(literal_expr) => match &literal_expr.literal {
                Literal::Int(val) => ir.push(vec![IR::ConstInt(*val)]),
                Literal::Bool(val) => ir.push(vec![IR::ConstBool(*val)]),
                Literal::String(val) => ir.push(vec![IR::ConstString(val.clone())]),
                Literal::Char(val) => ir.push(vec![IR::ConstChar(*val)]),
            }
            Expr::Ident(ident) => ir.push(vec![IR::Load(ident.name.clone())]),
            Expr::Call(call_expr) => {
                ir.push(self.compile_expr(&call_expr.callee)?);

                for arg in call_expr.args.iter() {
                    ir.push(self.compile_expr(arg)?);
                }

                ir.push(vec![IR::Call(call_expr.args.len())]);
            }
            Expr::Not(not_expr) => {
                ir.push(self.compile_expr(&not_expr.expr)?);
                ir.push(vec![IR::Not]);
            }
            Expr::Array(array_expr) => {
                for item in array_expr.items.iter() {
                    ir.push(self.compile_expr(item)?);
                }

                ir.push(vec![IR::MakeArray(array_expr.items.len())]);
            }
            Expr::Map(map_expr) => {
                for keyval in map_expr.key_values.iter() {
                    ir.push(self.compile_expr(&keyval.key)?);
                    ir.push(self.compile_expr(&keyval.value)?);
                }

                ir.push(vec![IR::MakeMap(map_expr.key_values.len())]);
            }
            Expr::Arithmetic(arithmetic_expr) => {
                ir.push(self.compile_expr(&arithmetic_expr.left)?);
                ir.push(self.compile_expr(&arithmetic_expr.right)?);
                ir.push(vec![match arithmetic_expr.op {
                    ArithmeticOp::Mul => IR::ArithmeticMul,
                    ArithmeticOp::Div => IR::ArithmeticDiv,
                    ArithmeticOp::Add => IR::ArithmeticAdd,
                    ArithmeticOp::Sub => IR::ArithmeticSub,
                    ArithmeticOp::BitAnd => IR::ArithmeticBitAnd,
                    ArithmeticOp::BitOr => IR::ArithmeticBitOr,
                }]);
            }
            Expr::Comparison(comparison_expr) => {
                ir.push(self.compile_expr(&comparison_expr.left)?);
                ir.push(self.compile_expr(&comparison_expr.right)?);
                ir.push(vec![match comparison_expr.op {
                    ComparisonOp::Lt => IR::ComparisonLt,
                    ComparisonOp::Lte => IR::ComparisonLte,
                    ComparisonOp::Gt => IR::ComparisonGt,
                    ComparisonOp::Gte => IR::ComparisonGte,
                    ComparisonOp::Eq => IR::ComparisonEq,
                    ComparisonOp::Neq => IR::ComparisonNeq,
                }]);
            }
            Expr::Logical(logical_expr) => {
                ir.push(self.compile_expr(&logical_expr.left)?);
                ir.push(self.compile_expr(&logical_expr.right)?);
                ir.push(vec![match logical_expr.op {
                    LogicalOp::And => IR::LogicalAnd,
                    LogicalOp::Or => IR::LogicalOr,
                }]);
            }
        };

        Ok(ir.concat())
    }

    fn compile_assign(&mut self, name: &str, value: &Expr) -> Result<Vec<IR>> {
        if let Some(local) = Scope::get_local(&self.scope, name) {
            if !local.mutable {
                return Err(CompileError::new(
                    format!("name is not mutable: {}", name),
                    self.pos.clone(),
                ));
            }

            return Ok(vec![
                self.compile_expr(value)?,
                vec![if local.mutable {
                    IR::StoreMut(name.to_string())
                } else {
                    IR::Store(name.to_string())
                }],
            ].concat());
        }

        Err(CompileError::new(
            format!("unable to assign value to unknown name: {}", name),
            self.pos.clone(),
        ))
    }

    fn compile_let(&mut self, mutable: bool, name: &str, value: Option<&Expr>) -> Result<Vec<IR>> {
        if Scope::get_local(&self.scope, name).is_some() {
            return Err(CompileError::new(
                format!("name already defined: {}", name),
                self.pos.clone(),
            ));
        } else {
            let mut scope = self.scope.borrow_mut();

            scope.set_local(Local { name: name.to_string(), mutable, is_function: false });
        }

        if let Some(expr) = value {
            return Ok(vec![self.compile_expr(expr)?, vec![if mutable {
                IR::StoreMut(name.to_string())
            } else {
                IR::Store(name.to_string())
            }]].concat());
        }

        Ok(vec![])
    }

    fn compile_stmt_list(&mut self, tree: &Vec<Stmt>) -> Result<Vec<IR>> {
        let mut ir = vec![];

        {
            let new_scope = Scope::push(Rc::clone(&self.scope));

            self.scope = Rc::new(RefCell::new(new_scope));
        }

        for stmt in tree.iter() {
            self.pos = stmt.pos();

            match stmt {
                Stmt::Expr(expr_stmt) => ir.push(self.compile_expr(&expr_stmt.expr)?),
                Stmt::Let(let_stmt) => ir.push(self.compile_let(let_stmt.mutable, &let_stmt.name, Some(&let_stmt.value))?),
                Stmt::LetDecl(let_decl_stmt) => ir.push(self.compile_let(false, &let_decl_stmt.name, None)?),
                Stmt::Assign(assign_stmt) => ir.push(self.compile_assign(&assign_stmt.name, &assign_stmt.value)?),
                _ => unreachable!(),
            }
        }

        let parent_scope = Rc::clone(self.scope.borrow().parent.as_ref().unwrap());

        self.scope = parent_scope;

        Ok(ir.concat())
    }

    fn compile_fn(&mut self, fn_decl: &FnDeclStmt) -> Result<Func> {
        if Scope::get_local(&self.scope, &fn_decl.name).is_some() {
            return Err(CompileError::new(
                format!("unable to redefine function: {}", fn_decl.name),
                self.pos.clone(),
            ));
        }

        {
            let mut scope = self.scope.borrow_mut();

            scope.set_local(Local {
                name: fn_decl.name.clone(),
                mutable: false,
                is_function: true,
            });
        }

        let body = self.compile_stmt_list(&fn_decl.body)?;

        Ok(Func {
            name: fn_decl.name.clone(),
            body,
            args: fn_decl.args.iter()
                .map(|arg| FuncArg {
                    mutable: arg.mutable,
                    name: arg.name.clone(),
                })
                .collect::<Vec<_>>(),
        })
    }

    pub fn compile(mut self) -> Result<Module> {
        let mut module = Module::new("main");

        while !self.tree.is_empty() {
            let stmt = self.tree.remove(0);

            match stmt {
                Stmt::FnDecl(fn_decl) => {
                    module.funcs.insert(fn_decl.name.clone(), self.compile_fn(&fn_decl)?);
                }
                _ => unreachable!(),
            }
        }

        Ok(module)
    }
}
