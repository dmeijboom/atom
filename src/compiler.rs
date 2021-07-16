use std::error::Error;
use std::fmt;

use crate::ast::{ArithmeticOp, ComparisonOp, Expr, ExprStmt, FnDeclStmt, Literal, LogicalOp, Pos, Stmt};
use crate::ir::IR;
use crate::module::{Func, FuncArg, Module};

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
    tree: Vec<Stmt>,
}

impl Compiler {
    pub fn new(tree: Vec<Stmt>) -> Self {
        Self {
            tree,
        }
    }

    fn compile_expr(&self, expr: &Expr) -> Result<Vec<IR>> {
        Ok(match &expr {
            Expr::Literal(literal_expr) => match &literal_expr.literal {
                Literal::Int(val) => vec![IR::ConstInt(*val)],
                Literal::Bool(val) => vec![IR::ConstBool(*val)],
                Literal::String(val) => vec![IR::ConstString(val.clone())],
                Literal::Char(val) => vec![IR::ConstChar(*val)],
            }
            Expr::Ident(ident) => vec![IR::Load(ident.name.clone())],
            Expr::Call(call_expr) => {
                let mut output = vec![];

                output.append(&mut self.compile_expr(&call_expr.callee)?);

                for arg in call_expr.args.iter() {
                    output.append(&mut self.compile_expr(arg)?);
                }

                output.push(IR::Call(call_expr.args.len()));

                output
            }
            Expr::Not(not_expr) => vec![self.compile_expr(&not_expr.expr)?, vec![IR::Not]].concat(),
            Expr::Array(array_expr) => {
                let mut output = vec![];

                for item in array_expr.items.iter() {
                    output.append(&mut self.compile_expr(item)?);
                }

                output.push(IR::MakeArray(array_expr.items.len()));

                output
            }
            Expr::Map(map_expr) => {
                let mut output = vec![];

                for keyval in map_expr.key_values.iter() {
                    output.append(&mut self.compile_expr(&keyval.key)?);
                    output.append(&mut self.compile_expr(&keyval.value)?);
                }

                output.push(IR::MakeMap(map_expr.key_values.len()));

                output
            }
            Expr::Arithmetic(arithmetic_expr) => {
                let mut output = vec![];

                output.append(&mut self.compile_expr(&arithmetic_expr.left)?);
                output.append(&mut self.compile_expr(&arithmetic_expr.right)?);
                output.push(match arithmetic_expr.op {
                    ArithmeticOp::Mul => IR::ArithmeticMul,
                    ArithmeticOp::Div => IR::ArithmeticDiv,
                    ArithmeticOp::Add => IR::ArithmeticAdd,
                    ArithmeticOp::Sub => IR::ArithmeticSub,
                    ArithmeticOp::BitAnd => IR::ArithmeticBitAnd,
                    ArithmeticOp::BitOr => IR::ArithmeticBitOr,
                });

                output
            }
            Expr::Comparison(comparison_expr) => {
                let mut output = vec![];

                output.append(&mut self.compile_expr(&comparison_expr.left)?);
                output.append(&mut self.compile_expr(&comparison_expr.right)?);
                output.push(match comparison_expr.op {
                    ComparisonOp::Lt => IR::ComparisonLt,
                    ComparisonOp::Lte => IR::ComparisonLte,
                    ComparisonOp::Gt => IR::ComparisonGt,
                    ComparisonOp::Gte => IR::ComparisonGte,
                    ComparisonOp::Eq => IR::ComparisonEq,
                    ComparisonOp::Neq => IR::ComparisonNeq,
                });

                output
            }
            Expr::Logical(logical_expr) => {
                let mut output = vec![];

                output.append(&mut self.compile_expr(&logical_expr.left)?);
                output.append(&mut self.compile_expr(&logical_expr.right)?);
                output.push(match logical_expr.op {
                    LogicalOp::And => IR::LogicalAnd,
                    LogicalOp::Or => IR::LogicalOr,
                });

                output
            }
        })
    }

    fn compile_let(&self, mutable: bool, name: &str, value: Option<&Expr>) -> Result<Vec<IR>> {
        if let Some(expr) = value {
            if mutable {
                return Ok(vec![self.compile_expr(expr)?, vec![IR::StoreMut(name.to_string())]].concat());
            }

            return Ok(vec![self.compile_expr(expr)?, vec![IR::Store(name.to_string())]].concat());
        }

        Ok(vec![])
    }

    fn compile_stmt_list(&self, tree: &Vec<Stmt>) -> Result<Vec<IR>> {
        let mut ir = vec![];

        for stmt in tree.iter() {
            match stmt {
                Stmt::Expr(expr_stmt) => ir.append(&mut self.compile_expr(&expr_stmt.expr)?),
                Stmt::Let(let_stmt) => ir.append(&mut self.compile_let(let_stmt.mutable, &let_stmt.name, Some(&let_stmt.value))?),
                Stmt::LetDecl(let_decl_stmt) => ir.append(&mut self.compile_let(false, &let_decl_stmt.name, None)?),
                _ => unreachable!(),
            }
        }

        Ok(ir)
    }

    fn compile_fn(&self, fn_decl: &FnDeclStmt) -> Result<Func> {
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

    pub fn compile(self) -> Result<Module> {
        let mut module = Module::new("main");

        for stmt in self.tree.iter() {
            match stmt {
                Stmt::FnDecl(fn_decl) => {
                    module.funcs.insert(fn_decl.name.clone(), self.compile_fn(fn_decl)?);
                }
                _ => unreachable!(),
            }
        }

        Ok(module)
    }
}
