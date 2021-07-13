pub use parser::*;

use crate::ast::*;

peg::parser! {
    grammar parser() for str {
        rule whitespace()
            = [' ' | '\n']

        rule _()
            = whitespace()*

        rule __()
            = whitespace()+

        rule ident() -> String
            = name:$(['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { name.to_string() }

        rule ident_expr() -> Expr
            = name:ident() { Expr::Ident(name.to_string()) }

        rule call_expr() -> Expr
            = name:ident() "(" args:expr() ** (_ ",") ")" { Expr::Call(CallExpr { name, args }) }

        rule number_expr() -> Expr
            = num:$("-"? ['0'..='9']+) { Expr::Int(num.parse().unwrap()) }

        rule prefix() -> Expr
            = call_expr() / ident_expr() / number_expr()

        rule expr() -> Expr = precedence!{
            //left:(@) _ "||" _ right:@ { Node::new(left.pos, Expr::Logical(Logical { left, op: LogicalOp::Or, right }.into())) }
            //--
            //left:(@) _ "&&" _ right:@ { Node::new(left.pos, Expr::Logical(Logical { left, op: LogicalOp::And, right }.into())) }
            //--
            left:(@) _ "|" _ right:@ { Expr::Arithmetic(ArithmeticExpr { left, op: ArithmeticOp::BitOr, right }.into()) }
            left:(@) _ "&" _ right:@ { Expr::Arithmetic(ArithmeticExpr { left, op: ArithmeticOp::BitAnd, right }.into()) }
            --
            left:(@) _ "==" _ right:@ { Expr::Comparison(ComparisonExpr { left, op: ComparisonOp::Eq, right }.into()) }
            left:(@) _ "!=" _ right:@ { Expr::Comparison(ComparisonExpr { left, op: ComparisonOp::Neq, right }.into()) }
            --
            left:(@) _ ">" _ right:@ { Expr::Comparison(ComparisonExpr { left, op: ComparisonOp::Gt, right }.into()) }
            left:(@) _ ">=" _ right:@ { Expr::Comparison(ComparisonExpr { left, op: ComparisonOp::Gte, right }.into()) }
            --
            left:(@) _ "<" _ right:@ { Expr::Comparison(ComparisonExpr { left, op: ComparisonOp::Lt, right }.into()) }
            left:(@) _ "<=" _ right:@ { Expr::Comparison(ComparisonExpr { left, op: ComparisonOp::Lte, right }.into()) }
            --
            left:(@) _ "+" _ right:@ { Expr::Arithmetic(ArithmeticExpr { left, op: ArithmeticOp::Add, right }.into()) }
            left:(@) _ "-" _ right:@ { Expr::Arithmetic(ArithmeticExpr { left, op: ArithmeticOp::Sub, right }.into()) }
            --
            left:(@) _ "*" _ right:@ { Expr::Arithmetic(ArithmeticExpr { left, op: ArithmeticOp::Mul, right }.into()) }
            left:(@) _ "/" _ right:@ { Expr::Arithmetic(ArithmeticExpr { left, op: ArithmeticOp::Div, right }.into()) }
            --
            "!" _ expr:@ { Expr::Not(expr.into()) }
            --
            "(" _ expr:expr() _ ")" { expr }
            --
            prefix:prefix() { prefix }
        }

        rule expr_stmt() -> Stmt
            = expr:expr() _ ";" { Stmt::Expr(expr) }

        rule stmt() -> Stmt
            = expr_stmt()

        rule stmt_list() -> Vec<Stmt>
            = stmt() ** _

        pub rule parse() -> Vec<Stmt>
            = _ stmts:stmt_list() _ { stmts }
    }
}

