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

        rule pos() -> usize
            = position!()

        rule ident() -> String
            = name:$(['a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { name.to_string() }

        rule ident_expr() -> Expr
            = start:pos() name:ident() end:pos() { Expr::Ident(IdentExpr { name, pos: (start..end) }) }

        rule call_expr() -> Expr
            = start:pos() name:ident() "(" args:expr() ** (_ ",") ")" end:pos() { Expr::Call(CallExpr { name, args, pos: (start..end) }) }

        rule number() -> Literal
            = num:$("-"? ['0'..='9']+) { Literal::Int(num.parse().unwrap()) }

        rule literal() -> Literal
            = number()

        rule literal_expr() -> Expr
            = start:pos() literal:literal() end:pos() { Expr::Literal(LiteralExpr { literal, pos: (start..end) }) }

        rule prefix() -> Expr
            = call_expr() / ident_expr() / literal_expr()

        rule expr() -> Expr = precedence!{
            left:(@) _ "||" _ right:@ { Expr::Logical(LogicalExpr { pos: (left.pos().start..right.pos().end), left, op: LogicalOp::Or, right }.into()) }
            --
            left:(@) _ "&&" _ right:@ { Expr::Logical(LogicalExpr { pos: (left.pos().start..right.pos().end), left, op: LogicalOp::And, right }.into()) }
            --
            left:(@) _ "|" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (left.pos().start..right.pos().end), left, op: ArithmeticOp::BitOr, right }.into()) }
            left:(@) _ "&" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (left.pos().start..right.pos().end), left, op: ArithmeticOp::BitAnd, right }.into()) }
            --
            left:(@) _ "==" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (left.pos().start..right.pos().end), left, op: ComparisonOp::Eq, right }.into()) }
            left:(@) _ "!=" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (left.pos().start..right.pos().end), left, op: ComparisonOp::Neq, right }.into()) }
            --
            left:(@) _ ">" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (left.pos().start..right.pos().end), left, op: ComparisonOp::Gt, right }.into()) }
            left:(@) _ ">=" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (left.pos().start..right.pos().end), left, op: ComparisonOp::Gte, right }.into()) }
            --
            left:(@) _ "<" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (left.pos().start..right.pos().end), left, op: ComparisonOp::Lt, right }.into()) }
            left:(@) _ "<=" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (left.pos().start..right.pos().end), left, op: ComparisonOp::Lte, right }.into()) }
            --
            left:(@) _ "+" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (left.pos().start..right.pos().end), left, op: ArithmeticOp::Add, right }.into()) }
            left:(@) _ "-" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (left.pos().start..right.pos().end), left, op: ArithmeticOp::Sub, right }.into()) }
            --
            left:(@) _ "*" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (left.pos().start..right.pos().end), left, op: ArithmeticOp::Mul, right }.into()) }
            left:(@) _ "/" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (left.pos().start..right.pos().end), left, op: ArithmeticOp::Div, right }.into()) }
            --
            "!" _ expr:@ { Expr::Not(NotExpr {pos: expr.pos(),  expr: expr.into() }) }
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

