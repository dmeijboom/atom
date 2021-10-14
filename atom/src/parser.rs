pub use parser::*;

use crate::ast::*;

peg::parser! {
    grammar parser() for str {
        rule whitespace()
            = quiet!{[' ' | '\n' | '\r']}

        rule _()
            = whitespace()*

        rule __()
            = whitespace()+

        rule pos() -> usize
            = position!()

        rule ident() -> String
            = quiet!{ name:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { name.to_string() } }
                / expected!("identifier")

        rule variable() -> Variable
            = name:ident() { Variable::Name(name) }
                / "(" _ names:ident() ** (_ "," _) _ ")" { Variable::Tuple(names) }
                / "[" _ names:ident() ** (_ "," _) _ "]" { Variable::Array(names) }

        rule ident_expr() -> Expr
            = start:pos() name:ident() end:pos() { Expr::Ident(IdentExpr { name, pos: (start..end) }) }

        rule raw_int() -> String
            = value:$(['1'..='9'] ['0'..='9']* ("_" ['0'..='9']['0'..='9']['0'..='9'])*) { value.replace("_", "") }
                / "0" { "0".to_string() }

        rule hex_lit() -> Literal
            = "0x" value:$(['0'..='9' | 'A'..='F' | 'a'..='f']+) { Literal::Uint(u64::from_str_radix(value, 16).unwrap()) }

        rule int_lit() -> Literal
            = sign:$("-"?) value:raw_int() {
                let num = format!("{}{}", sign, value);
                let int: Result<i64, _> = num.parse();

                match int {
                    Ok(int) => Literal::Int(int),
                    Err(_) => Literal::Uint(num.parse().unwrap())
                }
            }

        rule float_lit() -> Literal
            = sign:$("-"?) pre:raw_int() "." post:$(['0'..='9']+) { Literal::Float(format!("{}{}.{}", sign, pre, post).parse().unwrap()) }
                / "." post:$(['0'..='9']+) { Literal::Float(format!(".{}", post).parse().unwrap()) }

        rule bool_lit() -> Literal
            = value:$("true" / "false") { Literal::Bool(value.parse().unwrap()) }

        rule symbol_lit() -> Literal
            = ":" name:ident() { Literal::Symbol(name.to_string()) }
                / "nil" { Literal::Symbol("nil".to_string()) }

        rule char() -> char
            = value:$([_]) { value.parse().unwrap() }

        rule char_lit() -> Literal
            = "'" value:(escaped_char() / unicode_char() / char()) "'" { Literal::Char(value) }

        rule byte_lit() -> Literal
            = "b'" value:(escaped_char() / unicode_char() / char()) "'" { Literal::Byte(value as u8) }

        rule string_lit() -> Literal
            = "\"" value:string_char()* "\"" { Literal::String(value.into_iter().collect::<String>()) }

        rule unicode_char() -> char
            = "\\u{" value:$(['0'..='9' | 'a'..='f' | 'A'..='F']+) "}" { char::from_u32(u32::from_str_radix(value, 16).unwrap()).unwrap() }

        rule escaped_char() -> char
            = "\\n" { '\n' }
                / "\\r" { '\r' }
                / "\\t" { '\t' }
                / "\\\\" { '\\' }

        rule string_char() -> char
            = unicode_char()
                / escaped_char()
                / "\\" value:$(['"']) { value.parse().unwrap() }
                / !("\"" / "\\" / ("\r"? "\n")) value:char() { value }

        rule literal() -> Literal
            = symbol_lit() / hex_lit() / bool_lit() / float_lit() / int_lit() / byte_lit() / char_lit() / string_lit()

        rule literal_expr() -> Expr
            = start:pos() literal:literal() end:pos() { Expr::Literal(LiteralExpr { literal, pos: (start..end) }) }

        rule template_char() -> char
            = "\\{" { '{' }
                / !"{" c:string_char() { c }

        rule template_component() -> TemplateComponent
            = "{" _ expr:expr() _ "}" { TemplateComponent::Expr(expr) }
                / value:template_char()+ { TemplateComponent::String(value.into_iter().collect::<String>()) }

        rule template_expr() -> Expr
            = start:pos() "f" "\"" components:template_component()* "\"" end:pos() { Expr::Template(TemplateExpr { components, pos: (start..end) }) }

        rule keyval() -> KeyValue
            = start:pos() key:expr() _ "=>" _ value:expr() end:pos() { KeyValue { key, value, pos: (start..end) } }

        rule map_expr() -> Expr
            = start:pos() "{" _ key_values:keyval() ** (_ "," _ ) _ "}" end:pos() { Expr::Map(MapExpr { key_values, pos: (start..end) }) }

        rule array_expr() -> Expr
            = start:pos() "[" _ items:expr() ** (_ "," _) _ "]" end:pos() { Expr::Array(ArrayExpr { items, pos: (start..end) }) }

        rule tuple_expr() -> Expr
            = start:pos() "(" _ items:expr() ** (_ "," _) _ ")" end:pos() { Expr::Tuple(TupleExpr { items, pos: (start..end) }) }

        rule dot_expr() -> Expr
            = start:pos() "." _ member:ident() end:pos() { Expr::Member(MemberExpr { object: Expr::Ident(IdentExpr { name: ".".to_string(), pos: (start..end) }), member, pos: (start..end) }.into()) }
                / start:pos() "." !['0'..='9'] end:pos() { Expr::Ident(IdentExpr { name: ".".to_string(), pos: (start..end) }) }

        rule closure_body() -> Vec<Stmt>
            = "{" _ body:stmt_list() _ "}" { body }
                / start:pos() expr:expr() end:pos() { vec![Stmt::Return(ReturnStmt { expr, pos: (start..end) })] }

        rule closure_expr() -> Expr
            = start:pos() "|" _ args:fn_arg() ** (_ "," _) _ "|" _ body:closure_body() end:pos() { Expr::Closure(ClosureExpr { args, body, pos: (start..end) }) }

        rule prefix() -> Expr
            = template_expr() / closure_expr() / literal_expr() / ident_expr() / dot_expr() / array_expr() / map_expr() / tuple_expr()

        rule keyword_arg() -> KeywordArg
            = start:pos() name:ident() _ ":" _ value:expr() end:pos() { KeywordArg { name, value, pos: (start..end) } }

        rule _expr(start: usize) -> Expr = precedence!{
            left:(@) _ "||" _ right:@ { Expr::Logical(LogicalExpr { pos: (start..right.pos().end), left, op: LogicalOp::Or, right }.into()) }
            --
            left:(@) _ "&&" _ right:@ { Expr::Logical(LogicalExpr { pos: (start..right.pos().end), left, op: LogicalOp::And, right }.into()) }
            --
            left:(@) _ "|" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::BitOr, right }.into()) }
            left:(@) _ "&" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::BitAnd, right }.into()) }
            left:(@) _ "^" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::BitXor, right }.into()) }
            left:(@) _ "<<" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::BitShiftLeft, right }.into()) }
            left:(@) _ ">>" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::BitShiftRight, right }.into()) }
            --
            from:(@) _ ".." _ to:@ { Expr::Range(RangeExpr { pos: (start..to.pos().end), from, to }.into()) }
            left:(@) _ "==" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (start..right.pos().end), left, op: ComparisonOp::Eq, right }.into()) }
            left:(@) _ "!=" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (start..right.pos().end), left, op: ComparisonOp::Neq, right }.into()) }
            --
            left:(@) _ ">" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (start..right.pos().end), left, op: ComparisonOp::Gt, right }.into()) }
            left:(@) _ ">=" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (start..right.pos().end), left, op: ComparisonOp::Gte, right }.into()) }
            --
            left:(@) _ "<" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (start..right.pos().end), left, op: ComparisonOp::Lt, right }.into()) }
            left:(@) _ "<=" _ right:@ { Expr::Comparison(ComparisonExpr { pos: (start..right.pos().end), left, op: ComparisonOp::Lte, right }.into()) }
            --
            left:(@) _ "is" _ right:@ { Expr::TypeAssert(TypeAssertExpr{ pos: (start..right.pos().end), left, right }.into()) }
            --
            "&" _ expr:(@) { Expr::MakeRef(MakeRefExpr { pos: (start..expr.pos().end), expr }.into()) }
            "*" _ expr:(@) { Expr::Deref(DerefExpr { pos: (start..expr.pos().end), expr }.into()) }
            --
            left:(@) _ "+" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::Add, right }.into()) }
            left:(@) _ "-" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::Sub, right }.into()) }
            --
            left:(@) _ "**" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::Exp, right }.into()) }
            left:(@) _ "*" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::Mul, right }.into()) }
            left:(@) _ "/" _ right:@ { Expr::Arithmetic(ArithmeticExpr { pos: (start..right.pos().end), left, op: ArithmeticOp::Div, right }.into()) }
            --
            expr:(@) _ "!" { Expr::Unwrap(UnwrapExpr {pos: (start..expr.pos().end), expr: expr.into()}) }
            "!" _ expr:(@) { Expr::Not(NotExpr {pos: (start..expr.pos().end), expr: expr.into() }) }
            --
            object:(@) _ "?." _ member:ident() end:pos() { Expr::MemberCond(MemberCondExpr { pos: (start..end), object, member }.into()) }
            object:(@) _ "." _ member:ident() end:pos() { Expr::Member(MemberExpr { pos: (start..end), object, member }.into()) }
            --
            object:(@) "[" _ index:expr() _ "]" end:pos() { Expr::Index(IndexExpr { pos: (start..end), object, index }.into()) }
            "(" _ type_name:ident() _ ")" _ expr:(@) { Expr::Cast(CastExpr { pos: (start..expr.pos().end), type_name, expr }.into()) }
            --
            "(" _ expr:expr() _ ")" { expr }
            --
            callee:@ "(" _ keyword_args:keyword_arg() ** (_ "," _) _ "," _ args:expr() ** (_ "," _) _ ")" end:pos() { Expr::Call(CallExpr{ keyword_args, args, pos: (callee.pos().start..end), callee }.into()) }
            callee:@ "(" _ keyword_args:keyword_arg() ** (_ "," _) _ ")" end:pos() { Expr::Call(CallExpr{ keyword_args, args: vec![], pos: (callee.pos().start..end), callee }.into()) }
            callee:@ "(" _ args:expr() ** (_ "," _) _ ")" end:pos() { Expr::Call(CallExpr{ keyword_args: vec![], args, pos: (callee.pos().start..end), callee }.into()) }
            --
            prefix:prefix() { prefix }
        }

        rule expr() -> Expr
            = start:pos() expr:_expr(start) { expr }

        rule expr_stmt() -> Stmt
            = start:pos() expr:expr() _ ";" end:pos() { Stmt::Expr(ExprStmt { expr, pos: (start..end) }) }

        rule let_decl_stmt() -> Stmt
            = start:pos() "let" __ name:ident() _ ";" end:pos() { Stmt::LetDecl(LetDeclStmt { name, pos: (start..end) }) }

        rule let_stmt() -> Stmt
            = start:pos() "let" __ mutable:("mut" __)? var:variable() _ "=" _ value:expr() _ ";" end:pos() { Stmt::Let(LetStmt { mutable: mutable.is_some(), var, value, pos: (start..end) }) }

        rule assign_op() -> Option<AssignOp>
            = "/=" { Some(AssignOp::Div) }
                / "*=" { Some(AssignOp::Mul) }
                / "+=" { Some(AssignOp::Add) }
                / "-=" { Some(AssignOp::Sub) }
                / "=" { None }

        rule assign_stmt() -> Stmt
            = start:pos() left:expr() _ op:assign_op() _ right:expr() _ ";" end:pos() { Stmt::Assign(AssignStmt { left, right, op, pos: (start..end) }) }

        rule return_stmt() -> Stmt
            = start:pos() "return" __ expr:expr() _ ";" end:pos() { Stmt::Return(ReturnStmt { expr, pos: (start..end) }) }

        rule else_stmt() -> Stmt
            = start:pos() "else" __ "{" _ body:stmt_list() _ "}" end:pos() { Stmt::Else(ElseStmt { body, pos: (start..end) }) }

        rule if_alt_stmt() -> Stmt
            = start:pos() "else if" __ cond:expr() _ "{" _ body:stmt_list() _ "}" _ alt:if_alt_stmt()? end:pos() { Stmt::If(IfStmt { cond, body, alt: alt.map(Box::new), pos: (start..end) }) }
                / else_stmt()

        rule if_stmt() -> Stmt
            = start:pos() "if" __ cond:expr() _ "{" _ body:stmt_list() _ "}" _ alt:if_alt_stmt()? end:pos() { Stmt::If(IfStmt { cond, body, alt: alt.map(Box::new), pos: (start..end) }) }

        rule for_alias() -> Variable
            = __ var:variable() __ "in" { var }

        rule for_stmt() -> Stmt
            = start:pos() "for" alias:for_alias()? __ expr:expr()? _ "{" _ body:stmt_list() _ "}" end:pos() { Stmt::For(ForStmt { expr, alias, body, pos: (start..end) }) }

        rule break_stmt() -> Stmt
            = start:pos() "break" __ label:ident() _ ";" end:pos() { Stmt::Break(BreakStmt { label: Some(label), pos: (start..end) }) }
                / start:pos() "break" _ ";" end:pos() { Stmt::Break(BreakStmt { label: None, pos: (start..end) }) }

        rule unsafe_stmt() -> Stmt
            = start:pos() "unsafe" __ "{" _ body:stmt_list() _ "}" end:pos() { Stmt::Unsafe(UnsafeStmt { body, pos: (start..end) }) }

        rule raise_stmt() -> Stmt
            = start:pos() "raise" __ expr:expr() _ ";" end:pos() { Stmt::Raise(RaiseStmt { expr, pos: (start..end) }) }

        rule comment() -> Comment
            = start:pos() "#" content:$([^ '\n']+) "\n"? end:pos() { Comment { content: content.trim().to_string(), pos: (start..end) } }

        rule stmt() -> Stmt
            = unsafe_stmt() / raise_stmt() / for_stmt() / break_stmt() / if_stmt() / return_stmt() / assign_stmt() / let_stmt() / let_decl_stmt() / expr_stmt()

        rule fn_arg() -> FnArg
            = start:pos() mutable:$("mut" _)? name:ident() end:pos() { FnArg { name, mutable: mutable.is_some(), pos: (start..end) } }

        rule fn_decl_body() -> Vec<Stmt>
            = "{" _ body:stmt_list() _ "}" { body }
                / "->" _ start:pos() expr:expr() end:pos() _ ";" { vec![Stmt::Return(ReturnStmt { expr, pos: (start..end) })] }

        rule fn_decl() -> FnDeclStmt
            = start:pos() public:$("pub")? _ "fn" __ name:ident() _ "(" _ args:fn_arg() ** (_ "," _) _ ")" _ body:fn_decl_body() end:pos() { FnDeclStmt { name, args, body, public: public.is_some(), comments: vec![], pos: (start..end) } }

        rule fn_decl_stmt() -> Stmt
            = fn_decl:fn_decl() { Stmt::FnDecl(fn_decl) }

        rule extern_fn_decl_stmt() -> Stmt
            = extern_fn_decl:extern_fn_decl() { Stmt::ExternFnDecl(extern_fn_decl) }

        rule extern_fn_decl() -> ExternFnDeclStmt
            = start:pos() public:$("pub")? _ "extern" __ "fn" __ name:ident() _ "(" args:fn_arg() ** (_ "," _) ");" end:pos() { ExternFnDeclStmt { name, args, public: public.is_some(), comments: vec![], pos: (start..end) } }

        rule interface_fn() -> InterfaceFn
            = start:pos() "fn" __ name:ident() _ "()" _ ";" end:pos() { InterfaceFn { name, pos: (start..end) } }

        rule interface_decl_stmt() -> Stmt
            = start:pos() public:$("pub" __)? _ "interface" __ name:ident() _ "{" _ functions:interface_fn() ** (_) _ "}" end:pos() { Stmt::InterfaceDecl(InterfaceDeclStmt { name, public: public.is_some(), functions, comments: vec![], pos: (start..end) }) }

        rule field() -> Field
            = start:pos() public:$("pub" __)? _ "let" __ mutable:("mut" __)? name:ident() _ "=" _ value:expr() _ ";" end:pos() { Field { mutable: mutable.is_some(), public: public.is_some(), name, value: Some(value), pos: (start..end) } }
                / start:pos() public:$("pub" __)? _ "let" __ mutable:("mut" __)? name:ident() _ ";" end:pos() { Field { mutable: mutable.is_some(), public: public.is_some(), name, value: None, pos: (start..end) } }

        rule class_extends() -> Vec<String>
            = __ "extends" __ names:ident() ** (_ "," _) { names }

        rule class_decl_stmt() -> Stmt
            = start:pos() public:$("pub" __)? _ "class" __ name:ident() extends:class_extends()? _ "{" _ fields:field() ** _ _ extern_funcs:extern_fn_decl() ** _ _ funcs:fn_decl() ** _ _ "}" end:pos() { Stmt::ClassDecl(ClassDeclStmt { name, public: public.is_some(), extends: extends.unwrap_or_default(), fields, extern_funcs, funcs, comments: vec![], pos: (start..end) }) }

        rule mixin_decl_stmt() -> Stmt
            = start:pos() public:$("pub" __)? _ "mixin" __ name:ident() _ "{" _ extern_funcs:extern_fn_decl() ** _ _ funcs:fn_decl() ** _ _ "}" end:pos() { Stmt::MixinDecl(MixinDeclStmt { name, public: public.is_some(), extern_funcs, funcs, comments: vec![], pos: (start..end) }) }

        rule module_stmt() -> Stmt
            = start:pos() "module" __ path:(ident() ** ".") _ ";" end:pos() { Stmt::Module(ModuleStmt { name: path.join("."), pos: (start..end)} ) }

        rule import_stmt() -> Stmt
            = start:pos() "import" __ path:(ident() ** ".") _ ";" end:pos() { Stmt::Import(ImportStmt { name: path.join("."), pos: (start..end)} ) }

        rule top_level_stmt() -> Stmt
            = comments:comment()* stmt:(extern_fn_decl_stmt() / fn_decl_stmt() / class_decl_stmt() / mixin_decl_stmt() / interface_decl_stmt() / module_stmt() / import_stmt()) { stmt.with_comments(comments) }

        rule stmt_list() -> Vec<Stmt>
            = comment()* stmt:stmt() ** _ { stmt }

        rule top_level_stmt_list() -> Vec<Stmt>
            = (top_level_stmt() / stmt()) ** _

        pub rule parse() -> Vec<Stmt>
            = _ stmts:top_level_stmt_list() _ { stmts }

        pub rule parse_expr() -> Expr
            = _ expr:expr() _ { expr }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Display;

    use peg::error::ParseError;
    use peg::str::LineCol;
    use test_case::test_case;

    use super::*;

    fn parse_single<L: Display>(source: &str) -> Result<Stmt, ParseError<L>>
    where
        ParseError<L>: From<ParseError<LineCol>>,
    {
        Ok(parser::parse(source)?.pop().unwrap())
    }

    #[test_case("hello", 0..5; "simple")]
    #[test_case("hello_world", 0..11; "with underscore")]
    #[test_case("hello1994", 0..9; "with numeric chars")]
    #[test_case("_test", 0..5; "starting with an underscore")]
    fn test_ident_expr(name: &str, pos: Pos) {
        let source = format!("{};", name);

        assert_eq!(
            parse_single(&source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Ident(IdentExpr {
                    name: name.to_string(),
                    pos: pos.clone(),
                }),
                pos: (pos.start..pos.end + 1),
            }))
        );
    }

    #[test_case("0;", 0, 0..1; "unsigned int zero value")]
    #[test_case("102;", 102, 0..3; "unsigned int")]
    #[test_case("-1029;", - 1029, 0..5; "signed int")]
    #[test_case("10_000;", 10000, 0..6; "unsigned int with underscore for readability")]
    #[test_case("-1_000;", - 1000, 0..6; "signed int with underscore for readability")]
    fn test_int_literals(source: &str, value: i64, pos: Pos) {
        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::Int(value),
                    pos: pos.clone(),
                }),
                pos: (pos.start..pos.end + 1),
            }))
        );
    }

    #[test_case(".0;", 0.0, 0..2; "unsigned float zero value")]
    #[test_case("10.2;", 10.2, 0..4; "unsigned float")]
    #[test_case("-10.29;", -10.29, 0..6; "signed float")]
    #[test_case("10_000.43;", 10000.43, 0..9; "unsigned float with underscore for readability")]
    #[test_case("-1_000.1;", -1000.1, 0..8; "signed float with underscore for readability")]
    fn test_float_literals(source: &str, value: f64, pos: Pos) {
        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::Float(value),
                    pos: pos.clone(),
                }),
                pos: (pos.start..pos.end + 1),
            }))
        );
    }

    #[test_case("true;", true, 0..4; "value true")]
    #[test_case("false;", false, 0..5; "value false")]
    fn test_bool_literal(source: &str, value: bool, pos: Pos) {
        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::Bool(value),
                    pos: pos.clone(),
                }),
                pos: (pos.start..pos.end + 1),
            }))
        );
    }

    #[test_case("\"hello world\";", "hello world", 0..13; "simple string")]
    #[test_case("\"hello \\\"world\";", "hello \"world", 0..15; "string with escaped char")]
    #[test_case("\"hi \\u{0060}\";", "hi `", 0..13; "string with unicode char")]
    #[test_case("\"hi \\n\";", "hi \n", 0..7; "string with escaped newline")]
    fn test_string_literal(source: &str, value: &str, pos: Pos) {
        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::String(value.to_string()),
                    pos: pos.clone(),
                }),
                pos: (pos.start..pos.end + 1),
            }))
        );
    }

    #[test_case("b'x';", b'x', 0..4; "simple byte")]
    #[test_case("b'\n';", b'\n', 0..4; "newline byte")]
    #[test_case("b'\\u{0060}';", b'`', 0..11; "unicode byte")]
    fn test_byte_literal(source: &str, value: u8, pos: Pos) {
        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::Byte(value),
                    pos: pos.clone(),
                }),
                pos: (pos.start..pos.end + 1),
            }))
        );
    }

    #[test_case("'x';", 'x', 0..3; "simple char")]
    #[test_case("'\n';", '\n', 0..3; "newline char")]
    #[test_case("'😃';", '😃', 0..6; "emoji char")]
    #[test_case("'\\u{0060}';", '`', 0..10; "unicode char")]
    fn test_char_literal(source: &str, value: char, pos: Pos) {
        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::Char(value),
                    pos: pos.clone(),
                }),
                pos: (pos.start..pos.end + 1),
            }))
        );
    }

    #[test_case("020;"; "int starting with a zero")]
    #[test_case("--10;"; "int with two dashes")]
    #[test_case("10_0"; "int with invalid underscore")]
    #[test_case("10__000"; "int with multiple underscores")]
    #[test_case("'tx';"; "char with more than one character")]
    #[test_case("'t;"; "unterminated char")]
    #[test_case("\"hello;"; "unterminated string")]
    fn test_invalid_syntax(source: &str) {
        assert_eq!(parse_single(source).is_err(), true, "parsing should fail");
    }

    #[test]
    fn test_make_ref() {
        let source = "&100;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::MakeRef(
                    MakeRefExpr {
                        expr: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(100),
                            pos: 1..4,
                        }),
                        pos: 0..4,
                    }
                    .into(),
                ),
                pos: 0..5,
            })),
        );
    }

    #[test]
    fn test_deref() {
        let source = "*100;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Deref(
                    DerefExpr {
                        expr: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(100),
                            pos: 1..4,
                        }),
                        pos: 0..4,
                    }
                    .into(),
                ),
                pos: 0..5,
            })),
        );
    }

    #[test]
    fn test_call_expr_no_args() {
        let source = "test();";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Call(
                    CallExpr {
                        callee: Expr::Ident(IdentExpr {
                            name: "test".to_string(),
                            pos: (0..4),
                        }),
                        args: vec![],
                        keyword_args: vec![],
                        pos: (0..6),
                    }
                    .into()
                ),
                pos: (0..7),
            }))
        );
    }

    #[test]
    fn test_call_expr_with_args() {
        let source = "test(10, hi);";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Call(
                    CallExpr {
                        callee: Expr::Ident(IdentExpr {
                            name: "test".to_string(),
                            pos: (0..4),
                        }),
                        args: vec![
                            Expr::Literal(LiteralExpr {
                                literal: Literal::Int(10),
                                pos: (5..7),
                            }),
                            Expr::Ident(IdentExpr {
                                name: "hi".to_string(),
                                pos: (9..11),
                            }),
                        ],
                        keyword_args: vec![],
                        pos: (0..12),
                    }
                    .into()
                ),
                pos: (0..13),
            }))
        );
    }

    #[test]
    fn test_call_expr_callee_int_with_args() {
        let source = "200(10, hi);";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Call(
                    CallExpr {
                        callee: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(200),
                            pos: (0..3),
                        }),
                        args: vec![
                            Expr::Literal(LiteralExpr {
                                literal: Literal::Int(10),
                                pos: (4..6),
                            }),
                            Expr::Ident(IdentExpr {
                                name: "hi".to_string(),
                                pos: (8..10),
                            }),
                        ],
                        keyword_args: vec![],
                        pos: (0..11),
                    }
                    .into()
                ),
                pos: (0..12),
            }))
        );
    }

    #[test_case("||", LogicalOp::Or; "or")]
    #[test_case("&&", LogicalOp::And; "and")]
    fn test_logical_expr(op_name: &str, op: LogicalOp) {
        let source = format!("1 {} 2;", op_name);

        assert_eq!(
            parse_single(&source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Logical(
                    LogicalExpr {
                        left: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(1),
                            pos: (0..1),
                        }),
                        right: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(2),
                            pos: (5..6),
                        }),
                        op,
                        pos: (0..6),
                    }
                    .into()
                ),
                pos: (0..7),
            }))
        );
    }

    #[test_case("==", ComparisonOp::Eq; "equal")]
    #[test_case("!=", ComparisonOp::Neq; "not equal")]
    #[test_case(">", ComparisonOp::Gt; "greater than")]
    #[test_case(">=", ComparisonOp::Gte; "greater than or equal")]
    #[test_case("<", ComparisonOp::Lt; "less than")]
    #[test_case("<=", ComparisonOp::Lte; "less than or equal")]
    fn test_comparison_expr(op_name: &str, op: ComparisonOp) {
        let width = op_name.len();
        let source = format!("1 {} 2;", op_name);

        assert_eq!(
            parse_single(&source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Comparison(
                    ComparisonExpr {
                        left: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(1),
                            pos: (0..1),
                        }),
                        right: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(2),
                            pos: (3 + width..4 + width),
                        }),
                        op,
                        pos: (0..4 + width),
                    }
                    .into()
                ),
                pos: (0..5 + width),
            }))
        );
    }

    #[test_case("|", ArithmeticOp::BitOr; "bit or")]
    #[test_case("&", ArithmeticOp::BitAnd; "bit and")]
    #[test_case("+", ArithmeticOp::Add; "addition")]
    #[test_case("-", ArithmeticOp::Sub; "subtraction")]
    #[test_case("*", ArithmeticOp::Mul; "multiplication")]
    #[test_case("**", ArithmeticOp::Exp; "exponent")]
    #[test_case("/", ArithmeticOp::Div; "division")]
    fn test_arithmetic_expr(op_name: &str, op: ArithmeticOp) {
        let width = op_name.len();
        let source = format!("1 {} 2;", op_name);

        assert_eq!(
            parse_single(&source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Arithmetic(
                    ArithmeticExpr {
                        left: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(1),
                            pos: (0..1),
                        }),
                        right: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(2),
                            pos: (3 + width..4 + width),
                        }),
                        op,
                        pos: (0..4 + width),
                    }
                    .into()
                ),
                pos: (0..5 + width),
            }))
        );
    }

    #[test_case("let current_year = 2021;", false, ((19..23), (0..24)); "immutable let stmt")]
    #[test_case("let mut current_year = 2021;", true, ((23..27), (0..28)); "mutable let stmt")]
    fn test_let_stmt(source: &str, mutable: bool, pos: (Pos, Pos)) {
        assert_eq!(
            parse_single(source),
            Ok(Stmt::Let(LetStmt {
                var: Variable::Name("current_year".to_string()),
                value: Expr::Literal(LiteralExpr {
                    literal: Literal::Int(2021),
                    pos: pos.0,
                }),
                mutable,
                pos: pos.1,
            }))
        );
    }

    #[test]
    fn test_assign_stmt_decl() {
        let source = "current_year = 100;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Assign(AssignStmt {
                left: Expr::Ident(IdentExpr {
                    name: "current_year".to_string(),
                    pos: (0..12),
                }),
                right: Expr::Literal(LiteralExpr {
                    literal: Literal::Int(100),
                    pos: (15..18),
                }),
                op: None,
                pos: (0..19),
            }))
        );
    }

    #[test]
    fn test_let_stmt_decl() {
        let source = "let current_year;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::LetDecl(LetDeclStmt {
                name: "current_year".to_string(),
                pos: (0..17),
            }))
        );
    }

    #[test]
    fn test_array() {
        let source = "[2021, \"hello\"];";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Array(ArrayExpr {
                    items: vec![
                        Expr::Literal(LiteralExpr {
                            literal: Literal::Int(2021),
                            pos: (1..5),
                        }),
                        Expr::Literal(LiteralExpr {
                            literal: Literal::String("hello".to_string()),
                            pos: (7..14),
                        }),
                    ],
                    pos: (0..15),
                }),
                pos: (0..16),
            }))
        );
    }

    #[test]
    fn test_map() {
        let source = "{\"num\" => 2021, \"word\" => \"hello\"};";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Map(MapExpr {
                    key_values: vec![
                        KeyValue {
                            key: Expr::Literal(LiteralExpr {
                                literal: Literal::String("num".to_string()),
                                pos: (1..6),
                            }),
                            value: Expr::Literal(LiteralExpr {
                                literal: Literal::Int(2021),
                                pos: (10..14),
                            }),
                            pos: (1..14),
                        },
                        KeyValue {
                            key: Expr::Literal(LiteralExpr {
                                literal: Literal::String("word".to_string()),
                                pos: (16..22),
                            }),
                            value: Expr::Literal(LiteralExpr {
                                literal: Literal::String("hello".to_string()),
                                pos: (26..33),
                            }),
                            pos: (16..33),
                        },
                    ],
                    pos: (0..34),
                }),
                pos: (0..35),
            }))
        );
    }

    #[test]
    fn return_stmt() {
        let source = "return 10;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Return(ReturnStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::Int(10),
                    pos: 7..9,
                }),
                pos: 0..10,
            }))
        );
    }

    #[test]
    fn if_stmt() {
        let source = "if true { 10; }";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::If(IfStmt {
                cond: Expr::Literal(LiteralExpr {
                    literal: Literal::Bool(true),
                    pos: (3..7),
                }),
                pos: (0..15),
                alt: None,
                body: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Literal(LiteralExpr {
                        literal: Literal::Int(10),
                        pos: (10..12),
                    }),
                    pos: (10..13),
                }),],
            }))
        );
    }

    #[test]
    fn if_else_stmt() {
        let source = "if true { 20; } else { 30; }";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::If(IfStmt {
                cond: Expr::Literal(LiteralExpr {
                    literal: Literal::Bool(true),
                    pos: (3..7),
                }),
                pos: (0..28),
                body: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Literal(LiteralExpr {
                        literal: Literal::Int(20),
                        pos: (10..12),
                    }),
                    pos: (10..13),
                })],
                alt: Some(
                    Stmt::Else(ElseStmt {
                        pos: (16..28),
                        body: vec![Stmt::Expr(ExprStmt {
                            expr: Expr::Literal(LiteralExpr {
                                literal: Literal::Int(30),
                                pos: (23..25),
                            }),
                            pos: (23..26),
                        })]
                    })
                    .into()
                ),
            }))
        );
    }

    #[test]
    fn class_decl_stmt() {
        let source = "class Test { let name; let mut age = 1; }";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::ClassDecl(ClassDeclStmt {
                name: "Test".to_string(),
                public: false,
                extends: vec![],
                fields: vec![
                    Field {
                        name: "name".to_string(),
                        public: false,
                        mutable: false,
                        value: None,
                        pos: (13..22),
                    },
                    Field {
                        name: "age".to_string(),
                        mutable: true,
                        public: false,
                        value: Some(Expr::Literal(LiteralExpr {
                            literal: Literal::Int(1),
                            pos: (37..38),
                        })),
                        pos: (23..39),
                    },
                ],
                funcs: vec![],
                extern_funcs: vec![],
                comments: vec![],
                pos: (0..41),
            })),
        );
    }

    #[test]
    fn mixin_decl_stmt() {
        let source = "mixin Test { fn example() {} }";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::MixinDecl(MixinDeclStmt {
                name: "Test".to_string(),
                public: false,
                funcs: vec![FnDeclStmt {
                    name: "example".to_string(),
                    public: false,
                    args: vec![],
                    body: vec![],
                    comments: vec![],
                    pos: 13..28,
                },],
                extern_funcs: vec![],
                comments: vec![],
                pos: (0..30),
            })),
        );
    }

    #[test]
    fn member_cond_expr() {
        let source = "object?.member;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::MemberCond(
                    MemberCondExpr {
                        object: Expr::Ident(IdentExpr {
                            name: "object".to_string(),
                            pos: (0..6),
                        }),
                        member: "member".to_string(),
                        pos: (0..14),
                    }
                    .into()
                ),
                pos: (0..15),
            })),
        );
    }

    #[test]
    fn member_expr() {
        let source = "object.member;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Member(
                    MemberExpr {
                        object: Expr::Ident(IdentExpr {
                            name: "object".to_string(),
                            pos: (0..6),
                        }),
                        member: "member".to_string(),
                        pos: (0..13),
                    }
                    .into()
                ),
                pos: (0..14),
            })),
        );
    }

    #[test]
    fn module_stmt() {
        let source = "module test.this.that;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Module(ModuleStmt {
                name: "test.this.that".to_string(),
                pos: (0..22),
            })),
        )
    }

    #[test]
    fn import_stmt() {
        let source = "import test.this.that;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Import(ImportStmt {
                name: "test.this.that".to_string(),
                pos: (0..22),
            })),
        )
    }

    #[test]
    fn range_expr() {
        let source = "3..10;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Range(
                    RangeExpr {
                        from: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(3),
                            pos: (0..1),
                        }),
                        to: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(10),
                            pos: (3..5),
                        }),
                        pos: (0..5),
                    }
                    .into()
                ),
                pos: (0..6),
            })),
        );
    }

    #[test]
    fn cast_expr() {
        let source = "(Int)10.0;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Cast(
                    CastExpr {
                        type_name: "Int".to_string(),
                        expr: Expr::Literal(LiteralExpr {
                            literal: Literal::Float(10.0),
                            pos: 5..9,
                        }),
                        pos: 0..9,
                    }
                    .into()
                ),
                pos: 0..10,
            })),
        );
    }

    #[test]
    fn for_stmt_with_expr_and_alias() {
        let source = "for item in [1] { 40; }";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::For(ForStmt {
                alias: Some(Variable::Name("item".to_string())),
                expr: Some(Expr::Array(ArrayExpr {
                    items: vec![Expr::Literal(LiteralExpr {
                        literal: Literal::Int(1),
                        pos: (13..14),
                    })],
                    pos: (12..15),
                })),
                body: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Literal(LiteralExpr {
                        literal: Literal::Int(40),
                        pos: (18..20),
                    }),
                    pos: (18..21),
                })],
                pos: (0..23),
            })),
        );
    }

    #[test]
    fn for_stmt_with_expr() {
        let source = "for [1] { 40; }";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::For(ForStmt {
                alias: None,
                expr: Some(Expr::Array(ArrayExpr {
                    items: vec![Expr::Literal(LiteralExpr {
                        literal: Literal::Int(1),
                        pos: (5..6),
                    })],
                    pos: (4..7),
                })),
                body: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Literal(LiteralExpr {
                        literal: Literal::Int(40),
                        pos: (10..12),
                    }),
                    pos: (10..13),
                })],
                pos: (0..15),
            })),
        );
    }

    #[test]
    fn for_stmt_without_expr() {
        let source = "for { 40; }";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::For(ForStmt {
                alias: None,
                expr: None,
                body: vec![Stmt::Expr(ExprStmt {
                    expr: Expr::Literal(LiteralExpr {
                        literal: Literal::Int(40),
                        pos: (6..8),
                    }),
                    pos: (6..9),
                })],
                pos: (0..11),
            })),
        );
    }

    #[test]
    fn break_stmt_without_label() {
        let source = "break;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Break(BreakStmt {
                label: None,
                pos: (0..6),
            }))
        );
    }

    #[test]
    fn break_stmt_with_label() {
        let source = "break upper;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Break(BreakStmt {
                label: Some("upper".to_string()),
                pos: (0..12),
            }))
        );
    }

    #[test]
    fn raise_stmt() {
        let source = "raise \"stop\";";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Raise(RaiseStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::String("stop".to_string()),
                    pos: 6..12,
                }),
                pos: 0..13,
            })),
        );
    }

    #[test]
    fn nil_lit() {
        let source = "nil;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Literal(LiteralExpr {
                    literal: Literal::Symbol("nil".to_string()),
                    pos: 0..3
                }),
                pos: 0..4,
            }))
        )
    }

    #[test]
    fn comments() {
        let source = "# test\nclass Test{}";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::ClassDecl(ClassDeclStmt {
                name: "Test".to_string(),
                public: false,
                fields: vec![],
                funcs: vec![],
                extends: vec![],
                extern_funcs: vec![],
                comments: vec![Comment {
                    content: "test".to_string(),
                    pos: 0..7,
                },],
                pos: 7..19,
            }))
        )
    }

    #[test]
    fn type_assert() {
        let source = "1 is 2;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::TypeAssert(
                    TypeAssertExpr {
                        left: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(1),
                            pos: 0..1,
                        }),
                        right: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(2),
                            pos: 5..6,
                        }),
                        pos: 0..6,
                    }
                    .into()
                ),
                pos: 0..7,
            }))
        );
    }

    #[test]
    fn unwrap_expr() {
        let source = "10!;";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Unwrap(UnwrapExpr {
                    expr: Expr::Literal(LiteralExpr {
                        literal: Literal::Int(10),
                        pos: 0..2,
                    })
                    .into(),
                    pos: 0..2,
                }),
                pos: 0..4,
            }))
        );
    }

    #[test]
    fn closure_expr() {
        let source = "|mut arg1, arg2|{ 10; };";

        assert_eq!(
            parse_single(source),
            Ok(Stmt::Expr(ExprStmt {
                expr: Expr::Closure(ClosureExpr {
                    args: vec![
                        FnArg {
                            name: "arg1".to_string(),
                            mutable: true,
                            pos: 1..9,
                        },
                        FnArg {
                            name: "arg2".to_string(),
                            mutable: false,
                            pos: 11..15,
                        },
                    ],
                    body: vec![Stmt::Expr(ExprStmt {
                        expr: Expr::Literal(LiteralExpr {
                            literal: Literal::Int(10),
                            pos: 18..20,
                        }),
                        pos: 18..21,
                    })],
                    pos: 0..23,
                }),
                pos: 0..24,
            }))
        );
    }
}
