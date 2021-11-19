use colored::{ColoredString, Colorize};
use enumflags2::BitFlags;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::compiler::LineNumberOffset;
use crate::engine::{Engine, Options};
use crate::error::Error;
use crate::runtime::Value;
use crate::syntax::{parser, Expr, FnDeclStmt, ReturnStmt, Stmt};

fn pretty_fmt_items(items: &[Value]) -> ColoredString {
    let mut output = ColoredString::default();

    for (i, item) in items.iter().enumerate() {
        output = format!("{}{}", output, pretty_fmt(item)).black();

        if i < items.len() - 1 {
            output = format!("{}{}", output, ", ".white()).black();
        }
    }

    output
}

fn pretty_fmt(value: &Value) -> ColoredString {
    match value {
        Value::Void => "!".yellow(),
        Value::Int(val) => format!("{}", *val).purple(),
        Value::Float(val) => format!("{}", *val).purple(),
        Value::Char(val) => format!("'{}'", *val).green(),
        Value::Byte(val) => format!("'{}'", *val).purple(),
        Value::Bool(val) => format!("{}", *val).blue(),
        Value::Symbol(symbol) => format!(":{}", String::from_utf8_lossy(&symbol.name)).blue(),
        Value::Ref(val) => format!("{}{}", "*".blue(), pretty_fmt(val)).white(),
        Value::Fn(func) => format!("{}(..)", func.name.blue()).white(),
        Value::Tuple(tuple) => {
            format!("{}{}{}", "(".blue(), pretty_fmt_items(tuple), ")".blue()).white()
        }
        Value::Class(class) => class.name.blue(),
        Value::Interface(interface) => interface.name.blue(),
        Value::Closure(closure) => format!("{}(..)", closure.func.name.blue()).white(),
        Value::Method(method) => format!("{}(..)", method.func.name.blue()).white(),
        Value::String(val) => format!(
            "{}{}{}",
            "\"".green(),
            val.to_string().green(),
            "\"".green()
        )
        .white(),
        Value::Object(object) => format!(
            "{}({})",
            object.class.name.blue(),
            pretty_fmt_items(object.get_fields())
        )
        .white(),
        Value::Array(array) => format!(
            "{}{}{}",
            "[".blue(),
            pretty_fmt_items(array.as_ref()),
            "]".blue()
        )
        .white(),
        Value::Option(value) => match value {
            Some(val) => format!("{}{}{}", "Some(".blue(), pretty_fmt(val), ")".blue()).white(),
            None => "None".blue(),
        },
        _ => format!("{}", value).white(),
    }
}

fn print_result(value: Option<Value>) {
    println!("{}", pretty_fmt(&value.unwrap_or(Value::Option(None))));
}

fn print_err(err: Error) {
    eprintln!("{}", format!("{}", err).red());
}

fn make_program(expr: Expr) -> Vec<Stmt> {
    vec![Stmt::FnDecl(FnDeclStmt {
        name: "main".to_string(),
        pos: expr.pos(),
        modifiers: BitFlags::default(),
        args: vec![],
        body: vec![Stmt::Return(ReturnStmt {
            pos: expr.pos(),
            expr,
        })],
        comments: vec![],
    })]
}

pub fn command() -> Result<(), Error> {
    let mut editor = Editor::<()>::new();
    let _ = editor.load_history(".atom_repl_history");

    loop {
        match editor.readline("➜ ") {
            Ok(line) => {
                let source = line.trim();

                // Try parsing an expression
                let expr = match parser::parse_expr(source) {
                    Ok(expr) => expr,
                    Err(err) => {
                        print_err(Error::from(err));
                        continue;
                    }
                };
                let mut engine = Engine::new()?;

                match engine.run_ast(
                    Options {
                        capture_result: true,
                        ..Options::default()
                    },
                    LineNumberOffset::parse(source),
                    make_program(expr),
                ) {
                    Ok(output) => {
                        print_result(output.value);
                    }
                    Err(err) => print_err(err),
                }

                editor.add_history_entry(source);
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("ReadlineError: {}", err);
                break;
            }
        }
    }

    // This isn't returned as an error as it's only a warning
    if let Err(err) = editor.save_history(".atom_repl_history") {
        eprintln!("failed to save history: {}", err);
    }

    Ok(())
}
