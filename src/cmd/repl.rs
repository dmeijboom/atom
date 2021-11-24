use std::collections::HashMap;

use colored::{ColoredString, Colorize};
use enumflags2::BitFlags;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::compiler::{CompileError, LineNumberOffset};
use crate::engine::{Engine, Options};
use crate::error::Error;
use crate::runtime::{Convert, Value};
use crate::syntax::{parser, FnDeclStmt, ReturnStmt, Stmt, Variable};

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
        Value::Symbol(symbol) => format!(":{}", String::from_utf8_lossy(symbol.name.as_ref())).blue(),
        Value::Ref(val) => format!("{}{}", "*".blue(), pretty_fmt(val)).white(),
        Value::Fn(func) => format!("{}(..)", func.name.blue()).white(),
        Value::Tuple(tuple) => format!(
            "{}{}{}",
            "(".blue(),
            pretty_fmt_items(tuple.as_ref()),
            ")".blue()
        )
        .white(),
        Value::Class(class) => class.name.blue(),
        Value::Interface(interface) => interface.name.blue(),
        Value::Closure(closure) => format!("{}(..)", closure.func.name.blue()).white(),
        Value::Method(method) => format!("{}(..)", method.func.name.blue()).white(),
        Value::String(_) => {
            let s: &str = value.convert().unwrap();

            format!("{}{}{}", "\"".green(), s.green(), "\"".green()).white()
        }
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

fn print_err(err: Error) {
    eprintln!("{}", format!("{}", err).red());
}

pub struct Repl {
    vars: HashMap<String, Value>,
}

impl Repl {
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    fn make_program(&self, stmt: Stmt) -> Vec<Stmt> {
        vec![Stmt::FnDecl(FnDeclStmt {
            name: "main".to_string(),
            pos: stmt.pos(),
            modifiers: BitFlags::default(),
            args: vec![],
            body: vec![stmt],
            comments: vec![],
        })]
    }

    fn parse_input(&self, source: &str) -> Result<Stmt, Error> {
        match parser::parse_expr(source) {
            Ok(expr) => Ok(Stmt::Return(ReturnStmt {
                pos: expr.pos(),
                expr,
            })),
            Err(err) => Ok(parser::parse_stmt(source).map_err(|_| err)?),
        }
    }

    fn input(&mut self, source: &str) {
        let stmt = match self.parse_input(source) {
            Ok(stmt) => stmt,
            Err(err) => {
                print_err(err);
                return;
            }
        };

        if let Err(err) = self.handle_input(source, stmt) {
            print_err(err);
        }
    }

    fn handle_input(&mut self, source: &str, stmt: Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Let(let_stmt) => {
                if let Variable::Name(name) = let_stmt.var {
                    let value = self
                        .run_program(
                            source,
                            self.make_program(Stmt::Return(ReturnStmt {
                                pos: let_stmt.value.pos(),
                                expr: let_stmt.value,
                            })),
                        )?
                        .unwrap();

                    println!("{}", pretty_fmt(&value));

                    self.vars.insert(name, value);

                    return Ok(());
                }

                Err(Error::from(CompileError::new(
                    "invalid left-hand assignment (only names are supported in the REPL)"
                        .to_string(),
                )))
            }
            _ => {
                let value = self.run_program(source, self.make_program(stmt))?;

                println!("{}", pretty_fmt(&value.unwrap_or(Value::Option(None))));

                Ok(())
            }
        }
    }

    fn run_program(&self, source: &str, tree: Vec<Stmt>) -> Result<Option<Value>, Error> {
        let mut engine = Engine::new()?;
        let output = engine.run_ast(
            Options {
                capture_result: true,
                ..Options::default()
            },
            LineNumberOffset::parse(source),
            tree,
        )?;

        Ok(output.value)
    }
}

pub fn command() -> Result<(), Error> {
    let mut repl = Repl::new();
    let mut editor = Editor::<()>::new();
    let _ = editor.load_history(".atom_repl_history");

    loop {
        match editor.readline(&"➜ ".yellow().to_string()) {
            Ok(line) => {
                let source = line.trim();

                if source.is_empty() {
                    continue;
                }

                repl.input(source);
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
