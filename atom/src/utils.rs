use std::path::PathBuf;

use peg::error::ParseError;
use peg::str::LineCol;

use crate::ast::Pos;
use crate::compiler::{CompileError, Compiler};
use crate::parser;
use crate::vm::Module;
use atom_runtime::RuntimeError;

#[derive(Debug, PartialEq)]
pub enum Error {
    Compile(CompileError),
    Runtime(RuntimeError),
    ParseError(ParseError<LineCol>),
}

impl From<CompileError> for Error {
    fn from(e: CompileError) -> Self {
        Self::Compile(e)
    }
}

impl From<RuntimeError> for Error {
    fn from(e: RuntimeError) -> Self {
        Self::Runtime(e)
    }
}

impl From<ParseError<LineCol>> for Error {
    fn from(e: ParseError<LineCol>) -> Self {
        Self::ParseError(e)
    }
}

pub fn parse_and_compile(source: &str, filename: Option<PathBuf>) -> Result<Module, Error> {
    let tree = parser::parse(source)?;
    let compiler = Compiler::new(tree, true);
    let module = compiler.compile()?;

    Ok(Module::new(module, filename))
}

pub fn parse_line_column(source: &str, pos: &Pos) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;
    let mut chars = source.chars();

    for _ in 0..pos.start {
        match chars.next() {
            Some('\n') => {
                line += 1;
                column = 1;
            }
            _ => column += 1,
        }
    }

    (line, column)
}

pub fn display_parse_error(e: ParseError<LineCol>) {
    let common_errors = vec![
        (")", "Did you forget the closing parenthesis ')'?"),
        ("}", "Did you forget to end the block with '}'?"),
        (";", "Did you forget to end the statement with ';'?"),
    ];

    eprintln!(
        "ParseError: {} on line {} at column {}",
        if e.expected.tokens().count() <= 3 {
            format!(
                "expected one of: {}",
                e.expected
                    .tokens()
                    .map(|token| match &token[1..token.len() - 1] {
                        "!=" => "not equals",
                        "==" => "equals",
                        ">=" => "greater than or equal",
                        ">" => "greater than",
                        "<" => "less than",
                        "<=" => "less than or equal",
                        "." => "dot",
                        "||" => "or",
                        "&&" => "and",
                        "+" => "addition",
                        "-" => "subtraction",
                        "/" => "division",
                        "*" => "multiplication",
                        "|" => "bit or",
                        "&" => "bit and",
                        "'0' ..= '9'" => "number",
                        "(" => "opening parenthesis",
                        ")" => "closing parenthesis",
                        "[" => "opening square bracket",
                        "]" => "closing square bracket",
                        "," => "argument separator",
                        ".." => "range",
                        _ => token,
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        } else {
            "unexpected character".to_string()
        },
        e.location.line,
        e.location.column
    );

    for (search, message) in common_errors {
        if e.expected
            .tokens()
            .any(|token| &token[1..token.len() - 1] == search)
        {
            eprintln!("  | {}", message);
        }
    }
}

pub fn display_compile_error(e: CompileError) {
    eprintln!("CompileError: {}", e);
}

pub fn display_runtime_error(main_module: &str, contents: &str, e: RuntimeError) {
    let mut message = String::new();

    if !e.stack_trace.is_empty() {
        message.push_str("Stack trace:\n");
    }

    for trace in e.stack_trace {
        if !trace.target.starts_with(main_module) {
            message.push_str(&format!(
                "  > in {}(..) at {}..{}\n",
                trace.target, trace.pos.start, trace.pos.end,
            ));

            continue;
        }

        let (line, column) = parse_line_column(contents, &trace.pos);

        message.push_str(&format!(
            "  > in {}(..) on line {} at column {}\n",
            trace.target, line, column,
        ));
    }

    message.push_str("RuntimeError: ");
    message.push_str(&e.message);

    if let Some(pos) = e.pos {
        let (line, column) = parse_line_column(contents, &pos);

        message.push_str(&format!(" on line {} at column {}", line, column));
    }

    eprintln!("{}", message);
}

pub fn display_error(source: &str, e: Error) {
    match e {
        Error::Compile(e) => display_compile_error(e),
        Error::Runtime(e) => display_runtime_error("main", source, e),
        Error::ParseError(e) => display_parse_error(e),
    }
}
