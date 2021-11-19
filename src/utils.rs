use peg::error::ParseError;
use peg::str::LineCol;

use crate::compiler::CompileError;
use crate::runtime::RuntimeError;

#[derive(Debug, PartialEq)]
pub enum Error {
    Compile(CompileError),
    Runtime(RuntimeError),
    Parse(ParseError<LineCol>),
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
        Self::Parse(e)
    }
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

pub fn display_error(err: Error) {
    match err {
        Error::Compile(err) => eprintln!("CompileError: {}", err),
        Error::Runtime(err) => eprintln!("{}", err),
        Error::Parse(e) => display_parse_error(e),
    }
}
