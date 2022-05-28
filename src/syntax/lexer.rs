use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

use logos::{Lexer, Logos};

macro_rules! map_err {
    ($lex:expr, $result:expr) => {
        match $result {
            Ok(value) => Ok(value),
            Err(err) => {
                $lex.extras.error = Some(err.into());
                Err(())
            }
        }
    };
}

pub enum LexerError {
    ParseInt(ParseIntError),
    ParseFloat(ParseFloatError),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ParseInt(err) => write!(f, "ParseIntError: {}", err),
            Self::ParseFloat(err) => write!(f, "ParseFloatError: {}", err),
        }
    }
}

impl From<ParseIntError> for LexerError {
    fn from(err: ParseIntError) -> Self {
        Self::ParseInt(err)
    }
}

impl From<ParseFloatError> for LexerError {
    fn from(err: ParseFloatError) -> Self {
        Self::ParseFloat(err)
    }
}

pub struct LexerExtras {
    pub line: usize,
    pub column: usize,
    pub error: Option<LexerError>,
}

impl LexerExtras {
    pub fn apply(&mut self, text: &str) {
        for c in text.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 0;

                continue;
            }

            self.column += 1;
        }
    }
}

impl Default for LexerExtras {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
            error: None,
        }
    }
}

fn parse_ident<'s, T>(lex: &mut Lexer<'s, T>) -> String
where
    T: Logos<'s, Source = str>,
{
    lex.slice().trim_matches('`').to_string()
}

fn parse_float<'s, T>(lex: &mut Lexer<'s, T>) -> Result<f64, ()>
where
    T: Logos<'s, Source = str, Extras = LexerExtras>,
{
    map_err!(lex, lex.slice().replace('_', "").parse::<f64>())
}

fn parse_int<'s, T>(lex: &mut Lexer<'s, T>) -> Result<i64, ()>
where
    T: Logos<'s, Source = str, Extras = LexerExtras>,
{
    map_err!(lex, lex.slice().replace('_', "").parse::<i64>())
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = LexerExtras)]
pub enum Token {
    #[token("<<")]
    ShiftLeft,

    #[token(">>")]
    ShiftRight,

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Mul,

    #[token("/")]
    Div,

    #[token("**")]
    Exp,

    #[token("return")]
    Return,

    #[token("fn")]
    Fn,

    #[token("let")]
    Let,

    #[token(";")]
    End,

    #[token("->")]
    ResultType,

    #[token("=>")]
    Lambda,

    #[token(".")]
    Dot,

    #[token(",")]
    Separator,

    #[token("=")]
    Eq,

    #[token("$")]
    Dollar,

    #[token("[")]
    SqrBracketLeft,

    #[token("]")]
    SqrBracketRight,

    #[token("{")]
    BracketLeft,

    #[token("}")]
    BracketRight,

    #[token("(")]
    ParentLeft,

    #[token(")")]
    ParentRight,

    #[token("::")]
    TypeSeparator,

    #[regex(r"([a-zA-Z_][a-zA-Z0-9_]*|`[^`]+`)", parse_ident)]
    Ident(String),

    #[regex(
        r"(\.[0-9](_[0-9][0-9][0-9]|[0-9])*|[0-9](_[0-9][0-9][0-9]|[0-9])*\.[0-9]+)",
        parse_float
    )]
    Float(f64),

    #[regex(r"[0-9](_[0-9][0-9][0-9]|[0-9])*", parse_int)]
    Int(i64),

    #[token("\"")]
    BeginString,

    #[error]
    #[regex(r"[ \r\n\t\f]", |lex| {
        lex.extras.apply(lex.slice());
        logos::Skip
    })]
    Error,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = LexerExtras)]
pub enum StringToken {
    #[regex(r#"(\\n|\\r|\\t|\\")"#, |lex| {
        match lex.slice() {
            "\\n" => '\n',
            "\\r" => '\r',
            "\\t" => '\t',
            "\\\"" => '"',
            _ => unreachable!(),
        }
    })]
    Escaped(char),

    #[token("${")]
    BeginInterpolation,

    #[token("}")]
    EndInterpolation,

    #[regex(r#"[^\\"|\$\{]+"#)]
    Text,

    #[token("\"")]
    End,

    #[error]
    Error,
}
