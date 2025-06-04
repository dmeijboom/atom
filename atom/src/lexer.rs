use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use serde::Serialize;

use crate::error::{IntoSpanned, SpannedError};

#[derive(Debug, Default, Clone, Copy, PartialEq, Serialize)]
pub struct Span {
    pub offset: u32,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.offset)
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Ident(String),
    Keyword(String),
    Punct(&'static str),
    NilLit,
    BoolLit(bool),
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
}

impl TokenKind {
    pub fn at(self, span: Span) -> Token {
        Token { kind: self, span }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(name) => write!(f, "{name}"),
            Self::Keyword(name) => write!(f, "{name}"),
            Self::Punct(punct) => write!(f, "{punct}"),
            Self::NilLit => write!(f, "nil"),
            Self::BoolLit(true) => write!(f, "true"),
            Self::BoolLit(false) => write!(f, "false"),
            Self::IntLit(value) => write!(f, "{}", value),
            Self::FloatLit(value) => write!(f, "{}", value),
            Self::StringLit(value) => write!(f, "\"{value}\""),
        }
    }
}

fn is_term(c: &char) -> bool {
    matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
}

fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "if" | "else"
            | "elif"
            | "for"
            // | "self"
            | "let"
            | "fn"
            | "return"
            | "new"
            | "class"
            | "import"
            | "break"
            | "continue"
            | "extern"
    )
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("failed to parse float: {0}")]
    ParseFloat(#[from] ParseFloatError),
    #[error("failed to parse int: {0}")]
    ParseInt(#[from] ParseIntError),
    #[error("invalid escape sequence at: {0}")]
    InvalidEscapeSequence(char),
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error("invalid input at: {0}")]
    InvalidInput(char),
}

pub type TokenError = SpannedError<ErrorKind>;

pub struct Lexer<'a> {
    offset: usize,
    source: &'a [char],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [char]) -> Self {
        Self { offset: 0, source }
    }

    fn cur(&self) -> Option<char> {
        self.source.get(self.offset).copied()
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.offset + 1).copied()
    }

    fn accept(&mut self, expected: char) -> bool {
        if self.cur() == Some(expected) {
            self.advance();
            return true;
        }

        false
    }

    fn advance(&mut self) {
        self.offset += 1;
    }

    fn next(&mut self) -> Option<char> {
        self.advance();
        self.source.get(self.offset - 1).copied()
    }

    fn span(&self) -> Span {
        Span {
            offset: self.offset as u32,
        }
    }

    fn term(&mut self) -> Token {
        let span = self.span();
        let mut term = String::new();

        while let Some(c) = self.cur() {
            if !is_term(&c) {
                break;
            }

            self.advance();
            term.push(c);
        }

        match term.as_str() {
            "nil" => TokenKind::NilLit,
            "true" => TokenKind::BoolLit(true),
            "false" => TokenKind::BoolLit(false),
            name if is_keyword(name) => TokenKind::Keyword(term),
            _ => TokenKind::Ident(term),
        }
        .at(span)
    }

    fn number(&mut self) -> Result<Token, TokenError> {
        let mut dot = false;
        let span = self.span();
        let mut num = String::new();

        if self.accept('-') {
            num.push('-');
        }

        while let Some(c) = self.cur() {
            match c {
                '_' => {}
                '.' if !dot && matches!(self.peek(), Some(c) if c.is_ascii_digit()) => {
                    dot = true;
                    num.push('.');
                }
                c if c.is_ascii_digit() => num.push(c),
                _ => break,
            }

            self.advance();
        }

        if dot {
            let f = num.parse().map_err(|e| ErrorKind::ParseFloat(e).at(span))?;
            Ok(TokenKind::FloatLit(f).at(span))
        } else {
            let i = num.parse().map_err(|e| ErrorKind::ParseInt(e).at(span))?;
            Ok(TokenKind::IntLit(i).at(span))
        }
    }

    fn string(&mut self, span: Span) -> Result<Token, TokenError> {
        let mut s = String::new();

        while let Some(c) = self.next() {
            match c {
                '"' => break,
                '\\' => match self.next() {
                    Some(c) => match c {
                        'n' => s.push('\n'),
                        'r' => s.push('\r'),
                        't' => s.push('\t'),
                        '0' => s.push('\0'),
                        '\\' => s.push('\\'),
                        c => return Err(ErrorKind::InvalidEscapeSequence(c).at(self.span())),
                    },
                    None => return Err(ErrorKind::UnexpectedEof.at(self.span())),
                },
                c => s.push(c),
            }
        }

        Ok(TokenKind::StringLit(s).at(span))
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, TokenError> {
        let mut tokens = vec![];

        while let Some(cur) = self.cur() {
            let span = self.span();
            let next = self.peek();

            if matches!(cur, '_' | 'a'..='z' | 'A'..='Z') {
                tokens.push(self.term());
            } else if cur.is_ascii_digit()
                || (matches!(cur, '.' | '-') && matches!(next, Some(c) if c.is_ascii_digit()))
            {
                tokens.push(self.number()?);
            } else {
                let mut single = false;
                self.advance();

                let token = match (cur, next) {
                    ('&', Some('&')) => TokenKind::Punct("&&").at(span),
                    ('|', Some('|')) => TokenKind::Punct("||").at(span),
                    ('!', Some('=')) => TokenKind::Punct("!=").at(span),
                    ('=', Some('=')) => TokenKind::Punct("==").at(span),
                    ('<', Some('=')) => TokenKind::Punct("<=").at(span),
                    ('>', Some('=')) => TokenKind::Punct(">=").at(span),
                    ('+', Some('=')) => TokenKind::Punct("+=").at(span),
                    ('-', Some('=')) => TokenKind::Punct("-=").at(span),
                    ('*', Some('=')) => TokenKind::Punct("*=").at(span),
                    ('/', Some('=')) => TokenKind::Punct("/=").at(span),
                    ('%', Some('=')) => TokenKind::Punct("%=").at(span),
                    _ => {
                        single = true;
                        match cur {
                            c if c.is_whitespace() => continue,
                            '+' => TokenKind::Punct("+").at(span),
                            '-' => TokenKind::Punct("-").at(span),
                            '*' => TokenKind::Punct("*").at(span),
                            '/' => TokenKind::Punct("/").at(span),
                            '%' => TokenKind::Punct("%").at(span),
                            '{' => TokenKind::Punct("{").at(span),
                            '}' => TokenKind::Punct("}").at(span),
                            '[' => TokenKind::Punct("[").at(span),
                            ']' => TokenKind::Punct("]").at(span),
                            '(' => TokenKind::Punct("(").at(span),
                            ')' => TokenKind::Punct(")").at(span),
                            ':' => TokenKind::Punct(":").at(span),
                            ';' => TokenKind::Punct(";").at(span),
                            '.' => TokenKind::Punct(".").at(span),
                            ',' => TokenKind::Punct(",").at(span),
                            '&' => TokenKind::Punct("&").at(span),
                            '|' => TokenKind::Punct("|").at(span),
                            '^' => TokenKind::Punct("^").at(span),
                            '!' => TokenKind::Punct("!").at(span),
                            '=' => TokenKind::Punct("=").at(span),
                            '<' => TokenKind::Punct("<").at(span),
                            '>' => TokenKind::Punct(">").at(span),
                            '"' => self.string(span)?,
                            c => return Err(ErrorKind::InvalidInput(c).at(span)),
                        }
                    }
                };

                if !single {
                    self.advance();
                }

                tokens.push(token);
            }
        }

        Ok(tokens)
    }
}
