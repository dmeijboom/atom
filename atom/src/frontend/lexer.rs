use std::{
    fmt::Display,
    num::{IntErrorKind, ParseFloatError, ParseIntError},
    ops::Deref,
};

use serde::Serialize;

use crate::{
    error::{IntoSpanned, SpannedError},
    runtime::{self, BigInt},
};

#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct Span {
    pub offset: u32,
    pub line_number: u32,
}

impl Default for Span {
    fn default() -> Self {
        Self {
            offset: 0,
            line_number: 1,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line_number, self.offset)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
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
    Atom(String),
    Int(i64),
    BigInt(BigInt),
    Float(f64),
    String(String),
}

impl TokenKind {
    pub fn at(self, span: Span) -> Token {
        Token { kind: self, span }
    }

    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(_))
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(name) => write!(f, "{name}"),
            Self::Keyword(name) => write!(f, "{name}"),
            Self::Punct(punct) => write!(f, "{punct}"),
            Self::Atom(name) => write!(f, ":{name}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::BigInt(value) => write!(f, "{value}"),
            Self::Float(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "\"{value}\""),
        }
    }
}

fn is_term(c: char) -> bool {
    matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
}

fn is_keyword(s: &str) -> bool {
    matches!(
        s,
        "if" | "else"
            | "elif"
            | "for"
            | "pub"
            | "let"
            | "fn"
            | "is"
            | "in"
            | "return"
            | "match"
            | "new"
            | "yield"
            | "class"
            | "import"
            | "break"
            | "continue"
    )
}

fn is_atom(cur: char, next: Option<char>) -> bool {
    cur == ':' && matches!(next, Some('a'..='z' | 'A'..='Z'))
}

fn is_ident(cur: char) -> bool {
    matches!(cur, '_' | 'a'..='z' | 'A'..='Z')
}

fn is_number(cur: char, next: Option<char>) -> bool {
    cur.is_ascii_digit()
        || (matches!(cur, '.' | '-') && matches!(next, Some(c) if c.is_ascii_digit()))
}

fn is_comment(cur: char, next: Option<char>) -> bool {
    cur == '/' && next == Some('/')
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("failed to parse float: {0}")]
    ParseFloat(#[from] ParseFloatError),
    #[error("failed to parse int: {0}")]
    ParseInt(#[from] ParseIntError),
    #[error("failed to parse bigint: {0}")]
    ParseBigInt(#[from] runtime::ParseIntError),
    #[error("invalid escape sequence at: {0}")]
    InvalidEscapeSequence(char),
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error("invalid input at: {0}")]
    InvalidInput(char),
}

pub type TokenError = SpannedError<ErrorKind>;

pub struct Lexer<'a> {
    span: Span,
    offset: usize,
    source: &'a [char],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [char]) -> Self {
        Self {
            span: Span::default(),
            offset: 0,
            source,
        }
    }

    fn is_eof(&self) -> bool {
        self.offset >= self.source.len()
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
        if self.cur() == Some('\n') {
            self.span.line_number += 1;
            self.span.offset = 0;
        }

        self.offset += 1;
        self.span.offset += 1;
    }

    fn next(&mut self) -> Option<char> {
        self.advance();
        self.source.get(self.offset - 1).copied()
    }

    fn term(&mut self) -> String {
        let mut term = String::new();

        while let Some(c) = self.cur() {
            if !is_term(c) {
                break;
            }

            self.advance();
            term.push(c);
        }

        term
    }

    fn atom(&mut self) -> Token {
        let span = self.span;
        self.advance();
        TokenKind::Atom(self.term()).at(span)
    }

    fn ident_or_keyword(&mut self) -> Token {
        let span = self.span;
        let term = self.term();

        if is_keyword(&term) {
            TokenKind::Keyword(term)
        } else {
            TokenKind::Ident(term)
        }
        .at(span)
    }

    fn comment(&mut self) {
        while !self.accept('\n') && !self.is_eof() {
            self.advance();
        }
    }

    fn number(&mut self) -> Result<Token, TokenError> {
        let mut dot = false;
        let span = self.span;
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
            Ok(TokenKind::Float(f).at(span))
        } else {
            match num.parse() {
                Ok(i) => Ok(TokenKind::Int(i).at(span)),
                Err(e) => match e.kind() {
                    IntErrorKind::PosOverflow | IntErrorKind::NegOverflow => {
                        let integer =
                            BigInt::parse(&num).map_err(|e| ErrorKind::ParseBigInt(e).at(span))?;
                        Ok(TokenKind::BigInt(integer).at(span))
                    }
                    _ => Err(ErrorKind::ParseInt(e).at(span)),
                },
            }
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
                        c => return Err(ErrorKind::InvalidEscapeSequence(c).at(self.span)),
                    },
                    None => return Err(ErrorKind::UnexpectedEof.at(self.span)),
                },
                c => s.push(c),
            }
        }

        Ok(TokenKind::String(s).at(span))
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, TokenError> {
        let mut tokens = vec![];

        while let Some(cur) = self.cur() {
            let span = self.span;
            let next = self.peek();
            let last_is_ident = tokens.last().is_some_and(|t: &Token| t.kind.is_ident());

            if is_comment(cur, next) {
                self.comment();
            } else if is_atom(cur, next) && !last_is_ident {
                tokens.push(self.atom());
            } else if is_ident(cur) {
                tokens.push(self.ident_or_keyword());
            } else if is_number(cur, next) {
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
                    ('=', Some('>')) => TokenKind::Punct("=>").at(span),
                    ('<', Some('<')) => TokenKind::Punct("<<").at(span),
                    ('>', Some('>')) => TokenKind::Punct(">>").at(span),
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
                            '@' => TokenKind::Punct("@").at(span),
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
