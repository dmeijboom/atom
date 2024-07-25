use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

use crate::error::{IntoSpanned, SpannedError};

#[derive(Debug, Default, Clone, Copy)]
pub struct Span {
    pub offset: usize,
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
        "if" | "else" | "elif" | "for" | "let" | "fn" | "return" | "new" | "class"
    )
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("failed to parse float: {0}")]
    ParseFloat(#[from] ParseFloatError),
    #[error("failed to parse int: {0}")]
    ParseInt(#[from] ParseIntError),
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error("invalid input at: {0}")]
    InvalidInput(char),
}

pub type TokenError = SpannedError<ErrorKind>;

pub struct Lexer<'a> {
    n: usize,
    source: &'a [char],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [char]) -> Self {
        Self { n: 0, source }
    }

    fn cur(&self) -> Option<char> {
        self.source.get(self.n).copied()
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.n + 1).copied()
    }

    fn accept(&mut self, expected: char) -> bool {
        if self.cur() == Some(expected) {
            self.advance();
            return true;
        }

        false
    }

    fn advance(&mut self) {
        self.n += 1;
    }

    fn move_back(&mut self) {
        self.n -= 1;
    }

    fn next(&mut self) -> Option<char> {
        self.advance();
        self.source.get(self.n - 1).copied()
    }

    fn span(&self) -> Span {
        Span { offset: self.n }
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
                '.' if !dot && self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) => {
                    dot = true;
                    num.push('.');
                    self.advance();
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
                    Some(c) => {
                        s.push('\\');
                        s.push(c);
                    }
                    None => return Err(ErrorKind::UnexpectedEof.at(self.span())),
                },
                c => s.push(c),
            }
        }

        Ok(TokenKind::StringLit(s).at(span))
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, TokenError> {
        let mut tokens = vec![];

        while let Some(c) = self.next() {
            let span = self.span();
            let token = match (c, self.cur()) {
                (c, _) if c.is_whitespace() => continue,
                (c, Some(n))
                    if c.is_ascii_digit() || (matches!(c, '.' | '-') && n.is_ascii_digit()) =>
                {
                    self.move_back();
                    self.number()?
                }
                ('&', Some('&')) => {
                    self.advance();
                    TokenKind::Punct("&&").at(span)
                }
                ('|', Some('|')) => {
                    self.advance();
                    TokenKind::Punct("||").at(span)
                }
                ('!', Some('=')) => {
                    self.advance();
                    TokenKind::Punct("!=").at(span)
                }
                ('=', Some('=')) => {
                    self.advance();
                    TokenKind::Punct("==").at(span)
                }
                ('<', Some('=')) => {
                    self.advance();
                    TokenKind::Punct("<=").at(span)
                }
                ('>', Some('=')) => {
                    self.advance();
                    TokenKind::Punct(">=").at(span)
                }
                ('+', Some('=')) => {
                    self.advance();
                    TokenKind::Punct("+=").at(span)
                }
                ('-', Some('=')) => {
                    self.advance();
                    TokenKind::Punct("-=").at(span)
                }
                ('*', Some('=')) => {
                    self.advance();
                    TokenKind::Punct("*=").at(span)
                }
                ('/', Some('=')) => {
                    self.advance();
                    TokenKind::Punct("/=").at(span)
                }
                ('%', Some('=')) => {
                    self.advance();
                    TokenKind::Punct("%=").at(span)
                }
                ('+', _) => TokenKind::Punct("+").at(span),
                ('-', _) => TokenKind::Punct("-").at(span),
                ('*', _) => TokenKind::Punct("*").at(span),
                ('/', _) => TokenKind::Punct("/").at(span),
                ('%', _) => TokenKind::Punct("%").at(span),
                ('{', _) => TokenKind::Punct("{").at(span),
                ('}', _) => TokenKind::Punct("}").at(span),
                ('[', _) => TokenKind::Punct("[").at(span),
                (']', _) => TokenKind::Punct("]").at(span),
                ('(', _) => TokenKind::Punct("(").at(span),
                (')', _) => TokenKind::Punct(")").at(span),
                (':', _) => TokenKind::Punct(":").at(span),
                (';', _) => TokenKind::Punct(";").at(span),
                ('.', _) => TokenKind::Punct(".").at(span),
                (',', _) => TokenKind::Punct(",").at(span),
                ('&', _) => TokenKind::Punct("&").at(span),
                ('|', _) => TokenKind::Punct("|").at(span),
                ('^', _) => TokenKind::Punct("^").at(span),
                ('!', _) => TokenKind::Punct("!").at(span),
                ('=', _) => TokenKind::Punct("=").at(span),
                ('<', _) => TokenKind::Punct("<").at(span),
                ('>', _) => TokenKind::Punct(">").at(span),
                ('"', _) => self.string(span)?,
                ('_' | 'a'..='z' | 'A'..='Z', _) => {
                    self.move_back();
                    self.term()
                }
                (c, _) => return Err(ErrorKind::InvalidInput(c).at(span)),
            };

            tokens.push(token);
        }

        Ok(tokens)
    }
}
