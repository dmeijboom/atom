use std::{
    fmt::Display,
    num::{ParseFloatError, ParseIntError},
};

#[derive(Debug, Default, Clone)]
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
    BracketLeft,
    BracketRight,
    SqrBracketLeft,
    SqrBracketRight,
    ParentLeft,
    ParentRight,
    Dot,
    Comma,
    Colon,
    Semi,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Not,
    Lt,
    Gt,
    And,
    Or,
    Pow,
    BoolLit(bool),
    IntLit(i64),
    FloatLit(f64),
    StringLit(String),
    Eof,
}

impl TokenKind {
    pub fn at(self, span: Span) -> Token {
        Token { kind: self, span }
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ident(name) => write!(f, "{}", name),
            Self::Keyword(name) => write!(f, "{}", name),
            Self::BracketLeft => write!(f, "{{"),
            Self::BracketRight => write!(f, "}}"),
            Self::SqrBracketLeft => write!(f, "["),
            Self::SqrBracketRight => write!(f, "]"),
            Self::ParentLeft => write!(f, "("),
            Self::ParentRight => write!(f, ")"),
            Self::Dot => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::Colon => write!(f, ":"),
            Self::Semi => write!(f, ";"),
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Eq => write!(f, "="),
            Self::Not => write!(f, "!"),
            Self::Lt => write!(f, "<"),
            Self::Gt => write!(f, ">"),
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Pow => write!(f, "^"),
            Self::BoolLit(true) => write!(f, "true"),
            Self::BoolLit(false) => write!(f, "false"),
            Self::IntLit(value) => write!(f, "{}", value),
            Self::FloatLit(value) => write!(f, "{}", value),
            Self::StringLit(value) => write!(f, "\"{value}\""),
            Self::Eof => write!(f, "EOF"),
        }
    }
}

fn is_term(c: &char) -> bool {
    matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
}

fn is_keyword(s: &str) -> bool {
    matches!(s, "if" | "else" | "elif" | "for" | "let" | "fn" | "return")
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to parse float: {0}")]
    ParseFloat(#[from] ParseFloatError),
    #[error("failed to parse int: {0}")]
    ParseInt(#[from] ParseIntError),
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error("invalid input at: {0}")]
    InvalidInput(char),
}

pub struct Lexer<'a> {
    n: usize,
    source: &'a [char],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a [char]) -> Self {
        Self { n: 0, source }
    }

    fn hdr(&self) -> Option<&char> {
        self.source.get(self.n)
    }

    fn advance(&mut self) {
        self.n += 1;
    }

    fn move_back(&mut self) {
        self.n -= 1;
    }

    fn next(&mut self) -> Option<&char> {
        self.advance();
        self.source.get(self.n - 1)
    }

    fn next_and_hdr(&mut self) -> (Option<&char>, Option<&char>) {
        self.advance();
        (self.source.get(self.n - 1), self.source.get(self.n))
    }

    fn span(&self) -> Span {
        Span { offset: self.n }
    }

    fn term(&mut self) -> Token {
        let span = self.span();
        let mut term = String::new();

        while let Some(c) = self.hdr().cloned() {
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

    fn number(&mut self) -> Result<Token, Error> {
        let mut dot = false;
        let span = self.span();
        let mut num = String::new();

        loop {
            match self.next_and_hdr() {
                (Some(c), _) if c.is_ascii_digit() => num.push(*c),
                (Some('_'), _) => continue,
                (Some('.'), Some(c)) if !dot && c.is_ascii_digit() => {
                    dot = true;
                    num.push('.');
                    num.push(*c);
                    self.advance();
                }
                (Some(_), _) => {
                    self.move_back();
                    break;
                }
                (None, _) => break,
            }
        }

        Ok(if num.contains('.') {
            TokenKind::FloatLit(num.parse().unwrap())
        } else {
            TokenKind::IntLit(num.parse().unwrap())
        }
        .at(span))
    }

    fn string(&mut self, span: Span) -> Result<Token, Error> {
        let mut s = String::new();

        while let Some(c) = self.next() {
            match c {
                '"' => break,
                '\\' => match self.next() {
                    Some(c) => {
                        s.push('\\');
                        s.push(*c);
                    }
                    None => return Err(Error::UnexpectedEof),
                },
                c => s.push(*c),
            }
        }

        Ok(TokenKind::StringLit(s).at(span))
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens = vec![];

        loop {
            let span = self.span();
            let token = match self.next_and_hdr() {
                (Some(c), _) if c.is_whitespace() => continue,
                (Some(c), Some(n)) if c.is_ascii_digit() || (c == &'.' && n.is_ascii_digit()) => {
                    self.move_back();
                    self.number()?
                }
                (Some('{'), _) => TokenKind::BracketLeft.at(span),
                (Some('}'), _) => TokenKind::BracketRight.at(span),
                (Some('['), _) => TokenKind::SqrBracketLeft.at(span),
                (Some(']'), _) => TokenKind::SqrBracketRight.at(span),
                (Some('('), _) => TokenKind::ParentLeft.at(span),
                (Some(')'), _) => TokenKind::ParentRight.at(span),
                (Some(':'), _) => TokenKind::Colon.at(span),
                (Some(';'), _) => TokenKind::Semi.at(span),
                (Some('.'), _) => TokenKind::Dot.at(span),
                (Some(','), _) => TokenKind::Comma.at(span),
                (Some('+'), _) => TokenKind::Add.at(span),
                (Some('-'), _) => TokenKind::Sub.at(span),
                (Some('*'), _) => TokenKind::Mul.at(span),
                (Some('/'), _) => TokenKind::Div.at(span),
                (Some('&'), _) => TokenKind::And.at(span),
                (Some('|'), _) => TokenKind::Or.at(span),
                (Some('^'), _) => TokenKind::Pow.at(span),
                (Some('!'), _) => TokenKind::Not.at(span),
                (Some('='), _) => TokenKind::Eq.at(span),
                (Some('<'), _) => TokenKind::Lt.at(span),
                (Some('>'), _) => TokenKind::Gt.at(span),
                (Some('"'), _) => self.string(span)?,
                (Some('_' | 'a'..='z' | 'A'..='Z'), _) => {
                    self.move_back();
                    self.term()
                }
                (Some(c), _) => return Err(Error::InvalidInput(*c)),
                (None, _) => {
                    tokens.push(TokenKind::Eof.at(span));
                    break;
                }
            };

            tokens.push(token);
        }

        Ok(tokens)
    }
}
