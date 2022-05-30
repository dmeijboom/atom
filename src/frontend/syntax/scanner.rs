use logos::{Lexer, Logos, Source};

use crate::frontend::syntax::lexer::{LexerExtras, StringToken, Token};
use crate::frontend::syntax::Span;

pub struct Scanner<'s, T: Logos<'s> = Token> {
    lexer: Lexer<'s, T>,
    stack: Vec<T>,
}

impl<'s> Scanner<'s> {
    pub fn into_string_scanner(self) -> Scanner<'s, StringToken> {
        debug_assert!(self.stack.is_empty());

        Scanner {
            lexer: self.lexer.morph(),
            stack: vec![],
        }
    }
}

impl<'s, T> Scanner<'s, T>
where
    T: Logos<'s, Source = str>,
    <T as Logos<'s>>::Extras: Default,
{
    pub fn new(source: &'s str) -> Self {
        Self {
            lexer: T::lexer(source),
            stack: vec![],
        }
    }
}

impl<'s, T: Logos<'s>> Scanner<'s, T> {
    pub fn peek(&mut self) -> Option<&T> {
        if !self.stack.is_empty() {
            return self.stack.last();
        }

        if let Some(token) = self.lexer.next() {
            self.stack.push(token);

            return self.stack.last();
        }

        None
    }

    #[inline]
    pub fn text(&self) -> &'s <T::Source as Source>::Slice {
        self.lexer.slice()
    }

    pub fn push_back(&mut self, token: T) {
        self.stack.push(token);
    }
}

impl<'s, T: Logos<'s, Source = str>> Scanner<'s, T> {
    pub fn into_scanner<T2>(self) -> Scanner<'s, T2>
    where
        T2: Logos<'s, Source = str>,
        <T as Logos<'s>>::Extras: Into<T2::Extras>,
    {
        debug_assert!(self.stack.is_empty());

        Scanner {
            lexer: self.lexer.morph(),
            stack: vec![],
        }
    }
}

impl<'s, T: Logos<'s, Extras = LexerExtras>> Scanner<'s, T> {
    #[inline]
    pub fn span(&self) -> Span {
        let span = self.lexer.span();

        Span {
            begin: span.start,
            end: span.end,
            line: self.lexer.extras.line,
            column: self.lexer.extras.column,
        }
    }
}

impl<'s, T: Logos<'s, Source = str, Extras = LexerExtras>> Scanner<'s, T> {
    pub fn advance(&mut self) -> Option<T> {
        let token = self.stack.pop().or_else(|| self.lexer.next());

        self.lexer.extras.apply(self.text());

        token
    }

    pub fn error_description(&mut self) -> String {
        if let Some(err) = self.lexer.extras.error.take() {
            return format!("{}", err);
        }

        format!("unexpected error in token: {}", self.lexer.slice())
    }
}
