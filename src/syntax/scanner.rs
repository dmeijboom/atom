use logos::{Lexer, Logos, Source};

use crate::syntax::lexer::{LexerExtras, StringToken, Token};
use crate::syntax::Span;

pub struct Scanner<'s, T: Logos<'s> = Token> {
    lexer: Lexer<'s, T>,
    peeked: Option<T>,
}

impl<'s> Scanner<'s> {
    pub fn into_string_scanner(self) -> Scanner<'s, StringToken> {
        debug_assert!(self.peeked.is_none());

        Scanner {
            lexer: self.lexer.morph(),
            peeked: None,
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
            peeked: None,
        }
    }
}

impl<'s, T: Logos<'s>> Scanner<'s, T> {
    #[inline]
    pub fn peek(&mut self) -> Option<&T> {
        if self.peeked.is_none() {
            self.peeked = self.lexer.next();
        }

        self.peeked.as_ref()
    }

    #[inline]
    pub fn text(&self) -> &'s <T::Source as Source>::Slice {
        self.lexer.slice()
    }
}

impl<'s, T: Logos<'s, Source = str>> Scanner<'s, T> {
    pub fn into_scanner<T2>(self) -> Scanner<'s, T2>
    where
        T2: Logos<'s, Source = str>,
        <T as Logos<'s>>::Extras: Into<T2::Extras>,
    {
        debug_assert!(self.peeked.is_none());

        Scanner {
            lexer: self.lexer.morph(),
            peeked: None,
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

    #[inline]
    pub fn error_description(&mut self) -> String {
        if let Some(err) = self.lexer.extras.error.take() {
            return format!("{}", err);
        }

        "unexpected error".to_string()
    }
}

impl<'s, T: Logos<'s, Source = str, Extras = LexerExtras>> Scanner<'s, T> {
    pub fn advance(&mut self) -> Option<T> {
        let token = self.peeked.take().or_else(|| self.lexer.next());

        self.lexer.extras.apply(self.text());

        token
    }
}
