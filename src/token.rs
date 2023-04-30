use logos::Logos;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(extras = LexerMeta)]
pub enum Token {
    #[token("fn")]
    Fn,

    #[token("(")]
    LParent,

    #[token(")")]
    RParent,

    #[token("{")]
    LBracket,

    #[token("}")]
    RBracket,

    #[regex(r"[0-9]([0-9]|_[0-9][0-9][0-9])*", |lex| lex.slice().parse())]
    #[regex(r"\.[0-9]([0-9]|_[0-9][0-9][0-9])*", |lex| lex.slice().parse())]
    #[regex(r"[0-9]([0-9]|_[0-9][0-9][0-9])*\.([0-9]|_[0-9][0-9][0-9])*", |lex| lex.slice().parse())]
    Number(f64),

    #[regex(r"(_|[a-z])[a-zA-Z_0-9]*")]
    Ident,

    #[error]
    #[regex(r"[ \t\n\f]+", |lex| {
        let s = lex.slice();

        lex.extras.line += s.matches('\n').count();

        if let Some(idx) = s.rfind('\n') {
            lex.extras.line_idx = Some(lex.span().start - idx);
        }

        logos::skip(lex)
    })]
    Error,
}

pub struct LexerMeta {
    pub line: usize,
    pub line_idx: Option<usize>,
}

impl Default for LexerMeta {
    fn default() -> Self {
        Self {
            line: 1,
            line_idx: None,
        }
    }
}
