#[derive(Debug, Default)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Location {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self {
            line,
            column,
            offset,
        }
    }
}

#[derive(Debug, Default)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

pub trait LocationExt {
    fn until(self, end: Location) -> Span;
}

impl LocationExt for Location {
    fn until(self, end: Location) -> Span {
        Span { start: self, end }
    }
}

#[derive(Debug)]
pub enum Node<'s> {
    Stmt(Stmt<'s>),
}

#[derive(Debug)]
pub struct Stmt<'s> {
    pub span: Span,
    pub kind: StmtKind<'s>,
}

impl<'s> Stmt<'s> {
    pub fn new(span: Span, kind: StmtKind<'s>) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug)]
pub enum StmtKind<'s> {
    FnDef { name: &'s str },
}
