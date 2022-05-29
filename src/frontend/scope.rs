use crate::frontend::syntax::Span;
use crate::frontend::{Error, Type};

pub type ScopeId = usize;

#[derive(Debug)]
pub enum ScopeKind {
    Global,
    Fn,
}

#[derive(Debug)]
pub struct Local {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
    locals: Vec<Local>,
}

impl Scope {
    #[inline]
    pub fn locals(&self) -> &[Local] {
        &self.locals
    }

    pub fn local(&self, name: &str) -> Option<(&Local, usize)> {
        if let Some(idx) = self.locals.iter().position(|local| local.name == name) {
            return Some((&self.locals[idx], idx));
        }

        None
    }

    pub fn declare(&mut self, name: String, ty: Type) -> Result<usize, Error> {
        if self.locals.iter().any(|local| local.name == name) {
            return Err(Error::new(
                Span::default(),
                format!("unable to redefine '{}'", name),
            ));
        }

        let idx = self.locals.len();

        self.locals.push(Local { name, ty });

        Ok(idx)
    }
}

pub struct ScopeList {
    index: usize,
    scopes: Vec<Scope>,
}

impl ScopeList {
    pub fn new() -> Self {
        Self {
            index: 0,
            scopes: vec![Scope {
                id: 0,
                kind: ScopeKind::Global,
                parent: None,
                locals: vec![],
            }],
        }
    }

    pub fn consume(self) -> Vec<Scope> {
        self.scopes
    }

    #[inline]
    pub fn head(&self) -> &Scope {
        &self.scopes[self.index]
    }

    #[inline]
    pub fn head_mut(&mut self) -> &mut Scope {
        &mut self.scopes[self.index]
    }

    pub fn enter(&mut self, kind: ScopeKind) -> ScopeId {
        let id = self.head().id + 1;

        self.scopes.push(Scope {
            id,
            kind,
            parent: Some(self.head().id),
            locals: vec![],
        });

        self.index += 1;

        id
    }

    pub fn exit(&mut self) -> ScopeId {
        assert_ne!(self.index, 0);

        let id = self.head().id;

        self.index -= 1;

        id
    }
}
