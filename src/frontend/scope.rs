use crate::frontend::syntax::Span;
use crate::frontend::{Error, Type};

pub type ScopeId = usize;

pub enum ScopeKind {
    Global,
    Fn,
}

pub struct Local {
    pub name: String,
    pub ty: Type,
}

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
    scopes: Vec<Scope>,
}

impl ScopeList {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                id: 0,
                kind: ScopeKind::Global,
                parent: None,
                locals: vec![],
            }],
        }
    }

    #[inline]
    pub fn head(&self) -> &Scope {
        let idx = self.scopes.len() - 1;
        &self.scopes[idx]
    }

    #[inline]
    pub fn head_mut(&mut self) -> &mut Scope {
        let idx = self.scopes.len() - 1;
        &mut self.scopes[idx]
    }

    pub fn enter(&mut self, kind: ScopeKind) -> ScopeId {
        let id = self.scopes.len();

        self.scopes.push(Scope {
            id,
            kind,
            parent: Some(self.head().id),
            locals: vec![],
        });

        id
    }

    pub fn exit(&mut self) -> Scope {
        assert_ne!(self.scopes.len(), 0);
        self.scopes.pop().unwrap()
    }
}
