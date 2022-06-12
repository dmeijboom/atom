use crate::frontend::syntax::Span;
use crate::frontend::{Error, Type};

pub type ScopeId = usize;

#[derive(Debug)]
pub enum ScopeKind {
    Fn,
    Local,
    Global,
}

#[derive(Debug)]
pub struct Local {
    pub ty: Option<Type>,
    pub name: String,
    pub mutable: bool,
    pub initialised_at: Vec<ScopeId>,
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

    pub fn declare(&mut self, local_def: Local) -> Result<usize, Error> {
        if self.locals.iter().any(|local| local.name == local_def.name) {
            return Err(Error::new(
                Span::default(),
                format!("unable to redefine '{}'", local_def.name),
            ));
        }

        let idx = self.locals.len();

        self.locals.push(local_def);

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

    fn is_initialised(&self, local: &Local) -> bool {
        'find_init: for scope_id in local.initialised_at.iter() {
            let mut current = self.index;

            loop {
                if current == *scope_id {
                    return true;
                }

                if let Some(idx) = self.scopes[current].parent {
                    current = idx;
                    continue;
                }

                continue 'find_init;
            }
        }

        false
    }

    pub fn local(&self, name: &str) -> Option<(&Local, usize, bool)> {
        find_local(&self.scopes, self.index, name).map(|(scope, idx)| {
            let local = &self.scopes[scope].locals[idx];

            (local, idx, self.is_initialised(local))
        })
    }

    pub fn local_mut(&mut self, name: &str) -> Option<(&mut Local, usize, bool)> {
        find_local(&self.scopes, self.index, name).map(|(scope, idx)| {
            let local = &self.scopes[scope].locals[idx];
            let initialised = self.is_initialised(local);

            (&mut self.scopes[scope].locals[idx], idx, initialised)
        })
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
        let id = self.scopes.len();

        self.scopes.push(Scope {
            id,
            kind,
            parent: Some(self.head().id),
            locals: vec![],
        });

        self.index = id;

        id
    }

    pub fn leave(&mut self) -> ScopeId {
        assert_ne!(self.index, 0);

        let head = self.head();
        let id = head.id;

        self.index = head.parent.unwrap();

        id
    }
}

pub fn find_local(scopes: &[Scope], index: usize, name: &str) -> Option<(ScopeId, usize)> {
    let mut idx = index;

    loop {
        let scope = &scopes[idx];

        if let Some(idx) = scope.locals.iter().position(|local| local.name == name) {
            return Some((scope.id, idx));
        }

        if let Some(parent) = scope.parent {
            idx = parent;
            continue;
        }

        return None;
    }
}
