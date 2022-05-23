pub type ScopeId = usize;

pub enum ScopeKind {
    Global,
    Fn,
}

pub struct Scope {
    pub id: ScopeId,
    pub kind: ScopeKind,
    pub parent: Option<ScopeId>,
}

pub struct ScopeList {
    pub current: ScopeId,
    scopes: Vec<Scope>,
}

impl ScopeList {
    pub fn new() -> Self {
        Self {
            current: 0,
            scopes: vec![Scope {
                id: 0,
                kind: ScopeKind::Global,
                parent: None,
            }],
        }
    }

    pub fn scope(&self) -> &Scope {
        &self.scopes[self.current]
    }

    pub fn scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[self.current]
    }

    pub fn enter(&mut self, kind: ScopeKind) -> ScopeId {
        let id = self.scopes.len();

        self.scopes.push(Scope {
            id,
            kind,
            parent: Some(self.current),
        });

        self.current = id;

        id
    }

    pub fn exit(&mut self) -> Option<ScopeId> {
        assert_ne!(self.current, 0);

        self.scopes.get(self.current).and_then(|scope| scope.parent)
    }
}
