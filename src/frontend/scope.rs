use crate::frontend::syntax::Span;
use crate::frontend::tree::{Cursor, Tree};
use crate::frontend::{Error, Type};

pub type ScopeId = usize;

#[derive(Debug)]
pub struct FnScope {
    pub args: Vec<Name>,
}

impl FnScope {
    pub fn new(args: Vec<Name>) -> Self {
        Self { args }
    }
}

#[derive(Debug)]
pub enum ScopeKind {
    Fn(FnScope),
    Local,
    Global,
}

#[derive(Debug)]
pub struct Name {
    pub span: Span,
    pub name: String,
    pub mutable: bool,
    pub ty: Option<Type>,
    pub usages: Vec<ScopeId>,
    pub assigned: Vec<ScopeId>,
}

impl Name {
    pub fn new(span: Span, name: String, mutable: bool) -> Self {
        Self {
            span,
            name,
            mutable,
            ty: None,
            usages: vec![],
            assigned: vec![],
        }
    }

    pub fn with_type(mut self, ty: Type) -> Self {
        self.ty = Some(ty);
        self
    }

    pub fn with_value(mut self, scope_id: ScopeId) -> Self {
        self.assigned = vec![scope_id];
        self
    }
}

#[derive(Debug)]
pub struct Scope {
    pub kind: ScopeKind,
    pub locals: Vec<Name>,
}

impl Scope {
    pub fn declare(&mut self, local_def: Name) -> Result<usize, Error> {
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

pub struct ScopeContainer {
    pub index: usize,
    tree: Tree<Scope>,
}

impl ScopeContainer {
    pub fn new() -> Self {
        let mut tree = Tree::new();
        let index = tree.add(
            Scope {
                kind: ScopeKind::Global,
                locals: vec![],
            },
            None,
        );

        Self { index, tree }
    }

    pub fn consume(self) -> Tree<Scope> {
        self.tree
    }

    pub fn cursor(&self) -> Cursor<Scope> {
        let mut cursor = self.tree.cursor();
        cursor.move_to(self.index);

        cursor
    }

    pub fn is_initialised(&self, local: &Name) -> bool {
        let mut cursor = self.cursor();

        loop {
            if let Some(scope) = cursor.node().map(|n| n.index) {
                if local.assigned.contains(&scope) {
                    return true;
                }
            }

            if !cursor.move_to_parent() {
                return false;
            }
        }
    }

    pub fn find_local(&self, name: &str) -> Option<&Name> {
        find_local(self.cursor(), name).map(|(scope, idx)| {
            self.tree
                .get(scope)
                .and_then(|n| n.value.locals.get(idx))
                .unwrap()
        })
    }

    pub fn find_local_mut(&mut self, name: &str) -> Option<&mut Name> {
        find_local(self.cursor(), name).map(|(scope, idx)| {
            self.tree
                .get_mut(scope)
                .and_then(|n| n.value.locals.get_mut(idx))
                .unwrap()
        })
    }

    pub fn head_mut(&mut self) -> &mut Scope {
        // This should be safe as `self.index` is always valid.
        self.tree.get_mut(self.index).map(|n| &mut n.value).unwrap()
    }

    pub fn enter(&mut self, kind: ScopeKind) -> ScopeId {
        self.index= self.tree.add(
            Scope {
                kind,
                locals: vec![],
            },
            Some(self.index),
        );
        self.index
    }

    pub fn leave(&mut self) -> ScopeId {
        assert_ne!(self.index, 0);

        let mut cursor = self.tree.cursor();

        cursor.move_to(self.index);
        cursor.move_to_parent();

        // This should be safe as long as `self.index` > 0 (which is what the assertion covers)
        self.index = cursor.node().map(|n| n.index).unwrap();
        self.index
    }
}

pub fn find_local(mut cursor: Cursor<'_, Scope>, name: &str) -> Option<(ScopeId, usize)> {
    loop {
        if let Some(node) = cursor.node() {
            if let Some(idx) = node
                .value
                .locals
                .iter()
                .position(|local| local.name == name)
            {
                return Some((node.index, idx));
            }
        }

        if !cursor.move_to_parent() {
            return None;
        }
    }
}
