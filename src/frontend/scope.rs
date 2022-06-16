use std::ops::Index;

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
    pub span: Span,
    pub ty: Option<Type>,
    pub name: String,
    pub mutable: bool,
    pub usages: Vec<ScopeId>,
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

    #[inline]
    pub fn cursor(&self) -> Cursor<'_, Scope> {
        Cursor::new(CursorData::Borrowed(self.scopes.as_slice()))
    }

    pub fn local_mut(&mut self, name: &str) -> Option<(&mut Local, usize)> {
        find_local(&self.scopes, self.index, name)
            .map(|(scope, idx)| (&mut self.scopes[scope].locals[idx], idx))
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

pub enum CursorData<'s, T> {
    Owned(Vec<T>),
    Borrowed(&'s [T]),
}

impl<'s, T> Index<usize> for CursorData<'s, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            CursorData::Owned(data) => data.index(index),
            CursorData::Borrowed(data) => data.index(index),
        }
    }
}

impl<'s, T> CursorData<'s, T> {
    pub fn len(&self) -> usize {
        match self {
            CursorData::Owned(data) => data.len(),
            CursorData::Borrowed(data) => data.len(),
        }
    }
}

pub struct Cursor<'c, T> {
    pub index: usize,
    data: CursorData<'c, T>,
}

impl<T> Default for Cursor<'_, T> {
    fn default() -> Self {
        Self {
            index: 0,
            data: CursorData::Owned(vec![]),
        }
    }
}

impl<'c, T> Cursor<'c, T> {
    pub fn new(slice: CursorData<'c, T>) -> Self {
        Self {
            index: 0,
            data: slice,
        }
    }

    pub fn move_to(&mut self, index: usize) {
        self.index = index;
    }

    #[inline]
    pub fn head(&self) -> &T {
        &self.data[self.index]
    }

    pub fn as_slice(&self) -> &[T] {
        match &self.data {
            CursorData::Owned(data) => data.as_slice(),
            CursorData::Borrowed(data) => data,
        }
    }
}

impl Cursor<'_, Scope> {
    // Moves the cursor to the next scope within the same parent. It will stop when it finds a scope
    // with a different parent which should work as all of the function scopes should be grouped
    // together.
    pub fn next_sibling(&mut self, parent: ScopeId) -> bool {
        if self.index >= self.data.len() - 1 {
            return false;
        }

        self.index += 1;

        let mut idx = self.index;

        loop {
            if idx == parent {
                return true;
            }

            if let Some(parent) = self.data[idx].parent {
                idx = parent;
                continue;
            }

            return false;
        }
    }
}
