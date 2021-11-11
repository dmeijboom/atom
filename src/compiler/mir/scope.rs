use std::collections::HashMap;

use crate::compiler::ir::Code;

use crate::compiler::CompileError;

pub type ScopeId = usize;
pub type LocalId = usize;

#[derive(Debug, Clone)]
pub struct Local {
    pub id: LocalId,
    pub name: String,
    pub mutable: bool,
}

impl Local {
    pub fn new(id: LocalId, name: String, mutable: bool) -> Self {
        Self { id, name, mutable }
    }

    pub fn store_instr(&self) -> Code {
        if self.mutable {
            Code::StoreMut(self.id)
        } else {
            Code::Store(self.id)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForLoopMeta {
    pub continue_label: String,
}

impl ForLoopMeta {
    pub fn new(continue_label: String) -> Self {
        Self { continue_label }
    }
}

#[derive(Debug)]
pub enum ScopeContext {
    Global,
    Block,
    IfElse,
    Class(String),
    ForLoop(ForLoopMeta),
    Function(String),
}

impl PartialEq for ScopeContext {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ScopeContext::Block => matches!(other, ScopeContext::Block),
            ScopeContext::Global => matches!(other, ScopeContext::Global),
            ScopeContext::IfElse => matches!(other, ScopeContext::IfElse),
            ScopeContext::Class(name) => {
                matches!(other, ScopeContext::Class(other) if name == other)
            }
            ScopeContext::ForLoop(_) => matches!(other, ScopeContext::ForLoop(_)),
            ScopeContext::Function(name) => {
                matches!(other, ScopeContext::Function(other) if name == other)
            }
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub id: ScopeId,
    pub local_id: LocalId,
    pub context: ScopeContext,
    pub parent: Option<ScopeId>,
    pub locals: HashMap<String, Local>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            id: 0,
            local_id: 0,
            parent: None,
            locals: HashMap::new(),
            context: ScopeContext::Global,
        }
    }

    pub fn new_child(parent: &Scope, context: ScopeContext) -> Self {
        Self {
            id: 0,
            context,
            local_id: 0,
            locals: HashMap::new(),
            parent: Some(parent.id),
        }
    }
}

#[derive(Debug)]
pub struct ScopeGraph {
    current: ScopeId,
    graph: Vec<Scope>,
}

impl ScopeGraph {
    pub fn new() -> Self {
        Self {
            current: 0,
            graph: vec![Scope::new()],
        }
    }

    fn walk<'s, T>(&'s self, handler: impl Fn(&'s Scope) -> Option<T>) -> Option<T> {
        for scope in self.graph.iter().rev() {
            if let Some(value) = handler(scope) {
                return Some(value);
            }
        }

        None
    }

    fn walk_mut<'s, T>(&'s mut self, handler: impl Fn(&'s mut Scope) -> Option<T>) -> Option<T> {
        for scope in self.graph.iter_mut().rev() {
            if let Some(value) = handler(scope) {
                return Some(value);
            }
        }

        None
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.graph.last_mut().unwrap()
    }

    pub fn current(&mut self) -> &Scope {
        self.graph.last().unwrap()
    }

    pub fn add(&mut self, mut scope: Scope) -> ScopeId {
        self.current += 1;

        scope.id = self.current;

        self.graph.push(scope);

        self.current
    }

    pub fn pop(&mut self) -> Option<Scope> {
        self.graph.pop()
    }

    pub fn set_local(&mut self, name: String, mutable: bool) -> Result<Local, CompileError> {
        let id = self
            .walk_mut(|scope| {
                if let ScopeContext::Function(_) = &scope.context {
                    let id = scope.local_id;

                    scope.local_id += 1;

                    return Some(id);
                }

                None
            })
            .ok_or_else(|| {
                CompileError::new("unable to set local outside of a function scope".to_string())
            })?;

        let scope = self.current_mut();
        let local = Local::new(id, name, mutable);

        scope.locals.insert(local.name.clone(), local.clone());

        Ok(local)
    }

    pub fn get_local(&self, name: &str, parents: bool) -> Option<&Local> {
        if !parents {
            return self.graph.last().and_then(|scope| scope.locals.get(name));
        }

        self.walk(|scope| {
            if let Some(local) = scope.locals.get(name) {
                return Some(local);
            }

            None
        })
    }
}
