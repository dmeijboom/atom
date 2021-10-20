use std::collections::HashMap;

use crate::compiler::{CompileError, Type};

#[derive(Clone)]
pub struct Local {
    pub id: usize,
    pub name: String,
    pub mutable: bool,
    pub known_type: Type,
}

impl Local {
    pub fn new(id: usize, name: String, mutable: bool, known_type: Type) -> Self {
        Self {
            id,
            name,
            mutable,
            known_type,
        }
    }
}

#[derive(Clone)]
pub struct ForLoopMeta {
    pub continue_label: String,
}

pub enum ScopeContext {
    Global,
    IfElse,
    Unsafe,
    Class(String),
    ForLoop(ForLoopMeta),
    Function((String, bool)),
}

pub struct Scope {
    pub id: usize,
    pub local_id: usize,
    pub context: ScopeContext,
    pub locals: HashMap<String, Local>,
}

pub struct ScopeGraph {
    current: usize,
    graph: Vec<Scope>,
}

impl ScopeGraph {
    pub fn new() -> Self {
        Self {
            current: 0,
            graph: vec![Scope::new()],
        }
    }

    fn walk<T>(&self, handler: impl Fn(&Scope) -> Option<T>) -> Option<T> {
        for scope in self.graph.iter().rev() {
            if let Some(value) = handler(scope) {
                return Some(value);
            }
        }

        None
    }

    fn walk_mut<T>(&mut self, handler: impl Fn(&mut Scope) -> Option<T>) -> Option<T> {
        for scope in self.graph.iter_mut().rev() {
            if let Some(value) = handler(scope) {
                return Some(value);
            }
        }

        None
    }

    pub fn len(&self) -> usize {
        self.graph.len()
    }

    pub fn get_by_id(&self, id: usize) -> Option<&Scope> {
        self.graph.get(id)
    }

    pub fn current_mut(&mut self) -> &mut Scope {
        self.graph.last_mut().unwrap()
    }

    pub fn push(&mut self, mut scope: Scope) {
        self.current += 1;

        scope.id = self.current;

        self.graph.push(scope);
    }

    pub fn pop(&mut self) {
        self.graph.pop();
    }

    pub fn in_unsafe_block(&self) -> bool {
        self.walk(|scope| {
            if let ScopeContext::Unsafe = &scope.context {
                return Some(true);
            }

            None
        })
        .unwrap_or(false)
    }

    pub fn in_function_block(&self) -> bool {
        self.walk(|scope| {
            if let ScopeContext::Function(_) = &scope.context {
                return Some(true);
            }

            None
        })
        .unwrap_or(false)
    }

    pub fn get_for_loop(&self) -> Option<ForLoopMeta> {
        self.walk(|scope| {
            if let ScopeContext::ForLoop(meta) = &scope.context {
                return Some(meta.clone());
            }

            None
        })
    }

    pub fn get_function_target(&self) -> Option<(String, bool)> {
        self.walk(|scope| {
            if let ScopeContext::Function((target, is_method)) = &scope.context {
                return Some((target.clone(), *is_method));
            }

            None
        })
    }

    pub fn set_local(
        &mut self,
        name: String,
        mutable: bool,
        known_type: Type,
    ) -> Result<Local, CompileError> {
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
        let local = Local::new(id, name, mutable, known_type);

        scope.locals.insert(local.name.clone(), local.clone());

        Ok(local)
    }

    pub fn get_local(&self, name: &str, parents: bool) -> Option<Local> {
        if !parents {
            return self
                .graph
                .last()
                .and_then(|scope| scope.locals.get(name).cloned());
        }

        self.walk(|scope| {
            if let Some(local) = scope.locals.get(name) {
                return Some(local.clone());
            }

            None
        })
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            id: 0,
            local_id: 0,
            locals: HashMap::new(),
            context: ScopeContext::Global,
        }
    }

    pub fn new_child(context: ScopeContext) -> Self {
        Self {
            id: 0,
            context,
            local_id: 0,
            locals: HashMap::new(),
        }
    }
}
