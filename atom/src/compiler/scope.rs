use std::collections::HashMap;

use atom_ir::Location;

use crate::compiler::CompileError;

#[derive(Clone)]
pub struct Local {
    pub id: usize,
    pub name: String,
    pub mutable: bool,
}

#[derive(Clone)]
pub struct ForLoopMeta {
    pub continue_label: String,
}

pub enum ScopeContext {
    Global,
    Class,
    IfElse,
    Unsafe,
    Closure(Option<String>),
    ForLoop(ForLoopMeta),
    Function((String, bool)),
}

pub struct Scope {
    local_id: usize,
    pub id: usize,
    pub context: ScopeContext,
    pub locals: HashMap<String, Local>,
}

fn walk<T>(scope: &[Scope], handler: impl Fn(&Scope) -> Option<T>) -> Option<T> {
    for scope in scope.iter().rev() {
        if let Some(value) = handler(scope) {
            return Some(value);
        }
    }

    None
}

fn walk_mut<T>(scope: &mut [Scope], handler: impl Fn(&mut Scope) -> Option<T>) -> Option<T> {
    for scope in scope.iter_mut().rev() {
        if let Some(value) = handler(scope) {
            return Some(value);
        }
    }

    None
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

    pub fn new_child(context: ScopeContext, id: usize) -> Self {
        Self {
            id,
            context,
            local_id: 0,
            locals: HashMap::new(),
        }
    }

    pub fn in_unsafe_block(scope: &[Scope]) -> bool {
        walk(scope, |scope| {
            if let ScopeContext::Unsafe = &scope.context {
                return Some(true);
            }

            None
        })
        .unwrap_or(false)
    }

    pub fn in_function_block(scope: &[Scope]) -> bool {
        walk(scope, |scope| {
            if let ScopeContext::Function(_) = &scope.context {
                return Some(true);
            }

            None
        })
        .unwrap_or(false)
    }

    pub fn get_for_loop(scope: &[Scope]) -> Option<ForLoopMeta> {
        walk(scope, |scope| {
            if let ScopeContext::ForLoop(meta) = &scope.context {
                return Some(meta.clone());
            }

            None
        })
    }

    pub fn set_local(
        scope: &mut [Scope],
        name: String,
        mutable: bool,
    ) -> Result<Local, CompileError> {
        let id = walk_mut(scope, |scope| {
            if let ScopeContext::Function(_) = &scope.context {
                let id = scope.local_id;

                scope.local_id += 1;

                return Some(id);
            }

            None
        })
        .ok_or_else(|| {
            CompileError::new(
                "unable to set local outside of a function scope".to_string(),
                Location::default(),
            )
        })?;

        let scope = scope.last_mut().unwrap();
        let local = Local { id, name, mutable };

        scope.locals.insert(local.name.clone(), local.clone());

        Ok(local)
    }

    pub fn get_local(scope: &[Scope], name: &str, parents: bool) -> Option<Local> {
        if !parents {
            return scope
                .last()
                .and_then(|scope| scope.locals.get(name).cloned());
        }

        walk(scope, |scope| {
            if let Some(local) = scope.locals.get(name) {
                return Some(local.clone());
            }

            None
        })
    }

    pub fn get_function_target(scope: &[Scope]) -> Option<(String, bool)> {
        walk(scope, |scope| {
            if let ScopeContext::Function((target, is_method)) = &scope.context {
                return Some((target.clone(), *is_method));
            }

            None
        })
    }
}
