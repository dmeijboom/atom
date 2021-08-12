use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    ForLoop(ForLoopMeta),
    Function(Option<String>),
}

pub struct Scope {
    pub id: usize,
    local_id: usize,
    pub context: ScopeContext,
    pub locals: HashMap<String, Local>,
    pub parent: Option<Rc<RefCell<Scope>>>,
}

fn walk<T>(scope: &Rc<RefCell<Scope>>, handler: impl Fn(&mut Scope) -> Option<T>) -> Option<T> {
    {
        let mut scope = scope.borrow_mut();

        if let Some(value) = handler(&mut scope) {
            return Some(value);
        }
    }

    if let Some(parent) = &scope.borrow_mut().parent {
        return walk(&Rc::clone(parent), handler);
    }

    None
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

    pub fn new_with_parent(context: ScopeContext, scope: Rc<RefCell<Scope>>, id: usize) -> Self {
        Self {
            id,
            context,
            local_id: 0,
            locals: HashMap::new(),
            parent: Some(Rc::clone(&scope)),
        }
    }

    pub fn set_local(&mut self, name: String, mutable: bool) -> Result<Local, CompileError> {
        let id = if let ScopeContext::Function(_) = self.context {
            let id = self.local_id;

            self.local_id += 1;

            Some(id)
        } else if let Some(parent) = &self.parent {
            walk(parent, |scope| {
                if let ScopeContext::Function(_) = &scope.context {
                    let id = scope.local_id;

                    scope.local_id += 1;

                    return Some(id);
                }

                None
            })
        } else {
            None
        }
        .ok_or_else(|| {
            CompileError::new(
                "unable to set local outside of a function scope".to_string(),
                0..0,
            )
        })?;

        let local = Local { id, name, mutable };

        self.locals.insert(local.name.clone(), local.clone());

        Ok(local)
    }

    pub fn in_unsafe_block(scope: &Rc<RefCell<Scope>>) -> bool {
        walk(scope, |scope| {
            if let ScopeContext::Unsafe = &scope.context {
                return Some(true);
            }

            None
        })
        .unwrap_or(false)
    }

    pub fn get_for_loop(scope: &Rc<RefCell<Scope>>) -> Option<ForLoopMeta> {
        walk(scope, |scope| {
            if let ScopeContext::ForLoop(meta) = &scope.context {
                return Some(meta.clone());
            }

            None
        })
    }

    pub fn get_local(scope: &Rc<RefCell<Scope>>, name: &str, parents: bool) -> Option<Local> {
        {
            let scope = scope.borrow();

            if let Some(local) = scope.locals.get(name) {
                return Some(local.clone());
            }
        }

        if parents {
            if let Some(scope) = &scope.borrow().parent {
                return Scope::get_local(&Rc::clone(scope), name, parents);
            }
        }

        return None;
    }

    pub fn get_target(scope: &Rc<RefCell<Scope>>) -> Option<String> {
        walk(scope, |scope| {
            if let ScopeContext::Function(target) = &scope.context {
                return target.clone();
            }

            None
        })
    }
}
