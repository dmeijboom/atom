use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Local {
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
    Function,
    Unsafe,
    ForLoop(ForLoopMeta),
}

pub struct Scope {
    pub id: usize,
    pub context: ScopeContext,
    pub locals: HashMap<String, Local>,
    pub parent: Option<Rc<RefCell<Scope>>>,
}

fn walk<T>(scope: &Rc<RefCell<Scope>>, handler: impl Fn(&Scope) -> Option<T>) -> Option<T> {
    {
        let scope = scope.borrow();

        if let Some(value) = handler(&scope) {
            return Some(value);
        }
    }

    if let Some(parent) = &scope.borrow().parent {
        return walk(&Rc::clone(parent), handler);
    }

    None
}

impl Scope {
    pub fn new() -> Self {
        Self {
            id: 0,
            parent: None,
            locals: HashMap::new(),
            context: ScopeContext::Global,
        }
    }

    pub fn new_with_parent(context: ScopeContext, scope: Rc<RefCell<Scope>>, id: usize) -> Self {
        Self {
            id,
            context,
            locals: HashMap::new(),
            parent: Some(Rc::clone(&scope)),
        }
    }

    pub fn set_local(&mut self, local: Local) {
        self.locals.insert(local.name.clone(), local);
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

    pub fn get_local(
        scope: &Rc<RefCell<Scope>>,
        name: &str,
        parents: bool,
    ) -> Option<(Local, usize)> {
        {
            let scope = scope.borrow();

            if let Some(local) = scope.locals.get(name) {
                return Some((local.clone(), scope.id));
            }
        }

        if parents {
            if let Some(scope) = &scope.borrow().parent {
                return Scope::get_local(&Rc::clone(scope), name, parents);
            }
        }

        return None;
    }
}
