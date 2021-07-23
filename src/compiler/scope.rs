use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub struct Local {
    pub name: String,
    pub mutable: bool,
    pub is_function: bool,
}

pub struct Scope {
    pub id: usize,
    pub locals: HashMap<String, Local>,
    pub parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            id: 0,
            parent: None,
            locals: HashMap::new(),
        }
    }

    pub fn new_with_parent(scope: Rc<RefCell<Scope>>, id: usize) -> Self {
        Self {
            id,
            locals: HashMap::new(),
            parent: Some(Rc::clone(&scope)),
        }
    }

    pub fn set_local(&mut self, local: Local) {
        self.locals.insert(local.name.clone(), local);
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
