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
    pub locals: HashMap<String, Local>,
    pub parent: Option<Rc<RefCell<Scope>>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            parent: None,
        }
    }

    pub fn set_local(&mut self, local: Local) {
        self.locals.insert(local.name.clone(), local);
    }

    pub fn push(scope: Rc<RefCell<Scope>>) -> Scope {
        let mut new_scope = Scope::new();

        new_scope.parent = Some(Rc::clone(&scope));

        new_scope
    }

    pub fn get_local(scope: &Rc<RefCell<Scope>>, name: &str) -> Option<Local> {
        {
            let scope = scope.borrow();

            if let Some(local) = scope.locals.get(name) {
                return Some(local.clone());
            }
        }

        if let Some(scope) = &scope.borrow().parent {
            return Scope::get_local(&Rc::clone(scope), name);
        }

        return None;
    }
}
