use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use indexmap::map::IndexMap;

use atom_ir::IR;

use crate::origin::Origin;
use crate::{Result, Value};

pub type ExternalFn = fn(Option<Value>, Vec<Value>) -> Result<Option<Value>>;

#[derive(Debug, Clone)]
pub enum FnPtr {
    External(ExternalFn),
    Native(Rc<Vec<IR>>),
}

#[derive(Debug, Clone)]
pub struct FnArg {
    pub mutable: bool,
}

impl FnArg {
    pub fn new(mutable: bool) -> Self {
        Self { mutable }
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub ptr: FnPtr,
    pub public: bool,
    pub origin: Origin,
    pub args: IndexMap<String, FnArg>,
}

impl Fn {
    pub fn native(
        name: String,
        origin: Origin,
        public: bool,
        args: IndexMap<String, FnArg>,
        ir: Vec<IR>,
    ) -> Self {
        Self {
            name,
            origin,
            public,
            args,
            ptr: FnPtr::Native(Rc::new(ir)),
        }
    }

    pub fn external(name: String, origin: Origin, public: bool, func: ExternalFn) -> Self {
        Self {
            name,
            origin,
            public,
            args: IndexMap::new(),
            ptr: FnPtr::External(func),
        }
    }
}

impl Display for Fn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.origin.module_name, self.name)
    }
}

impl PartialEq for Fn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.public && other.public && self.origin == other.origin
    }
}

impl Hash for Fn {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.public.hash(state);
        self.origin.hash(state);
    }
}
