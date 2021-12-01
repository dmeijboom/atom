use std::fmt::{Debug, Display, Formatter};

use indexmap::map::IndexMap;

use crate::compiler::ir::IR;
use crate::runtime::types::Value;
use crate::runtime::{AtomApi, Convert, ErrorKind, Origin, Result, RuntimeError};

pub struct Input<'i> {
    pub api: &'i dyn AtomApi,
    pub args: Vec<Value>,
}

impl<'i> Input<'i> {
    pub fn new(api: &'i dyn AtomApi, args: Vec<Value>) -> Self {
        Self { api, args }
    }

    pub fn take_receiver<T>(&mut self) -> Result<T>
    where
        Value: Convert<T>,
    {
        self.api
            .get_receiver()
            .ok_or_else(|| {
                RuntimeError::new(ErrorKind::FatalError, "missing receiver".to_string())
            })?
            .clone()
            .convert()
    }

    pub fn take_arg<T>(&mut self) -> Result<T>
    where
        Value: Convert<T>,
    {
        if self.args.is_empty() {
            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                "missing argument".to_string(),
            ));
        }

        self.args.remove(0).convert()
    }
}

pub type ExternalFn = fn(input: Input<'_>) -> Result<Value>;

#[derive(Clone)]
pub enum FnKind {
    External(ExternalFn),
    Native,
}

impl Debug for FnKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FnKind::External(_) => write!(f, "*ExternalFn"),
            FnKind::Native => write!(f, "*Fn"),
        }
    }
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
    pub kind: FnKind,
    // If `void` is true the function will never return a value
    pub void: bool,
    // Only used for instance methods
    pub public: bool,
    pub origin: Origin,
    pub args: IndexMap<String, FnArg>,
    pub ir: IR,
}

impl Fn {
    pub fn native(
        name: String,
        public: bool,
        void: bool,
        origin: Origin,
        args: IndexMap<String, FnArg>,
        instructions: IR,
    ) -> Self {
        Self {
            name,
            origin,
            args,
            public,
            void,
            kind: FnKind::Native,
            ir: instructions,
        }
    }

    pub fn external(name: String, public: bool, origin: Origin, func: ExternalFn) -> Self {
        Self {
            name,
            origin,
            args: IndexMap::new(),
            public,
            void: false,
            kind: FnKind::External(func),
            ir: IR::new(),
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
        self.name == other.name && self.origin == other.origin
    }
}
