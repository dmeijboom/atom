use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use indexmap::map::IndexMap;

use crate::compiler::ir::IR;
use crate::runtime::value::Convert;
use crate::runtime::RuntimeError;

use super::api::AtomApi;
use super::origin::Origin;
use super::result::Result;
use super::value::Value;

pub struct Input<'i> {
    pub api: &'i dyn AtomApi,
    pub args: Vec<Value>,
}

impl<'i> Input<'i> {
    pub fn new(api: &'i dyn AtomApi, args: Vec<Value>) -> Self {
        Self { api, args }
    }

    pub fn single(self) -> Value {
        let mut args = self.take_args();

        args.swap_remove(0)
    }

    pub fn take_args(self) -> Vec<Value> {
        self.args
    }

    pub fn get_receiver(&mut self) -> Result<&Value> {
        self.api
            .get_receiver()
            .ok_or_else(|| RuntimeError::new("missing receiver".to_string()))
    }

    pub fn take_receiver<T>(&mut self) -> Result<T>
    where
        Value: Convert<T>,
    {
        self.api
            .get_receiver()
            .ok_or_else(|| RuntimeError::new("missing receiver".to_string()))?
            .clone()
            .convert()
    }

    pub fn pop_first<T>(&mut self) -> Result<T>
    where
        Value: Convert<T>,
    {
        if self.args.is_empty() {
            return Err(RuntimeError::new("missing argument".to_string()));
        }

        self.args.remove(0).convert()
    }
}

pub type ExternalFn = fn(input: Input<'_>) -> Result<Option<Value>>;

#[derive(Clone)]
pub enum FnPtr {
    External(ExternalFn),
    Native(Rc<IR>),
}

impl Debug for FnPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FnPtr::External(_) => write!(f, "*ExternalFn"),
            FnPtr::Native(_) => write!(f, "*Fn"),
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
    pub ptr: FnPtr,
    pub origin: Origin,
    pub args: IndexMap<String, FnArg>,
}

impl Fn {
    pub fn native(name: String, origin: Origin, args: IndexMap<String, FnArg>, ir: IR) -> Self {
        Self {
            name,
            origin,
            args,
            ptr: FnPtr::Native(Rc::new(ir)),
        }
    }

    pub fn external(name: String, origin: Origin, func: ExternalFn) -> Self {
        Self {
            name,
            origin,
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
        self.name == other.name && self.origin == other.origin
    }
}
