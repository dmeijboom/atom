use std::fmt::{Display, Formatter};

use crate::runtime::{AtomRef, Value};

use super::class::Class;
use super::r#fn::Fn;

#[derive(Debug, PartialEq)]
pub enum Receiver {
    Bound(Value),
    Unbound,
}

#[derive(Debug)]
pub struct Method {
    pub is_static: bool,
    pub receiver: Receiver,
    pub func: AtomRef<Fn>,
    pub class: AtomRef<Class>,
}

impl Display for Method {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.class.as_ref(), self.func.as_ref().name)
    }
}

impl PartialEq for Method {
    fn eq(&self, other: &Self) -> bool {
        self.func.as_ref() == other.func.as_ref()
            && self.receiver == other.receiver
            && self.class.as_ref() == other.class.as_ref()
    }
}
