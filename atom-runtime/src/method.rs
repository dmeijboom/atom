use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

use super::atom_ref::AtomRef;
use super::class::Class;
use super::r#fn::Fn;
use super::value::Value;

#[derive(Debug)]
pub struct Method {
    pub receiver: Value,
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

impl Hash for Method {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.func.as_ref().hash(state);
        self.receiver.hash(state);
        self.class.as_ref().hash(state);
    }
}
