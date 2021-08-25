use std::fmt::{Display, Formatter};

use crate::Origin;

#[derive(Debug, PartialEq, Hash)]
pub struct Interface {
    pub name: String,
    pub origin: Origin,
    pub public: bool,
    pub functions: Vec<String>,
}

impl Display for Interface {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.origin.module_name, self.name)
    }
}
