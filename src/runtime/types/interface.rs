use std::fmt::{Display, Formatter};

use crate::runtime::Origin;

#[derive(Debug, PartialEq)]
pub struct Interface {
    pub name: String,
    pub origin: Origin,
    pub functions: Vec<String>,
}

impl Display for Interface {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.origin.module_name, self.name)
    }
}
