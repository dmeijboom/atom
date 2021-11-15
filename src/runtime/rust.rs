use std::any::Any;
use std::fmt;
use std::fmt::{Debug, Formatter};

use super::result::{Result, RuntimeError};

pub struct RustObject {
    any: Box<dyn Any>,
}

impl RustObject {
    pub fn new(data: impl Any) -> Self {
        Self {
            any: Box::new(data),
        }
    }

    pub fn try_as_ref<T: Any>(&self) -> Result<&T> {
        self.any
            .as_ref()
            .downcast_ref()
            .ok_or_else(|| RuntimeError::new("invalid type for RustObject".to_string()))
    }

    pub fn try_as_mut<T: Any>(&mut self) -> Result<&mut T> {
        self.any
            .as_mut()
            .downcast_mut()
            .ok_or_else(|| RuntimeError::new("invalid type for RustObject".to_string()))
    }
}

impl Debug for RustObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "RustObject<{:?}>", self.type_id())
    }
}

impl PartialEq for RustObject {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}
