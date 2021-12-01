use crate::runtime::types::{AtomRef, Class, Value};
use crate::runtime::Result;

pub trait AtomApi {
    fn find_class(&self, module_name: &str, class_name: &str) -> Result<AtomRef<Class>>;
    fn get_receiver(&self) -> Option<&Value>;
}
