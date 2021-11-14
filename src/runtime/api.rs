use super::atom_ref::AtomRef;
use super::class::Class;
use super::result::Result;
use super::value::Value;

pub trait AtomApi {
    fn find_class(&self, module_name: &str, class_name: &str) -> Result<AtomRef<Class>>;
    fn get_receiver(&self) -> Option<Value>;
}
