use super::atom_ref::AtomRef;
use super::class::Class;
use super::result::Result;

pub trait AtomApi {
    fn find_class(&self, module_name: &str, class_name: &str) -> Result<AtomRef<Class>>;
}
