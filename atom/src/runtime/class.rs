use std::borrow::Cow;

use linear_map::LinearMap;

use crate::gc::{Gc, Handle, Trace};

use super::{error::RuntimeError, function::Fn, value::Value};

#[derive(Debug, Default, Clone)]
pub struct Inline {
    pub instance: usize,
    pub methods: LinearMap<Cow<'static, str>, Handle<Fn>>,
}

impl Trace for Inline {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        for handle in self.methods.values() {
            gc.mark(handle);
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct Class {
    pub name: Cow<'static, str>,
    pub public: bool,
    pub inline: Inline,
    pub init: Option<Handle<Fn>>,
    pub methods: LinearMap<Cow<'static, str>, Fn>,
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Trace for Class {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        self.inline.trace(gc);

        if let Some(init) = &self.init {
            gc.mark(init);
        }
    }
}

impl Class {
    pub fn new(name: impl Into<Cow<'static, str>>) -> Self {
        Self {
            name: name.into(),
            ..Self::default()
        }
    }

    pub fn get_method(
        &mut self,
        gc: &mut Gc,
        name: &str,
    ) -> Result<Option<Handle<Fn>>, RuntimeError> {
        match self.inline.methods.get(name) {
            Some(handle) => Ok(Some(Handle::clone(handle))),
            None => match self.methods.get(name) {
                Some(func) => {
                    let mut handle = gc.alloc(func.clone())?;
                    handle.inline.instance = self.inline.instance;

                    self.inline
                        .methods
                        .insert(Cow::clone(&func.name), Handle::clone(&handle));
                    Ok(Some(handle))
                }
                None => Ok(None),
            },
        }
    }
}

pub struct Object {
    pub class: Handle<Class>,
    pub attrs: LinearMap<Cow<'static, str>, Value>,
}

impl Trace for Object {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        gc.mark(&self.class);

        for value in self.attrs.values() {
            value.trace(gc);
        }
    }
}

impl Object {
    pub fn new(class: Handle<Class>) -> Self {
        Self {
            class,
            attrs: LinearMap::default(),
        }
    }
}
