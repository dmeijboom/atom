use std::borrow::Cow;

use linear_map::LinearMap;

use crate::{
    collections::IntMap,
    gc::{Gc, Handle, Trace},
};

use super::{error::RuntimeError, function::Fn, value::Value};

#[derive(Debug, Clone)]
pub struct Inline<'gc> {
    pub module: usize,
    pub methods: LinearMap<Cow<'static, str>, Handle<'gc, Fn>>,
}

impl<'gc> Inline<'gc> {
    pub fn new(module: usize) -> Self {
        Self {
            module,
            methods: LinearMap::new(),
        }
    }
}

impl<'gc> Trace for Inline<'gc> {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        for handle in self.methods.values() {
            gc.mark(handle);
        }
    }
}

#[derive(Debug, Clone)]
pub struct Class<'gc> {
    pub name: Cow<'static, str>,
    pub public: bool,
    pub inline: Inline<'gc>,
    pub init: Option<Handle<'gc, Fn>>,
    pub methods: LinearMap<Cow<'static, str>, Fn>,
}

impl<'gc> PartialEq for Class<'gc> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'gc> Trace for Class<'gc> {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        self.inline.trace(gc);

        if let Some(init) = &self.init {
            gc.mark(init);
        }
    }
}

impl<'gc> Class<'gc> {
    pub fn new(name: impl Into<Cow<'static, str>>, module: usize) -> Self {
        Self {
            name: name.into(),
            public: false,
            inline: Inline::new(module),
            init: None,
            methods: LinearMap::new(),
        }
    }

    pub fn get_method(
        &mut self,
        gc: &mut Gc<'gc>,
        name: &str,
    ) -> Result<Option<Handle<'gc, Fn>>, RuntimeError> {
        match self.inline.methods.get(name) {
            Some(handle) => Ok(Some(Handle::clone(handle))),
            None => match self.methods.get(name) {
                Some(func) => {
                    let mut handle = gc.alloc(func.clone())?;
                    handle.context.module = self.inline.module;

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

pub struct Object<'gc> {
    pub class: Handle<'gc, Class<'gc>>,
    attrs: IntMap<u32, Value<'gc>>,
}

impl<'gc> Trace for Object<'gc> {
    fn trace(&self, gc: &mut crate::gc::Gc) {
        gc.mark(&self.class);

        for value in self.attrs.values() {
            value.trace(gc);
        }
    }
}

impl<'gc> Object<'gc> {
    pub fn new(class: Handle<'gc, Class<'gc>>) -> Self {
        Self {
            class,
            attrs: IntMap::default(),
        }
    }

    #[inline]
    pub fn get_attr(&self, key: u32) -> Option<Value<'gc>> {
        self.attrs.get(&key).cloned()
    }

    #[inline]
    pub fn set_attr(&mut self, key: u32, value: Value<'gc>) {
        self.attrs.insert(key, value);
    }
}
