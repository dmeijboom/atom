use std::borrow::Cow;

use linear_map::LinearMap;

use crate::gc::{Gc, Handle, Trace};

use super::{error::RuntimeError, function::Fn};

#[derive(Debug, Clone)]
pub struct Context<'gc> {
    pub module: usize,
    pub methods: LinearMap<Cow<'static, str>, Handle<'gc, Fn>>,
}

impl<'gc> Context<'gc> {
    pub fn new(module: usize) -> Self {
        Self {
            module,
            methods: LinearMap::new(),
        }
    }
}

impl<'gc> Trace for Context<'gc> {
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
    pub context: Context<'gc>,
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
        self.context.trace(gc);

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
            context: Context::new(module),
            init: None,
            methods: LinearMap::new(),
        }
    }

    pub fn get_method(
        &mut self,
        gc: &mut Gc<'gc>,
        name: &str,
    ) -> Result<Option<Handle<'gc, Fn>>, RuntimeError> {
        match self.context.methods.get(name) {
            Some(handle) => Ok(Some(Handle::clone(handle))),
            None => match self.methods.get(name) {
                Some(func) => {
                    let mut handle = gc.alloc(func.clone())?;
                    handle.context.module = self.context.module;

                    self.context
                        .methods
                        .insert(Cow::clone(&func.name), Handle::clone(&handle));
                    Ok(Some(handle))
                }
                None => Ok(None),
            },
        }
    }
}
