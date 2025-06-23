use std::borrow::Cow;

use linear_map::LinearMap;

use crate::{
    collections::IntMap,
    gc::{Gc, Handle, Trace},
};

use super::{
    error::RuntimeError,
    function::Fn,
    value::{IntoAtom, Value},
};

#[derive(Debug, Clone)]
pub struct Inline<'gc> {
    pub instance: usize,
    pub methods: LinearMap<Cow<'static, str>, Handle<'gc, Fn>>,
}

impl<'gc> Inline<'gc> {
    pub fn new(instance: usize) -> Self {
        Self {
            instance,
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
    pub fn new(name: impl Into<Cow<'static, str>>, instance: usize) -> Self {
        Self {
            name: name.into(),
            public: false,
            inline: Inline::new(instance),
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
                    handle.context.instance = self.inline.instance;

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
    pub attrs: IntMap<u32, Value<'gc>>,
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

    pub fn with_attr<T>(
        gc: &mut Gc<'gc>,
        class: Handle<'gc, Class<'gc>>,
        attrs: Vec<(u32, T)>,
    ) -> Result<Self, RuntimeError>
    where
        T: IntoAtom<'gc>,
    {
        let attrs = attrs
            .into_iter()
            .map(|(k, v)| Ok((k, v.into_atom(gc)?)))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            class,
            attrs: IntMap::from_iter(attrs),
        })
    }
}
