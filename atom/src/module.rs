use std::borrow::Cow;
use std::collections::HashMap;

use linear_map::LinearMap;
use wyhash2::WyHash;

use crate::collections::IntMap;
use crate::compiler::Package;
use crate::gc::{DynHandle, Gc, Handle, Trace};
use crate::runtime::class::Context;
use crate::runtime::{class::Class, function::Fn, value::Value};
use crate::vm::Error;

struct LazyCache<T> {
    values: Vec<Option<T>>,
    lookup: LinearMap<String, usize>,
}

impl<T: DynHandle> Trace for LazyCache<T> {
    fn trace(&self, gc: &mut Gc) {
        self.values
            .iter()
            .flatten()
            .for_each(|handle| gc.mark(handle));
    }
}

impl<T: Clone> LazyCache<T> {
    pub fn new(size: usize) -> Self {
        Self {
            values: vec![None; size],
            lookup: LinearMap::new(),
        }
    }

    pub fn lookup(&mut self, name: &str, f: impl FnOnce() -> Option<usize>) -> Option<usize> {
        match self.lookup.get(name).copied() {
            Some(idx) => Some(idx),
            None => match f() {
                Some(idx) => {
                    self.lookup.insert(name.to_string(), idx);
                    Some(idx)
                }
                None => None,
            },
        }
    }

    pub fn get(&self, idx: usize) -> Option<T> {
        self.values[idx].as_ref().cloned()
    }

    pub fn get_or_insert<E>(
        &mut self,
        idx: usize,
        f: impl FnOnce() -> Result<T, E>,
    ) -> Result<T, E> {
        if let Some(value) = self.get(idx) {
            return Ok(value);
        }

        let value = f()?;
        self.values[idx] = Some(value.clone());
        Ok(value)
    }
}

struct Cache<'gc> {
    functions: LazyCache<Handle<'gc, Fn>>,
    classes: LazyCache<Handle<'gc, Class<'gc>>>,
    methods: LazyCache<HashMap<String, Handle<'gc, Fn>, WyHash>>,
}

impl<'gc> Cache<'gc> {
    pub fn new(classes: usize, functions: usize) -> Self {
        Self {
            classes: LazyCache::new(classes),
            functions: LazyCache::new(functions),
            methods: LazyCache::new(classes),
        }
    }
}

impl<'gc> Trace for Cache<'gc> {
    fn trace(&self, gc: &mut Gc) {
        self.classes.trace(gc);
        self.functions.trace(gc);
        self.methods
            .values
            .iter()
            .flatten()
            .flat_map(|value| value.values())
            .for_each(|handle| {
                gc.mark(handle);
            });
    }
}

#[derive(Debug)]
pub struct Metadata {
    pub name: Cow<'static, str>,
}

impl Metadata {
    pub fn new(name: String) -> Self {
        Self {
            name: Cow::Owned(name),
        }
    }
}

impl Default for Metadata {
    fn default() -> Self {
        Self {
            name: Cow::Borrowed("__root__"),
        }
    }
}

pub struct Module<'gc> {
    id: usize,
    meta: Metadata,
    cache: Cache<'gc>,
    package: Package,
    pub consts: Vec<Value<'gc>>,
    pub vars: IntMap<u32, Value<'gc>>,
}

impl<'gc> Trace for Module<'gc> {
    fn trace(&self, gc: &mut Gc) {
        self.cache.trace(gc);
        self.consts
            .iter()
            .chain(self.vars.values())
            .for_each(|value| value.trace(gc));
    }
}

impl<'gc> Module<'gc> {
    pub fn new(id: usize, meta: Metadata, package: Package, consts: Vec<Value<'gc>>) -> Self {
        Self {
            id,
            meta,
            vars: IntMap::default(),
            consts,
            cache: Cache::new(package.classes.len(), package.functions.len()),
            package,
        }
    }

    pub fn metadata(&self) -> &Metadata {
        &self.meta
    }

    pub fn package(&self) -> &Package {
        &self.package
    }

    pub fn get_class_by_name(
        &mut self,
        gc: &mut Gc<'gc>,
        name: &str,
    ) -> Result<Option<Handle<'gc, Class<'gc>>>, Error> {
        match self.cache.classes.lookup(name, || {
            self.package.classes.iter().position(|c| c.name == name)
        }) {
            Some(idx) => self.get_class(gc, idx).map(Some),
            None => Ok(None),
        }
    }

    pub fn get_class(
        &mut self,
        gc: &mut Gc<'gc>,
        idx: usize,
    ) -> Result<Handle<'gc, Class<'gc>>, Error> {
        self.cache.classes.get_or_insert(idx, || {
            let orig = &mut self.package.classes[idx];
            let class = Class {
                name: orig.name.clone(),
                public: orig.public,
                context: Context::new(self.id),
                init: orig
                    .methods
                    .remove("init")
                    .map(|mut init| {
                        init.context.module = self.id;
                        gc.alloc(init)
                    })
                    .transpose()?,
                methods: orig
                    .methods
                    .iter()
                    .map(|(name, func)| (name.clone(), func.clone()))
                    .collect(),
            };

            let handle = gc.alloc(class)?;
            Ok(handle)
        })
    }

    pub fn get_fn_by_name(
        &mut self,
        gc: &mut Gc<'gc>,
        name: &str,
    ) -> Result<Option<Handle<'gc, Fn>>, Error> {
        match self.cache.functions.lookup(name, || {
            self.package.functions.iter().position(|f| f.name == name)
        }) {
            Some(idx) => self.get_fn(gc, idx).map(Some),
            None => Ok(None),
        }
    }

    pub fn get_fn(&mut self, gc: &mut Gc<'gc>, idx: usize) -> Result<Handle<'gc, Fn>, Error> {
        self.cache.functions.get_or_insert(idx, || {
            let mut func = self.package.functions[idx].clone();
            func.context.module = self.id;
            let handle = gc.alloc(func)?;
            Ok(handle)
        })
    }
}
