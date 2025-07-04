use std::borrow::Cow;
use std::collections::HashMap;

use wyhash2::WyHash;

use crate::collections::IntMap;
use crate::compiler::Package;
use crate::gc::{DynHandle, Gc, Handle, Trace};
use crate::runtime::{Class, Fn, IntoAtom, Value};
use crate::vm::Error;

#[derive(Default)]
struct LookupTable(HashMap<String, usize, WyHash>);

impl LookupTable {
    pub fn get_or_insert(
        &mut self,
        name: &str,
        f: impl FnOnce() -> Option<usize>,
    ) -> Option<usize> {
        match self.0.get(name).copied() {
            Some(idx) => Some(idx),
            None => match f() {
                Some(idx) => {
                    self.0.insert(name.to_string(), idx);
                    Some(idx)
                }
                None => None,
            },
        }
    }
}

struct LazyCache<T>(Vec<Option<T>>);

impl<T> LazyCache<T> {
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.iter().flatten()
    }
}

impl<T: DynHandle> Trace for LazyCache<T> {
    fn trace(&self, gc: &mut Gc) {
        self.iter().for_each(|handle| gc.mark(handle));
    }
}

impl<T: Clone> LazyCache<T> {
    pub fn new(size: usize) -> Self {
        Self(vec![None; size])
    }

    pub fn get(&self, idx: usize) -> Option<T> {
        self.0[idx].as_ref().cloned()
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
        self.0[idx] = Some(value.clone());
        Ok(value)
    }
}

struct Cache<'gc> {
    classes_lookup: LookupTable,
    functions_lookup: LookupTable,
    consts: LazyCache<Value<'gc>>,
    functions: LazyCache<Handle<'gc, Fn>>,
    classes: LazyCache<Handle<'gc, Class<'gc>>>,
    methods: LazyCache<HashMap<String, Handle<'gc, Fn>, WyHash>>,
}

impl<'gc> Cache<'gc> {
    pub fn new(consts: usize, classes: usize, functions: usize) -> Self {
        Self {
            consts: LazyCache::new(consts),
            classes: LazyCache::new(classes),
            classes_lookup: LookupTable::default(),
            functions_lookup: LookupTable::default(),
            functions: LazyCache::new(functions),
            methods: LazyCache::new(classes),
        }
    }
}

impl<'gc> Trace for Cache<'gc> {
    fn trace(&self, gc: &mut Gc) {
        self.classes.trace(gc);
        self.functions.trace(gc);
        self.consts.iter().for_each(|value| {
            value.trace(gc);
        });
        self.methods
            .iter()
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
    pub vars: IntMap<u32, Value<'gc>>,
}

impl<'gc> Trace for Module<'gc> {
    fn trace(&self, gc: &mut Gc) {
        self.cache.trace(gc);
    }
}

impl<'gc> Module<'gc> {
    pub fn new(id: usize, meta: Metadata, package: Package) -> Self {
        Self {
            id,
            meta,
            vars: IntMap::default(),
            cache: Cache::new(
                package.consts.len(),
                package.classes.len(),
                package.functions.len(),
            ),
            package,
        }
    }

    pub fn metadata(&self) -> &Metadata {
        &self.meta
    }

    pub fn package(&self) -> &Package {
        &self.package
    }

    pub fn load_class_by_name(
        &mut self,
        gc: &mut Gc<'gc>,
        name: &str,
    ) -> Result<Option<Handle<'gc, Class<'gc>>>, Error> {
        match self.cache.classes_lookup.get_or_insert(name, || {
            self.package.classes.iter().position(|c| c.name == name)
        }) {
            Some(idx) => self.load_class(gc, idx).map(Some),
            None => Ok(None),
        }
    }

    pub fn load_class(
        &mut self,
        gc: &mut Gc<'gc>,
        idx: usize,
    ) -> Result<Handle<'gc, Class<'gc>>, Error> {
        self.cache.classes.get_or_insert(idx, || {
            let orig = &mut self.package.classes[idx];
            let mut class = Class::new(orig.name.clone(), self.id);

            if let Some(mut init) = orig.methods.remove("init") {
                init.context = class.context.clone();
                class.init = Some(gc.alloc(init)?);
            }

            class.methods = orig
                .methods
                .iter()
                .map(|(name, func)| (name.clone(), func.clone()))
                .collect();

            Ok(gc.alloc(class)?)
        })
    }

    pub fn load_fn_by_name(
        &mut self,
        gc: &mut Gc<'gc>,
        name: &str,
    ) -> Result<Option<Handle<'gc, Fn>>, Error> {
        match self.cache.functions_lookup.get_or_insert(name, || {
            self.package.functions.iter().position(|f| f.name == name)
        }) {
            Some(idx) => self.load_fn(gc, idx).map(Some),
            None => Ok(None),
        }
    }

    pub fn load_fn(&mut self, gc: &mut Gc<'gc>, idx: usize) -> Result<Handle<'gc, Fn>, Error> {
        self.cache.functions.get_or_insert(idx, || {
            let mut func = self.package.functions[idx].clone();
            func.context.module = self.id;
            let handle = gc.alloc(func)?;
            Ok(handle)
        })
    }

    pub fn load_const(&mut self, gc: &mut Gc<'gc>, idx: usize) -> Result<Value<'gc>, Error> {
        self.cache.consts.get_or_insert(idx, || {
            Ok(self.package.consts[idx].clone().into_atom(gc)?)
        })
    }
}
