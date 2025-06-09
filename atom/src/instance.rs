use std::collections::HashMap;

use linear_map::LinearMap;
use nohash_hasher::IntMap;
use wyhash2::WyHash;

use crate::gc::{DynHandle, Gc, Handle, Trace};
use crate::runtime::{class::Class, function::Fn, value::Value, Package};
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
        match self.get(idx) {
            Some(value) => Ok(value),
            None => {
                let value = f()?;
                self.values[idx] = Some(value.clone());
                Ok(value)
            }
        }
    }
}

struct Cache {
    functions: LazyCache<Handle<Fn>>,
    classes: LazyCache<Handle<Class>>,
    methods: LazyCache<HashMap<String, Handle<Fn>, WyHash>>,
}

impl Cache {
    pub fn new(classes: usize, functions: usize) -> Self {
        Self {
            classes: LazyCache::new(classes),
            functions: LazyCache::new(functions),
            methods: LazyCache::new(classes),
        }
    }
}

impl Trace for Cache {
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

pub struct Instance {
    id: usize,
    cache: Cache,
    package: Package,
    pub consts: Vec<Value>,
    pub vars: IntMap<u32, Value>,
}

impl Trace for Instance {
    fn trace(&self, gc: &mut Gc) {
        self.cache.trace(gc);
        self.consts
            .iter()
            .chain(self.vars.values())
            .for_each(|value| value.trace(gc));
    }
}

impl Instance {
    pub fn new(id: usize, package: Package, consts: Vec<Value>) -> Self {
        Self {
            id,
            vars: IntMap::default(),
            consts,
            cache: Cache::new(package.classes.len(), package.functions.len()),
            package,
        }
    }

    pub fn get_class_by_name(
        &mut self,
        gc: &mut Gc,
        name: &str,
    ) -> Result<Option<Handle<Class>>, Error> {
        match self.cache.classes.lookup(name, || {
            self.package.classes.iter().position(|c| c.name == name)
        }) {
            Some(idx) => self.get_class(gc, idx).map(Some),
            None => Ok(None),
        }
    }

    pub fn get_class(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Class>, Error> {
        self.cache.classes.get_or_insert(idx, || {
            let mut class = self.package.classes[idx].clone();

            if let Some(mut init) = class.methods.remove("init") {
                init.inline.instance = self.id;
                let handle = gc.alloc(init)?;
                class.init = Some(handle);
            }

            let handle = gc.alloc(class)?;
            Ok(handle)
        })
    }

    pub fn get_fn_by_name(&mut self, gc: &mut Gc, name: &str) -> Result<Option<Handle<Fn>>, Error> {
        match self.cache.functions.lookup(name, || {
            self.package.functions.iter().position(|f| f.name == name)
        }) {
            Some(idx) => self.get_fn(gc, idx).map(Some),
            None => Ok(None),
        }
    }

    pub fn get_fn(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Fn>, Error> {
        self.cache.functions.get_or_insert(idx, || {
            let mut func = self.package.functions[idx].clone();
            func.inline.instance = self.id;
            let handle = gc.alloc(func)?;
            Ok(handle)
        })
    }
}
