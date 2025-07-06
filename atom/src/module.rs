use std::borrow::Cow;
use std::collections::HashMap;

use wyhash2::WyHash;

use crate::collections::IntMap;
use crate::compiler::Package;
use crate::gc::{DynHandle, Gc, Handle, Trace};
use crate::runtime::{Class, Fn, IntoAtom, Value};
use crate::vm::Error;

fn get_cached<T, E, F>(vec: &mut OptVec<T>, idx: usize, f: F) -> Result<T, Error>
where
    T: Clone,
    F: FnOnce() -> Result<T, E>,
    Error: From<E>,
{
    match vec[idx] {
        Some(ref value) => Ok(value.clone()),
        None => {
            let value = f()?;
            vec[idx] = Some(value.clone());
            Ok(value)
        }
    }
}

macro_rules! lookup_cached {
    ($container:expr, $key:expr, $block:block) => {
        match $container.get($key) {
            Some(value) => Ok(value.clone()),
            None => {
                let value = $block?;
                $container.insert($key.to_string(), value.clone());
                Ok(value)
            }
        }
    };
}

type OptVec<T> = Vec<Option<T>>;
type OptMap<T> = HashMap<String, Option<T>, WyHash>;

struct Cache<'gc> {
    class_lookup: OptMap<Handle<'gc, Class<'gc>>>,
    fn_lookup: OptMap<Handle<'gc, Fn>>,
    consts: OptVec<Value<'gc>>,
    functions: OptVec<Handle<'gc, Fn>>,
    classes: OptVec<Handle<'gc, Class<'gc>>>,
    methods: OptVec<HashMap<String, Handle<'gc, Fn>, WyHash>>,
}

fn mark<'a, T>(gc: &mut Gc<'_>, iter: impl Iterator<Item = &'a Option<T>>)
where
    T: DynHandle + 'a,
{
    iter.flatten().for_each(|handle| {
        gc.mark(handle);
    });
}

impl<'gc> Cache<'gc> {
    pub fn new(consts: usize, classes: usize, functions: usize) -> Self {
        Self {
            consts: vec![None; consts],
            classes: vec![None; classes],
            functions: vec![None; functions],
            methods: vec![None; classes],
            class_lookup: HashMap::default(),
            fn_lookup: HashMap::default(),
        }
    }
}

impl<'gc> Trace for Cache<'gc> {
    fn trace(&self, gc: &mut Gc) {
        self.consts.iter().flatten().for_each(|value| {
            value.trace(gc);
        });
        mark(gc, self.fn_lookup.values().chain(self.functions.iter()));
        mark(gc, self.class_lookup.values().chain(self.classes.iter()));
        self.methods
            .iter()
            .flatten()
            .flat_map(|map| map.values())
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

    pub fn load_class_by_name(
        &mut self,
        gc: &mut Gc<'gc>,
        name: &str,
    ) -> Result<Option<Handle<'gc, Class<'gc>>>, Error> {
        lookup_cached!(self.cache.class_lookup, name, {
            self.package
                .classes
                .iter()
                .filter(|c| c.public)
                .position(|c| c.name == name)
                .map(|idx| self.load_class(gc, idx))
                .transpose()
        })
    }

    pub fn load_class(
        &mut self,
        gc: &mut Gc<'gc>,
        idx: usize,
    ) -> Result<Handle<'gc, Class<'gc>>, Error> {
        get_cached(&mut self.cache.classes, idx, || {
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

            gc.alloc(class)
        })
    }

    pub fn load_fn_by_name(
        &mut self,
        gc: &mut Gc<'gc>,
        name: &str,
    ) -> Result<Option<Handle<'gc, Fn>>, Error> {
        lookup_cached!(self.cache.fn_lookup, name, {
            self.package
                .functions
                .iter()
                .filter(|f| f.public)
                .position(|f| f.name == name)
                .map(|idx| self.load_fn(gc, idx))
                .transpose()
        })
    }

    pub fn load_fn(&mut self, gc: &mut Gc<'gc>, idx: usize) -> Result<Handle<'gc, Fn>, Error> {
        get_cached(&mut self.cache.functions, idx, || {
            let mut func = self.package.functions[idx].clone();
            func.context.module = self.id;
            gc.alloc(func)
        })
    }

    pub fn load_const(&mut self, gc: &mut Gc<'gc>, idx: usize) -> Result<Value<'gc>, Error> {
        get_cached(&mut self.cache.consts, idx, || {
            self.package.consts[idx].clone().into_atom(gc)
        })
    }
}
