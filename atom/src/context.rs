use std::collections::HashMap;

use nohash_hasher::IntMap;
use wyhash2::WyHash;

use crate::gc::{AnyHandle, Gc, Handle, Trace};
use crate::runtime::{class::Class, function::Fn};
use crate::vm::Error;
use crate::{Module, Value};

fn map_handle<T: Trace + 'static, K, S>(
    map: &HashMap<K, Handle<T>, S>,
) -> impl Iterator<Item = Box<dyn AnyHandle>> + '_ {
    map.values().map(Handle::boxed)
}

#[derive(Default)]
struct Cache {
    functions: IntMap<usize, Handle<Fn>>,
    classes: IntMap<usize, Handle<Class>>,
    classes_by_name: HashMap<String, usize, WyHash>,
    methods: IntMap<usize, HashMap<String, Handle<Fn>, WyHash>>,
}

impl Trace for Cache {
    fn trace(&self, gc: &mut Gc) {
        map_handle(&self.classes)
            .chain(map_handle(&self.classes))
            .chain(self.methods.values().flat_map(map_handle))
            .for_each(|h| {
                h.trace(gc);
                gc.mark(h);
            });
    }
}

pub struct Context<const C: usize> {
    cache: Cache,
    module: Module,
    pub consts: [Value; C],
    pub vars: IntMap<usize, Value>,
}

impl<const C: usize> Trace for Context<C> {
    fn trace(&self, gc: &mut Gc) {
        self.consts
            .iter()
            .chain(self.vars.values())
            .for_each(|value| value.trace(gc));

        self.cache.trace(gc);
    }
}

impl<const C: usize> Context<C> {
    pub fn new(module: Module, consts: [Value; C]) -> Self {
        Self {
            cache: Cache::default(),
            module,
            consts,
            vars: IntMap::default(),
        }
    }

    pub fn load_class_by_name(
        &mut self,
        gc: &mut Gc,
        name: &str,
    ) -> Result<Option<Handle<Class>>, Error> {
        let idx = match self.cache.classes_by_name.get(name) {
            Some(idx) => *idx,
            None => match self
                .module
                .functions
                .iter()
                .position(|class| class.name == name)
            {
                Some(idx) => idx,
                None => return Ok(None),
            },
        };

        self.load_class(gc, idx).map(Some)
    }

    pub fn load_class(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Class>, Error> {
        match self.cache.classes.get(&idx) {
            Some(class) => Ok(Handle::clone(class)),
            None => {
                let class = self.module.classes[idx].clone();
                let handle = gc.alloc(class)?;

                self.cache.classes.insert(idx, Handle::clone(&handle));

                Ok(handle)
            }
        }
    }

    pub fn load_fn(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Fn>, Error> {
        match self.cache.functions.get(&idx) {
            Some(f) => Ok(Handle::clone(f)),
            None => {
                let class = self.module.functions[idx].clone();
                let handle = gc.alloc(class)?;

                self.cache.functions.insert(idx, Handle::clone(&handle));

                Ok(handle)
            }
        }
    }

    pub fn load_method(
        &mut self,
        gc: &mut Gc,
        class: &Handle<Class>,
        name: &str,
    ) -> Result<Option<Handle<Fn>>, Error> {
        match self.cache.methods.get(&class.addr()) {
            Some(entry) => match entry.get(name) {
                Some(method) => Ok(Some(Handle::clone(method))),
                None => Ok(None),
            },
            None => match class.methods.get(name) {
                Some(method) => {
                    let handle = gc.alloc(method.clone())?;
                    let methods = self.cache.methods.entry(class.addr()).or_default();

                    methods.insert(name.to_string(), Handle::clone(&handle));

                    Ok(Some(handle))
                }
                None => Ok(None),
            },
        }
    }
}
