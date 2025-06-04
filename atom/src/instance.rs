use std::collections::HashMap;

use linear_map::LinearMap;
use nohash_hasher::IntMap;
use wyhash2::WyHash;

use crate::gc::{AnyHandle, Gc, Handle, Trace};
use crate::runtime::{class::Class, function::Fn};
use crate::vm::Error;
use crate::{Module, Value};

struct Cache {
    functions: Vec<Option<Handle<Fn>>>,
    classes: Vec<Option<Handle<Class>>>,
    classes_by_name: HashMap<String, usize, WyHash>,
    methods: IntMap<usize, LinearMap<String, Handle<Fn>>>,
}

impl Cache {
    pub fn new(module: &Module) -> Self {
        Self {
            methods: IntMap::default(),
            functions: vec![None; module.functions.len()],
            classes: vec![None; module.classes.len()],
            classes_by_name: module
                .classes
                .iter()
                .enumerate()
                .map(|(i, class)| (class.name.to_string(), i))
                .collect::<HashMap<_, _, WyHash>>(),
        }
    }
}

impl Trace for Cache {
    fn trace(&self, gc: &mut Gc) {
        self.classes.iter().flatten().for_each(|class| {
            gc.mark(class.boxed());
            class.trace(gc);

            if let Some(init) = &class.init {
                gc.mark(init.boxed());
                init.trace(gc);
            }
        });

        self.functions.iter().flatten().for_each(|h| {
            gc.mark(h.boxed());
            h.trace(gc);
        });
        self.methods
            .values()
            .flat_map(|methods| methods.values())
            .for_each(|h| {
                gc.mark(h.boxed());
                h.trace(gc);
            });
    }
}

pub struct Instance<const C: usize> {
    module: Module,
    cache: Cache,
    pub consts: [Value; C],
    pub vars: IntMap<u32, Value>,
}

impl<const C: usize> Trace for Instance<C> {
    fn trace(&self, gc: &mut Gc) {
        self.cache.trace(gc);
        self.consts
            .iter()
            .chain(self.vars.values())
            .for_each(|value| value.trace(gc));
    }
}

impl<const C: usize> Instance<C> {
    pub fn new(module: Module, consts: [Value; C]) -> Self {
        Self {
            vars: IntMap::default(),
            cache: Cache::new(&module),
            consts,
            module,
        }
    }

    pub fn get_class_by_name(
        &mut self,
        gc: &mut Gc,
        name: &str,
    ) -> Result<Option<Handle<Class>>, Error> {
        self.cache
            .classes_by_name
            .get(name)
            .copied()
            .map(|idx| self.get_class(gc, idx))
            .transpose()
    }

    pub fn get_class(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Class>, Error> {
        match &self.cache.classes[idx] {
            Some(handle) => Ok(Handle::clone(handle)),
            None => {
                let mut class = self.module.classes[idx].clone();

                if let Some(init) = class.methods.remove("init") {
                    let handle = gc.alloc(init)?;
                    class.init = Some(handle);
                }

                let handle = gc.alloc(class)?;
                self.cache.classes[idx] = Some(Handle::clone(&handle));

                Ok(handle)
            }
        }
    }

    pub fn get_fn(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Fn>, Error> {
        match &self.cache.functions[idx] {
            Some(handle) => Ok(Handle::clone(handle)),
            None => {
                let func = self.module.functions[idx].clone();
                let handle = gc.alloc(func)?;
                self.cache.functions[idx] = Some(Handle::clone(&handle));

                Ok(handle)
            }
        }
    }

    pub fn get_method(
        &mut self,
        gc: &mut Gc,
        class: &Handle<Class>,
        name: &str,
    ) -> Result<Option<Handle<Fn>>, Error> {
        if let Some(method) = self
            .cache
            .methods
            .get(&class.addr())
            .and_then(|entry| entry.get(name))
        {
            return Ok(Some(Handle::clone(method)));
        }

        match class.methods.get(name) {
            Some(method) => {
                let handle = gc.alloc(method.clone())?;
                let methods = self.cache.methods.entry(class.addr()).or_default();

                methods.insert(name.to_string(), Handle::clone(&handle));

                Ok(Some(handle))
            }
            None => Ok(None),
        }
    }
}
