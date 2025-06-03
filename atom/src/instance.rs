use std::collections::HashMap;

use nohash_hasher::IntMap;
use wyhash2::WyHash;

use crate::gc::{AnyHandle, Gc, Handle, Trace};
use crate::runtime::{class::Class, function::Fn};
use crate::vm::Error;
use crate::{Module, Value};

pub struct Instance<const C: usize> {
    module: Module,
    pub consts: [Value; C],
    pub vars: IntMap<usize, Value>,
    functions: Vec<Option<Handle<Fn>>>,
    classes: Vec<Option<Handle<Class>>>,
    classes_by_name: HashMap<String, usize, WyHash>,
    methods: IntMap<usize, HashMap<String, Handle<Fn>, WyHash>>,
}

impl<const C: usize> Trace for Instance<C> {
    fn trace(&self, gc: &mut Gc) {
        self.consts
            .iter()
            .chain(self.vars.values())
            .for_each(|value| value.trace(gc));

        self.classes
            .iter()
            .filter_map(|c| match c {
                Some(handle) => Some(handle),
                None => None,
            })
            .for_each(|value| value.trace(gc));

        self.methods
            .values()
            .flat_map(|methods| methods.values())
            .for_each(|h| {
                h.trace(gc);
                gc.mark(h.boxed());
            });
    }
}

impl<const C: usize> Instance<C> {
    pub fn new(module: Module, consts: [Value; C]) -> Self {
        Self {
            consts,
            vars: IntMap::default(),
            methods: IntMap::default(),
            functions: vec![None; module.functions.len()],
            classes: vec![None; module.classes.len()],
            classes_by_name: module
                .classes
                .iter()
                .enumerate()
                .map(|(i, class)| (class.name.to_string(), i))
                .collect::<HashMap<_, _, WyHash>>(),
            module,
        }
    }

    pub fn get_class_by_name(
        &mut self,
        gc: &mut Gc,
        name: &str,
    ) -> Result<Option<Handle<Class>>, Error> {
        self.classes_by_name
            .get(name)
            .copied()
            .map(|idx| self.get_class(gc, idx))
            .transpose()
    }

    pub fn get_class(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Class>, Error> {
        match &self.classes[idx] {
            Some(handle) => Ok(Handle::clone(&handle)),
            None => {
                let class = self.module.classes[idx].clone();
                let handle = gc.alloc(class)?;
                self.classes[idx] = Some(Handle::clone(&handle));

                Ok(handle)
            }
        }
    }

    pub fn get_fn(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Fn>, Error> {
        match &self.functions[idx] {
            Some(handle) => Ok(Handle::clone(&handle)),
            None => {
                let func = self.module.functions[idx].clone();
                let handle = gc.alloc(func)?;
                self.functions[idx] = Some(Handle::clone(&handle));

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
        if let Some(entry) = self.methods.get(&class.addr()) {
            if let Some(method) = entry.get(name) {
                return Ok(Some(Handle::clone(method)));
            }
        }

        match class.methods.get(name) {
            Some(method) => {
                let handle = gc.alloc(method.clone())?;
                let methods = self.methods.entry(class.addr()).or_default();

                methods.insert(name.to_string(), Handle::clone(&handle));

                Ok(Some(handle))
            }
            None => Ok(None),
        }
    }
}
