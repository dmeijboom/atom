use std::{collections::HashMap, rc::Rc};

use wyhash2::WyHash;

use crate::gc::Gc;

pub mod array;
pub mod core;

use super::{
    error::Error,
    function::Func,
    value::{Type, Value},
};

pub type TypeRegistry = HashMap<Type, TypeDescr, WyHash>;

type FieldHandler = dyn Fn(&mut Gc, Value) -> Result<Value, Error>;

pub struct Field {
    pub readonly: bool,
    handler: Box<FieldHandler>,
}

impl Field {
    #[allow(dead_code)]
    pub fn new<F>(handler: F, readonly: bool) -> Self
    where
        F: Fn(&mut Gc, Value) -> Result<Value, Error> + 'static,
    {
        Field {
            readonly,
            handler: Box::new(handler),
        }
    }

    pub fn call(&self, gc: &mut Gc, this: Value) -> Result<Value, Error> {
        (self.handler)(gc, this)
    }
}

pub struct TypeDescr {
    ty: Type,
    methods: HashMap<String, Rc<Func>, WyHash>,
    fields: HashMap<&'static str, Field, WyHash>,
}

impl TypeDescr {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            methods: HashMap::default(),
            fields: HashMap::default(),
        }
    }

    fn builder(self) -> TypeDescrBuilder {
        TypeDescrBuilder { descr: self }
    }

    #[inline]
    pub fn field(&self, name: &str) -> Option<&Field> {
        self.fields.get(name)
    }

    #[inline]
    pub fn method(&self, name: &str) -> Option<&Rc<Func>> {
        self.methods.get(name)
    }
}

struct TypeDescrBuilder {
    descr: TypeDescr,
}

impl TypeDescrBuilder {
    #[allow(dead_code)]
    fn field(mut self, name: &'static str, field: Field) -> Self {
        self.descr.fields.insert(name, field);
        self
    }

    fn method(mut self, method: impl FnOnce() -> Func) -> Self {
        let method = method();
        self.descr.methods.insert(
            method.name.clone(),
            Rc::new(method.with_receiver(self.descr.ty)),
        );
        self
    }

    fn build(self) -> TypeDescr {
        self.descr
    }
}

pub type FnHandler = dyn Fn(&mut Gc, Vec<Value>) -> Result<Value, Error>;

pub struct StdLib {
    pub types: TypeRegistry,
    pub funcs: Vec<Rc<Func>>,
}

pub fn stdlib() -> StdLib {
    let mut types = TypeRegistry::default();
    types.insert(Type::Array, array::descr());

    let funcs = core::funcs()
        .into_iter()
        .map(|f| Rc::new(f()))
        .collect::<Vec<_>>();

    StdLib { types, funcs }
}
