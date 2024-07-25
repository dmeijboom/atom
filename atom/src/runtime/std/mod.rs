use std::{collections::HashMap, rc::Rc};

use wyhash2::WyHash;

pub mod array;
pub mod core;
mod ffi;
pub mod str;

use super::{
    error::RuntimeError,
    function::{Func, Receiver},
    value::{Type, Value},
};

pub use ffi::{Context, Convert, FieldHandler, FnHandler};
pub type TypeRegistry = HashMap<Type, TypeDescr, WyHash>;

pub struct Field {
    handler: Box<FieldHandler>,
}

impl Field {
    #[allow(dead_code)]
    pub fn new<F>(handler: F) -> Self
    where
        F: Fn(Context<'_>, Value) -> Result<Value, RuntimeError> + 'static,
    {
        Field {
            handler: Box::new(handler),
        }
    }

    pub fn call(&self, ctx: Context, this: Value) -> Result<Value, RuntimeError> {
        (self.handler)(ctx, this)
    }
}

#[derive(Default)]
pub struct TypeDescr {
    methods: HashMap<String, Rc<Func>, WyHash>,
    fields: HashMap<&'static str, Field, WyHash>,
}

impl TypeDescr {
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
            Rc::new(method.with_receiver(Receiver::Type)),
        );
        self
    }

    fn build(self) -> TypeDescr {
        self.descr
    }
}

pub struct StdLib {
    pub types: TypeRegistry,
    pub funcs: Vec<Rc<Func>>,
}

pub fn stdlib() -> StdLib {
    let types = [(Type::Array, array::descr()), (Type::Str, str::descr())]
        .into_iter()
        .collect::<TypeRegistry>();

    let funcs = core::funcs()
        .into_iter()
        .map(|f| Rc::new(f()))
        .collect::<Vec<_>>();

    StdLib { types, funcs }
}
