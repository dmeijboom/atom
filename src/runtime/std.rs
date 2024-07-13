use std::collections::HashMap;

use safe_gc::Heap;

use super::{
    error::Error,
    value::{Type, Value},
};

type FieldHandler = dyn Fn(&mut Heap, Value) -> Result<Value, Error>;

pub struct Field {
    pub readonly: bool,
    handler: Box<FieldHandler>,
}

impl Field {
    pub fn new<F>(handler: F, readonly: bool) -> Self
    where
        F: Fn(&mut Heap, Value) -> Result<Value, Error> + 'static,
    {
        Field {
            readonly,
            handler: Box::new(handler),
        }
    }

    pub fn call(&self, heap: &mut Heap, this: Value) -> Result<Value, Error> {
        (self.handler)(heap, this)
    }
}

#[derive(Default)]
pub struct TypeDescr {
    fields: HashMap<&'static str, Field>,
}

impl TypeDescr {
    #[inline]
    pub fn field(&self, name: &str) -> Option<&Field> {
        self.fields.get(name)
    }
}

#[derive(Default)]
struct TypeDescrBuilder {
    descr: TypeDescr,
}

impl TypeDescrBuilder {
    fn field(mut self, name: &'static str, field: Field) -> Self {
        self.descr.fields.insert(name, field);
        self
    }

    fn build(self) -> TypeDescr {
        self.descr
    }
}

#[derive(Default)]
pub struct Registry {
    types: HashMap<Type, TypeDescr>,
}

impl Registry {
    #[inline]
    pub fn get(&self, ty: Type) -> Option<&TypeDescr> {
        self.types.get(&ty)
    }
}

pub fn registry() -> Registry {
    let mut registry = Registry::default();
    registry.types.insert(Type::Array, array());

    registry
}

fn array() -> TypeDescr {
    TypeDescrBuilder::default()
        .field(
            "length",
            Field::new(
                |gc, value| {
                    let handle = value.heap();
                    let value = gc.get(handle);
                    let array = value.array();

                    Ok((array.len() as i64).into())
                },
                true,
            ),
        )
        .build()
}
