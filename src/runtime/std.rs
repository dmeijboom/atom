use std::{collections::HashMap, rc::Rc};

use safe_gc::Heap;
use wyhash2::WyHash;

use super::{
    error::Error,
    function::Func,
    value::{Type, Value},
};

pub struct StdLib {
    pub types: TypeRegistry,
    pub funcs: [Rc<Func>; 1],
}

pub fn stdlib() -> StdLib {
    let mut types = TypeRegistry::default();
    types.insert(Type::Array, array());
    let funcs = [Rc::new(Func::with_handler(
        "println".to_string(),
        1,
        |_, args| {
            println!("{}", args[0]);
            Ok(Value::NIL)
        },
    ))];

    StdLib { types, funcs }
}

pub type TypeRegistry = HashMap<Type, TypeDescr, WyHash>;

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

pub type FnHandler = dyn Fn(&mut Heap, Vec<Value>) -> Result<Value, Error>;

//pub struct Func {
//    pub name: &'static str,
//    pub arg_count: usize,
//    pub handler: Rc<FnHandler>,
//}
//
//impl Func {
//    pub fn new<F>(name: &'static str, arg_count: usize, handler: F) -> Self
//    where
//        F: Fn(&mut Heap, TinyVec<[Value; 8]>) -> Result<Value, Error> + 'static,
//    {
//        Self {
//            name,
//            arg_count,
//            handler: Rc::new(handler),
//        }
//    }
//}

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
