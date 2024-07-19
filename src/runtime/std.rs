use std::{collections::HashMap, rc::Rc};

use wyhash2::WyHash;

use crate::gc::Gc;

use super::{
    error::Error,
    function::Func,
    value::{HeapValue, Type, Value},
};

pub fn repr(gc: &Gc, value: &Value) -> Result<String, Error> {
    let ty = value.ty();

    Ok(match value.ty() {
        Type::Str | Type::Array => match gc.get(value.handle()) {
            HeapValue::Buffer(buff) => match ty {
                Type::Str => format!("\"{}\"", String::from_utf8_lossy(buff)),
                _ => unreachable!(),
            },
            HeapValue::Array(items) => {
                let mut s = String::from("[");

                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&repr(gc, item)?);
                }

                s.push(']');
                s
            }
        },
        Type::Int => format!("{}", value.int()),
        Type::Float => format!("{}", value.float()),
        Type::Bool => format!("{}", value.bool()),
        Type::Fn => format!("{}(..)", value.func().name),
        Type::Nil => "".to_string(),
    })
}

pub struct StdLib {
    pub types: TypeRegistry,
    pub funcs: [Rc<Func>; 2],
}

pub fn stdlib() -> StdLib {
    let mut types = TypeRegistry::default();
    types.insert(Type::Array, array());
    let funcs = [
        Rc::new(Func::with_handler("repr".to_string(), 1, |gc, args| {
            let s = repr(gc, &args[0])?;
            let handle = gc.alloc(HeapValue::Buffer(s.into_bytes()));

            Ok(Value::new_str(handle))
        })),
        Rc::new(Func::with_handler(
            "println".to_string(),
            1,
            |heap, args| {
                let arg = args[0];

                match arg.ty() {
                    Type::Str => {
                        let handle = arg.handle();
                        let value = heap.get(handle);
                        let buffer = value.buffer();

                        println!("{}", String::from_utf8_lossy(buffer));
                    }
                    _ => println!("{}", repr(heap, &arg)?),
                }

                Ok(Value::NIL)
            },
        )),
    ];

    StdLib { types, funcs }
}

pub type TypeRegistry = HashMap<Type, TypeDescr, WyHash>;

type FieldHandler = dyn Fn(&mut Gc, Value) -> Result<Value, Error>;

pub struct Field {
    pub readonly: bool,
    handler: Box<FieldHandler>,
}

impl Field {
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

pub type FnHandler = dyn Fn(&mut Gc, Vec<Value>) -> Result<Value, Error>;

fn array() -> TypeDescr {
    TypeDescrBuilder::default()
        .field(
            "length",
            Field::new(
                |gc, value| {
                    let handle = value.handle();
                    let value = gc.get(handle);
                    let array = value.array();

                    Ok((array.len() as i64).into())
                },
                true,
            ),
        )
        .build()
}
