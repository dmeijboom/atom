use std::borrow::Cow;

use bytes::Bytes;

use crate::{
    error::IntoSpanned,
    gc::{Gc, Handle},
    lexer::Span,
    opcode::Const,
    vm::{self, FatalErrorKind, FFI},
};

use array::Array;
use atom_macros::atom_fn;
use class::Class;
use error::{ErrorKind, RuntimeError};
use str::Str;
use value::{TryIntoValue as _, Type, Value};

pub mod array;
pub mod class;
pub mod error;
pub mod function;
pub mod str;
pub mod value;

pub type Name = Cow<'static, str>;

#[derive(Debug, Default)]
pub struct Module {
    pub body: Bytes,
    pub consts: Vec<Const>,
    pub functions: Vec<function::Fn>,
    pub classes: Vec<Class>,
}

pub struct Api<'a> {
    gc: &'a mut Gc,
    span: Span,
    receiver: Option<Value>,
}

impl<'a> Api<'a> {
    pub fn new(gc: &'a mut Gc) -> Self {
        Self {
            gc,
            span: Span::default(),
            receiver: None,
        }
    }

    pub fn gc(&mut self) -> &mut Gc {
        self.gc
    }

    pub fn receiver(&self) -> Result<Value, RuntimeError> {
        self.receiver
            .ok_or_else(|| ErrorKind::NoReceiver.at(self.span))
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_receiver(mut self, receiver: Value) -> Self {
        self.receiver = Some(receiver);
        self
    }
}

macro_rules! match_fn {
    ($fn:ident, [$($name:ident),+]) => {
        pastey::paste!{
            match $fn {
                $(Self::[<_atom_ $name _name>] => Self::[<_atom_ $name>]),+,
                _ => {
                    return Err(vm::Error::Fatal(
                        FatalErrorKind::InvalidExternFn($fn.to_string()).at(Span::default()),
                    ))
                }
            }
        }
    };
}

#[inline]
fn map_str(api: &mut Api<'_>, f: impl FnOnce(&str) -> String) -> Result<Handle<Str>, RuntimeError> {
    let handle = api.receiver()?.str();
    let rust_string = f(handle.as_str());
    let str = Str::from_string(api.gc, rust_string);
    api.gc().alloc(str)
}

#[derive(Default)]
pub struct Runtime {}

impl Runtime {
    #[atom_fn("println")]
    fn println(_atom: &mut Api<'_>, arg: Value) -> Result<(), RuntimeError> {
        match arg.ty() {
            Type::Str => {
                println!("{}", arg.str().as_str());
            }
            _ => println!("{}", Self::repr(_atom, arg)?),
        }

        Ok(())
    }

    #[atom_fn("repr")]
    fn repr(_atom: &mut Api<'_>, value: Value) -> Result<String, RuntimeError> {
        Ok(match value.ty() {
            Type::Array => {
                let array = value.array();
                let mut s = String::from("[");

                for (i, item) in array.iter().copied().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&Self::repr(_atom, item)?);
                }

                s.push(']');
                s
            }
            Type::Str => {
                format!("\"{}\"", value.str().as_str())
            }
            Type::Int => format!("{}", value.int()),
            Type::Float => format!("{}", value.float()),
            Type::Bool => format!("{}", value.bool()),
            Type::Fn => format!("{}(..)", value.func().name),
            Type::Class => value.class().name.to_string(),
            Type::Object => format!("{}{{..}}", value.object().class.name),
            Type::Nil => "<nil>".to_string(),
        })
    }

    #[atom_fn("Array.pop")]
    fn array_pop(api: &mut Api<'_>) -> Result<Value, RuntimeError> {
        let mut array = api.receiver()?.array();
        array
            .pop()
            .map_or_else(|| Err(ErrorKind::IndexOutOfBounds(0).at(api.span)), Ok)
    }

    #[atom_fn("Array.push")]
    fn array_push(api: &mut Api<'_>, item: Value) -> Result<(), RuntimeError> {
        let mut array = api.receiver()?.array();
        array.push(api.gc(), item)?;

        Ok(())
    }

    #[atom_fn("Array.len")]
    fn array_len(api: &mut Api<'_>) -> Result<usize, RuntimeError> {
        Ok(api.receiver()?.array().len)
    }

    #[atom_fn("Array.cap")]
    fn array_cap(api: &mut Api<'_>) -> Result<usize, RuntimeError> {
        Ok(api.receiver()?.array().cap)
    }

    #[atom_fn("Array.concat")]
    fn array_concat(api: &mut Api<'_>, other: Value) -> Result<Handle<Array<Value>>, RuntimeError> {
        let handle = api.receiver()?.array();
        let array = handle.concat(api.gc(), &other.array());
        api.gc().alloc(array)
    }

    #[atom_fn("Str.len")]
    fn str_len(_api: &mut Api<'_>, s: Handle<Str>) -> Result<usize, RuntimeError> {
        Ok(s.0.len())
    }

    #[atom_fn("Str.concat")]
    fn str_concat(api: &mut Api<'_>, other: Value) -> Result<Value, RuntimeError> {
        let handle = api.receiver()?.str();
        let array = handle.0.concat(api.gc(), &other.str().0);
        api.gc().alloc(Str(array)).map(Value::from)
    }

    #[atom_fn("Str.upper")]
    fn str_upper(api: &mut Api<'_>) -> Result<Handle<Str>, RuntimeError> {
        map_str(api, |s| s.to_uppercase())
    }

    #[atom_fn("Str.lower")]
    fn str_lower(api: &mut Api<'_>) -> Result<Handle<Str>, RuntimeError> {
        map_str(api, |s| s.to_lowercase())
    }
}

impl FFI for Runtime {
    fn call(&self, name: &str, api: Api, args: Vec<Value>) -> Result<Value, vm::Error> {
        let handler = match_fn!(
            name,
            [
                println,
                repr,
                array_pop,
                array_push,
                array_len,
                array_cap,
                array_concat,
                str_len,
                str_upper,
                str_lower,
                str_concat
            ]
        );

        Ok((handler)(api, args)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn array_push() {
        let mut gc = Gc::default();
        let array: Array<Value> = Array::default();

        assert_eq!(array.len(), 0);

        let handle = gc.alloc(array).unwrap();
        let expected = [
            (1, 1),
            (2, 2),
            (3, 4),
            (4, 4),
            (5, 8),
            (6, 8),
            (7, 8),
            (8, 8),
            (9, 16),
        ];

        for (i, (len, cap)) in expected.into_iter().enumerate() {
            let mut api = Api::new(&mut gc).with_receiver(handle.clone().into());
            Runtime::array_push(&mut api, (10 * i).try_into().unwrap()).expect("Array.push failed");

            assert_eq!(handle.len() as i64, len);
            assert_eq!(handle.cap as i64, cap);
        }

        for (i, item) in handle.iter().copied().enumerate() {
            assert_eq!(Type::Int, item.ty());
            assert_eq!(10 * i, item.int() as usize);
        }
    }
}
