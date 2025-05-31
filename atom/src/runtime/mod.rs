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
fn map_str(
    gc: &mut Gc,
    handle: Handle<Str>,
    f: impl FnOnce(&str) -> String,
) -> Result<Handle<Str>, RuntimeError> {
    let rust_string = f(handle.as_str());
    let str = Str::from_string(gc, rust_string);
    gc.alloc(str)
}

#[derive(Default)]
pub struct Runtime {}

impl Runtime {
    #[atom_fn("println")]
    fn println(_gc: &mut Gc, arg: Value) -> Result<(), RuntimeError> {
        match arg.ty() {
            Type::Str => {
                println!("{}", arg.str().as_str());
            }
            _ => println!("{}", Self::repr(_gc, arg)?),
        }

        Ok(())
    }

    #[atom_fn("repr")]
    fn repr(_atom: &mut Gc, value: Value) -> Result<String, RuntimeError> {
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
    fn array_pop(_gc: &mut Gc, mut this: Handle<Array<Value>>) -> Result<Value, RuntimeError> {
        this.pop().map_or_else(
            || Err(ErrorKind::IndexOutOfBounds(0).at(Span::default())),
            Ok,
        )
    }

    #[atom_fn("Array.push")]
    fn array_push(
        gc: &mut Gc,
        mut this: Handle<Array<Value>>,
        item: Value,
    ) -> Result<(), RuntimeError> {
        this.push(gc, item)?;
        Ok(())
    }

    #[atom_fn("Array.len")]
    fn array_len(_gc: &mut Gc, this: Handle<Array<Value>>) -> Result<usize, RuntimeError> {
        Ok(this.len)
    }

    #[atom_fn("Array.cap")]
    fn array_cap(_gc: &mut Gc, this: Handle<Array<Value>>) -> Result<usize, RuntimeError> {
        Ok(this.cap)
    }

    #[atom_fn("Array.concat")]
    fn array_concat(
        gc: &mut Gc,
        this: Handle<Array<Value>>,
        other: Value,
    ) -> Result<Handle<Array<Value>>, RuntimeError> {
        let array = this.concat(gc, &other.array());
        gc.alloc(array)
    }

    #[atom_fn("Str.len")]
    fn str_len(_gc: &mut Gc, this: Handle<Str>) -> Result<usize, RuntimeError> {
        Ok(this.0.len())
    }

    #[atom_fn("Str.concat")]
    fn str_concat(gc: &mut Gc, this: Handle<Str>, other: Value) -> Result<Value, RuntimeError> {
        let array = this.0.concat(gc, &other.str().0);
        gc.alloc(Str(array)).map(Value::from)
    }

    #[atom_fn("Str.upper")]
    fn str_upper(gc: &mut Gc, this: Handle<Str>) -> Result<Handle<Str>, RuntimeError> {
        map_str(gc, this, |s| s.to_uppercase())
    }

    #[atom_fn("Str.lower")]
    fn str_lower(gc: &mut Gc, this: Handle<Str>) -> Result<Handle<Str>, RuntimeError> {
        map_str(gc, this, |s| s.to_lowercase())
    }
}

impl FFI for Runtime {
    fn call(
        &self,
        name: &str,
        gc: &mut Gc,
        recv: Option<Value>,
        args: Vec<Value>,
    ) -> Result<Value, vm::Error> {
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

        Ok((handler)(gc, recv, args)?)
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
            Runtime::array_push(&mut gc, handle.clone(), (10 * i).try_into().unwrap())
                .expect("Array.push failed");

            assert_eq!(handle.len() as i64, len);
            assert_eq!(handle.cap as i64, cap);
        }

        for (i, item) in handle.iter().copied().enumerate() {
            assert_eq!(Type::Int, item.ty());
            assert_eq!(10 * i, item.int() as usize);
        }
    }
}
