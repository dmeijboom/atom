use crate::{
    error::IntoSpanned,
    gc::{Gc, Handle},
    lexer::Span,
    vm::{self, FatalErrorKind, Ffi},
};

use atom_macros::atom_fn;
use error::RuntimeError;
use str::Str;
use value::{IntoAtom as _, Type, Value};

pub mod array;
pub mod class;
pub mod error;
pub mod function;
pub mod str;
pub mod value;

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

#[derive(Default)]
pub struct Runtime {}

impl Runtime {
    #[atom_fn("isDarwin")]
    fn is_darwin() -> Result<bool, RuntimeError> {
        Ok(cfg!(target_os = "macos"))
    }

    #[atom_fn("typeOf")]
    fn type_of(value: Value) -> Result<String, RuntimeError> {
        Ok(value.ty().name().to_string())
    }

    #[atom_fn("syscall4")]
    fn syscall4(arg1: i32, arg2: i64, arg3: i64, arg4: i64) -> Result<i64, RuntimeError> {
        unsafe { Ok(libc::syscall(arg1, arg2, arg3, arg4) as i64) }
    }

    #[atom_fn("repr")]
    pub fn repr(value: Value) -> Result<String, RuntimeError> {
        Ok(match value.ty() {
            Type::Array => {
                let array = value.array();
                let mut s = String::from("[");

                for (i, item) in array.iter().copied().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }

                    s.push_str(&Self::repr(item)?);
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
            Type::Method => format!(".{}(..)", value.func().name),
            Type::Class => value.class().name.to_string(),
            Type::Object => format!("{}{{..}}", value.object().class.name),
            Type::Nil => "<nil>".to_string(),
        })
    }

    #[atom_fn("Array.pop")]
    fn array_pop<'gc>(mut a: Vec<Value<'gc>>) -> Result<Value<'gc>, RuntimeError> {
        Ok(a.pop().unwrap_or(Value::NIL))
    }

    #[atom_fn("Array.push")]
    fn array_push<'gc>(mut a: Vec<Value<'gc>>, item: Value<'gc>) -> Result<(), RuntimeError> {
        a.push(item);
        Ok(())
    }

    #[atom_fn("Array.len")]
    fn array_len<'gc>(a: Vec<Value<'gc>>) -> Result<usize, RuntimeError> {
        Ok(a.len())
    }

    #[atom_fn("Array.cap")]
    fn array_cap<'gc>(a: Vec<Value<'gc>>) -> Result<usize, RuntimeError> {
        Ok(a.capacity())
    }

    #[atom_fn("Array.concat")]
    fn array_concat<'gc>(
        lhs: Vec<Value<'gc>>,
        rhs: Vec<Value<'gc>>,
    ) -> Result<Vec<Value<'gc>>, RuntimeError> {
        Ok([lhs, rhs].concat())
    }

    #[atom_fn("Str.len")]
    fn str_len(s: String) -> Result<usize, RuntimeError> {
        Ok(s.len())
    }

    #[atom_fn("Str.concat")]
    fn str_concat(lhs: String, rhs: String) -> Result<String, RuntimeError> {
        Ok([lhs, rhs].concat())
    }

    #[atom_fn("Str.upper")]
    fn str_upper(s: String) -> Result<String, RuntimeError> {
        Ok(s.to_uppercase())
    }

    #[atom_fn("Str.lower")]
    fn str_lower(s: String) -> Result<String, RuntimeError> {
        Ok(s.to_lowercase())
    }

    #[atom_fn("Str.cptr")]
    fn str_cptr(s: Handle<Str>) -> Result<i64, RuntimeError> {
        // @TODO: what to do with an empty string?
        Ok(s.0.addr().unwrap_or(0) as i64)
    }
}

impl<'gc> Ffi<'gc> for Runtime {
    fn call(
        &mut self,
        name: &str,
        gc: &mut Gc<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>, vm::Error> {
        let handler = match_fn!(
            name,
            [
                repr,
                type_of,
                syscall4,
                is_darwin,
                array_pop,
                array_push,
                array_len,
                array_cap,
                array_concat,
                str_len,
                str_upper,
                str_lower,
                str_concat,
                str_cptr
            ]
        );

        Ok((handler)(gc, args)?)
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::array::Array;

    use super::*;

    #[test]
    fn array_push() {
        let mut gc = Gc::default();
        let array: Array<Value> = Array::default();
        let mut handle = gc.alloc(array).unwrap();

        assert_eq!(handle.len(), 0);

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
            let item = (10 * i).into_atom(&mut gc).unwrap();
            handle.push(&mut gc, item).expect("Array.push failed");

            assert_eq!(handle.len() as i64, len);
            assert_eq!(handle.cap as i64, cap);
        }

        for (i, item) in handle.iter().copied().enumerate() {
            assert_eq!(Type::Int, item.ty());
            assert_eq!(10 * i, item.int() as usize);
        }
    }
}
