use std::borrow::Cow;

use crate::{
    gc::Gc,
    runtime::{error::ErrorKind, Result, Runtime, Value},
};

pub struct Fn0<F>(pub F);
pub struct Fn1<F>(pub F);
pub struct Fn2<F>(pub F);
pub struct Fn3<F>(pub F);
pub struct Fn4<F>(pub F);

macro_rules! assert_argc {
    ($rt:expr, $args:expr, $argc:expr) => {
        if $args.len() != $argc {
            return Err(ErrorKind::ArgCountMismatch {
                func_name: Cow::Borrowed("<builtin>"),
                func_arg_count: $argc,
                arg_count: $args.len() as u32,
            }
            .at($rt.span())
            .into());
        }
    };
}

pub trait BuiltinFunction {
    fn call<'gc>(
        &mut self,
        gc: &mut Gc<'gc>,
        rt: &dyn Runtime,
        args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>>;
}

impl<F> BuiltinFunction for Fn0<F>
where
    F: for<'gc> FnMut(&mut Gc<'gc>, &dyn Runtime) -> Result<Value<'gc>>,
{
    fn call<'gc>(
        &mut self,
        gc: &mut Gc<'gc>,
        rt: &dyn Runtime,
        args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>> {
        assert_argc!(rt, args, 0);
        (self.0)(gc, rt)
    }
}

impl<F> BuiltinFunction for Fn1<F>
where
    F: for<'gc> FnMut(&mut Gc<'gc>, &dyn Runtime, Value<'gc>) -> Result<Value<'gc>>,
{
    fn call<'gc>(
        &mut self,
        gc: &mut Gc<'gc>,
        rt: &dyn Runtime,
        mut args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>> {
        assert_argc!(rt, args, 1);
        (self.0)(gc, rt, args.remove(0))
    }
}

impl<F> BuiltinFunction for Fn2<F>
where
    F: for<'gc> FnMut(&mut Gc<'gc>, &dyn Runtime, Value<'gc>, Value<'gc>) -> Result<Value<'gc>>,
{
    fn call<'gc>(
        &mut self,
        gc: &mut Gc<'gc>,
        rt: &dyn Runtime,
        mut args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>> {
        assert_argc!(rt, args, 2);
        (self.0)(gc, rt, args.remove(0), args.remove(0))
    }
}

impl<F> BuiltinFunction for Fn3<F>
where
    F: for<'gc> FnMut(
        &mut Gc<'gc>,
        &dyn Runtime,
        Value<'gc>,
        Value<'gc>,
        Value<'gc>,
    ) -> Result<Value<'gc>>,
{
    fn call<'gc>(
        &mut self,
        gc: &mut Gc<'gc>,
        rt: &dyn Runtime,
        mut args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>> {
        assert_argc!(rt, args, 3);
        (self.0)(gc, rt, args.remove(0), args.remove(0), args.remove(0))
    }
}

impl<F> BuiltinFunction for Fn4<F>
where
    F: for<'gc> FnMut(
        &mut Gc<'gc>,
        &dyn Runtime,
        Value<'gc>,
        Value<'gc>,
        Value<'gc>,
        Value<'gc>,
    ) -> Result<Value<'gc>>,
{
    fn call<'gc>(
        &mut self,
        gc: &mut Gc<'gc>,
        rt: &dyn Runtime,
        mut args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>> {
        assert_argc!(rt, args, 4);
        (self.0)(
            gc,
            rt,
            args.remove(0),
            args.remove(0),
            args.remove(0),
            args.remove(0),
        )
    }
}
