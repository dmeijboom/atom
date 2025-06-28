use crate::{
    gc::Gc,
    runtime::{error::RuntimeError, value::Value},
};

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub struct Fn0<F>(pub F);
pub struct Fn1<F>(pub F);
pub struct Fn2<F>(pub F);
pub struct Fn3<F>(pub F);
pub struct Fn4<F>(pub F);

pub trait BuiltinFunction {
    fn call<'gc>(&mut self, gc: &mut Gc<'gc>, args: Vec<Value<'gc>>) -> Result<Value<'gc>>;
}

impl<F> BuiltinFunction for Fn0<F>
where
    F: for<'gc> FnMut(&mut Gc<'gc>) -> Result<Value<'gc>>,
{
    fn call<'gc>(&mut self, gc: &mut Gc<'gc>, _args: Vec<Value<'gc>>) -> Result<Value<'gc>> {
        (self.0)(gc)
    }
}

impl<F> BuiltinFunction for Fn1<F>
where
    F: for<'gc> FnMut(&mut Gc<'gc>, Value<'gc>) -> Result<Value<'gc>>,
{
    fn call<'gc>(&mut self, gc: &mut Gc<'gc>, mut args: Vec<Value<'gc>>) -> Result<Value<'gc>> {
        (self.0)(gc, args.remove(0))
    }
}

impl<F> BuiltinFunction for Fn2<F>
where
    F: for<'gc> FnMut(&mut Gc<'gc>, Value<'gc>, Value<'gc>) -> Result<Value<'gc>>,
{
    fn call<'gc>(&mut self, gc: &mut Gc<'gc>, mut args: Vec<Value<'gc>>) -> Result<Value<'gc>> {
        (self.0)(gc, args.remove(0), args.remove(0))
    }
}

impl<F> BuiltinFunction for Fn3<F>
where
    F: for<'gc> FnMut(&mut Gc<'gc>, Value<'gc>, Value<'gc>, Value<'gc>) -> Result<Value<'gc>>,
{
    fn call<'gc>(&mut self, gc: &mut Gc<'gc>, mut args: Vec<Value<'gc>>) -> Result<Value<'gc>> {
        (self.0)(gc, args.remove(0), args.remove(0), args.remove(0))
    }
}

impl<F> BuiltinFunction for Fn4<F>
where
    F: for<'gc> Fn(
        &mut Gc<'gc>,
        Value<'gc>,
        Value<'gc>,
        Value<'gc>,
        Value<'gc>,
    ) -> Result<Value<'gc>>,
{
    fn call<'gc>(&mut self, gc: &mut Gc<'gc>, mut args: Vec<Value<'gc>>) -> Result<Value<'gc>> {
        (self.0)(
            gc,
            args.remove(0),
            args.remove(0),
            args.remove(0),
            args.remove(0),
        )
    }
}
