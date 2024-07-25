use atom_macros::atom_fn;

use crate::{
    gc::Gc,
    runtime::{
        error::RuntimeError,
        function::Func,
        value::{Type, Value},
    },
};

use super::{str::Str, Context};

#[atom_fn(repr)]
fn std_repr(ctx: Context<'_>, arg: Value) -> Result<Value, RuntimeError> {
    let string = repr(ctx.gc, &arg)?;
    let str = Str::from_string(ctx.gc, string);
    ctx.gc.alloc(str)
}

#[atom_fn(println)]
fn std_println(ctx: Context<'_>, arg: Value) -> Result<(), RuntimeError> {
    match arg.ty() {
        Type::Str => {
            let str = ctx.gc.get(arg.str());
            println!("{}", str.as_str());
        }
        _ => println!("{}", repr(ctx.gc, &arg)?),
    }

    Ok(())
}

pub fn funcs() -> [fn() -> Func; 2] {
    [std_println, std_repr]
}

pub fn repr(gc: &Gc, value: &Value) -> Result<String, RuntimeError> {
    Ok(match value.ty() {
        Type::Array => {
            let array = gc.get(value.array());
            let mut s = String::from("[");

            for (i, item) in array.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }

                s.push_str(&repr(gc, item)?);
            }

            s.push(']');
            s
        }
        Type::Str => {
            let str = gc.get(value.str());
            format!("\"{}\"", str.as_str())
        }
        Type::Int => format!("{}", value.int()),
        Type::Float => format!("{}", value.float()),
        Type::Bool => format!("{}", value.bool()),
        Type::Fn => format!("{}(..)", value.func().name),
        Type::Class => value.class().name.clone(),
        Type::Instance => format!("{}{{..}}", gc.get(value.instance()).class.name),
        Type::Nil => "<nil>".to_string(),
    })
}
