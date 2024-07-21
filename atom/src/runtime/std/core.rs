use atom_macros::atom_fn;

use crate::{
    gc::Gc,
    runtime::{
        error::Error,
        function::Func,
        value::{Type, Value},
    },
};

#[atom_fn(repr)]
fn std_repr(gc: &mut Gc, arg: Value) -> Result<Value, Error> {
    let s = repr(gc, &arg)?;
    let handle = gc.alloc(s.into_bytes());
    Ok(Value::new_str(handle))
}

#[atom_fn(println)]
fn std_println(gc: &mut Gc, arg: Value) -> Result<(), Error> {
    match arg.ty() {
        Type::Str => {
            let buff = gc.get(arg.buffer());
            println!("{}", String::from_utf8_lossy(buff));
        }
        _ => println!("{}", repr(gc, &arg)?),
    }

    Ok(())
}

pub fn funcs() -> [fn() -> Func; 2] {
    [std_println, std_repr]
}

pub fn repr(gc: &Gc, value: &Value) -> Result<String, Error> {
    Ok(match value.ty() {
        Type::Array => {
            let array = gc.get(value.array());
            let mut s = String::from("[");

            for (i, item) in array.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }

                s.push_str(&repr(gc, &item)?);
            }

            s.push(']');
            s
        }
        Type::Str => {
            let buff = gc.get(value.buffer());
            format!("\"{}\"", String::from_utf8_lossy(buff))
        }
        Type::Int => format!("{}", value.int()),
        Type::Float => format!("{}", value.float()),
        Type::Bool => format!("{}", value.bool()),
        Type::Fn => format!("{}(..)", value.func().name),
        Type::Nil => "".to_string(),
    })
}
