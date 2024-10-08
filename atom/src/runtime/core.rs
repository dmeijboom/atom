use atom_macros::export;

use crate::{
    gc::Handle,
    runtime::{
        error::RuntimeError,
        value::{Type, Value},
    },
};

use super::{str::Str, Atom};

#[export]
fn repr(mut atom: Atom<'_>, arg: Value) -> Result<Handle<Str>, RuntimeError> {
    let string = repr(&arg)?;
    let str = Str::from_string(atom.gc, string);
    atom.alloc(str)
}

#[export]
fn println(_atom: Atom<'_>, arg: Value) -> Result<(), RuntimeError> {
    match arg.ty() {
        Type::Str => {
            println!("{}", arg.str().as_str());
        }
        _ => println!("{}", repr(&arg)?),
    }

    Ok(())
}

pub fn repr(value: &Value) -> Result<String, RuntimeError> {
    Ok(match value.ty() {
        Type::Array => {
            let array = value.array();
            let mut s = String::from("[");

            for (i, item) in array.iter().enumerate() {
                if i > 0 {
                    s.push_str(", ");
                }

                s.push_str(&repr(item)?);
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
