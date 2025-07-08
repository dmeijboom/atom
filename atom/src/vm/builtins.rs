use std::{
    collections::HashMap,
    fmt::{self, Write}, ops::Deref,
};

use crate::{
    builtins::{BuiltinFunction, Fn0, Fn1, Fn2, Fn4},
    gc::Gc,
    runtime::{error::RuntimeError, value::Type, IntoAtom, Runtime, Value},
};

fn read_usize<'gc>(
    gc: &mut Gc<'gc>,
    _rt: &dyn Runtime,
    addr: Value<'gc>,
    offset: Value<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    unsafe {
        let ptr = addr.as_bigint().as_usize() as *mut u8;
        let ptr = ptr.byte_add(offset.as_bigint().as_usize()) as *const usize;
        std::ptr::read(ptr).into_atom(gc)
    }
}

fn str<'gc>(
    gc: &mut Gc<'gc>,
    _rt: &dyn Runtime,
    value: Value<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    (value.to_string()).into_atom(gc)
}

fn ptr<'gc>(
    gc: &mut Gc<'gc>,
    _rt: &dyn Runtime,
    value: Value<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    (value.as_raw_ptr() as usize).into_atom(gc)
}

fn array_push<'gc>(
    gc: &mut Gc<'gc>,
    _rt: &dyn Runtime,
    array: Value<'gc>,
    element: Value<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let mut array = array.as_array();
    array.push(gc, element)?;
    Ok(Value::default())
}

fn data_ptr<'gc>(
    gc: &mut Gc<'gc>,
    _rt: &dyn Runtime,
    value: Value<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    match value.ty() {
        Type::Array => value.as_array().deref().addr().unwrap_or(0).into_atom(gc),
        Type::Str => value.as_str().0.addr().unwrap_or(0).into_atom(gc),
        _ => unreachable!(),
    }
}

#[cfg(target_os = "macos")]
fn sys_code(code: Value) -> i32 {
    code.as_int() as i32
}

#[cfg(target_os = "linux")]
fn sys_code(code: Value) -> i64 {
    code.as_int()
}

fn syscall3<'gc>(
    gc: &mut Gc<'gc>,
    _rt: &dyn Runtime,
    code: Value<'gc>,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    unsafe { libc::syscall(sys_code(code), arg1.as_int(), arg2.as_int(), arg3.as_int()) }
        .into_atom(gc)
}

fn format_type(module: &str, name: &str) -> String {
    if module == "__root__" {
        name.to_string()
    } else {
        format!(
            "{}.{}",
            module.split('/').next_back().unwrap_or_default(),
            name
        )
    }
}

fn format_repr<'gc>(rt: &dyn Runtime, w: &mut impl Write, value: &Value<'gc>) -> fmt::Result {
    match value.ty() {
        Type::Array => {
            write!(w, "[")?;

            for (i, elem) in value.as_array().iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }

                format_repr(rt, w, elem)?;
            }

            write!(w, "]")
        }
        Type::Blob => {
            write!(w, "Blob[{}]", value.as_blob().len())
        }
        Type::Str => write!(w, "'{}'", value.as_str().as_str()),
        Type::Int => write!(w, "{}", value.as_int()),
        Type::BigInt => write!(w, "{}", value.as_bigint()),
        Type::Float => write!(w, "{}", value.as_float()),
        Type::Atom => write!(w, ":{}", rt.get_atom(value.as_atom())),
        Type::Resumable | Type::Fn => {
            let func = value.as_fn();
            let module = rt.get_module(func.context.module);

            if value.ty() == Type::Resumable {
                write!(w, "*")?;
            }

            write!(w, "{}(..)", format_type(&module.name, &func.name))
        }
        Type::Method => {
            let func = &value.as_method().func;
            let module = rt.get_module(func.context.module);

            write!(w, "{}(..)", format_type(&module.name, &func.name))
        }
        Type::Class => write!(w, "{}", value.as_class().name),
        Type::Object => {
            let object = value.as_object();
            let module = rt.get_module(object.class.context.module);

            write!(w, "{}{{..}}", format_type(&module.name, &object.class.name))
        }
    }
}

pub fn repr<'gc>(
    gc: &mut Gc<'gc>,
    rt: &dyn Runtime,
    value: Value<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let mut s = String::new();
    let _ = format_repr(rt, &mut s, &value);
    s.into_atom(gc)
}

fn is_darwin<'gc>(gc: &mut Gc<'gc>, _rt: &dyn Runtime) -> Result<Value<'gc>, RuntimeError> {
    cfg!(target_os = "macos").into_atom(gc)
}

fn is_arm64<'gc>(gc: &mut Gc<'gc>, _rt: &dyn Runtime) -> Result<Value<'gc>, RuntimeError> {
    cfg!(target_arch = "aarch64").into_atom(gc)
}

pub struct Builtins(pub HashMap<&'static str, Box<dyn BuiltinFunction>>);

impl Builtins {
    #[allow(dead_code)]
    pub fn register(&mut self, name: &'static str, f: Box<dyn BuiltinFunction>) {
        self.0.insert(name, f);
    }
}

impl Default for Builtins {
    fn default() -> Self {
        let mut builtins: HashMap<&'static str, Box<dyn BuiltinFunction>> = HashMap::default();
        builtins.insert("is_darwin", Box::new(Fn0(is_darwin)));
        builtins.insert("is_arm64", Box::new(Fn0(is_arm64)));
        builtins.insert("repr", Box::new(Fn1(repr)));
        builtins.insert("data_ptr", Box::new(Fn1(data_ptr)));
        builtins.insert("ptr", Box::new(Fn1(ptr)));
        builtins.insert("str", Box::new(Fn1(str)));
        builtins.insert("read_usize", Box::new(Fn2(read_usize)));
        builtins.insert("array_push", Box::new(Fn2(array_push)));
        builtins.insert("syscall3", Box::new(Fn4(syscall3)));

        Self(builtins)
    }
}
