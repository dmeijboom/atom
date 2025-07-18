use std::{
    collections::HashMap,
    fmt::{self, Write},
};

use crate::runtime::{
    errors::RuntimeError, Array, Blob, BuiltinFunction, Fn0, Fn1, Fn2, Fn3, Fn4, Gc, IntoAtom,
    Runtime, Type, Value,
};

macro_rules! builtins {
    ($($name:ident ( $gc:ident, $rt:ident $(, $arg_name:ident)* ) $body:block),+) => {
        $(
            pub fn $name<'gc>($gc: &mut Gc<'gc>, $rt: &dyn Runtime, $($arg_name: Value<'gc>),*) -> Result<Value<'gc>, RuntimeError> {
                $body.into_atom($gc)
            }
        )+
    };
}

pub(crate) use builtins;

fn read<'gc, T>(addr: Value<'gc>, offset: Value<'gc>) -> T {
    unsafe {
        let ptr = addr.as_bigint().as_usize() as *mut u8;
        let ptr = ptr.byte_add(offset.as_bigint().as_usize()) as *const T;
        std::ptr::read(ptr)
    }
}

fn write<'gc, T>(addr: Value<'gc>, offset: Value<'gc>, data: T) {
    unsafe {
        let ptr = addr.as_bigint().as_usize() as *mut u8;
        let ptr = ptr.add(offset.as_bigint().as_usize()) as *mut T;
        std::ptr::write(ptr, data);
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

#[cfg(target_os = "macos")]
mod mac {
    use super::*;

    builtins!(
        mach_timebase_info(gc, _rt, timebase_info) {
            unsafe {
                let addr = timebase_info.as_bigint().as_usize();
                let timebase_info = addr as *mut mach2::mach_time::mach_timebase_info;

                mach2::mach_time::mach_timebase_info(timebase_info);
            }
        },

        mach_wait_until(_gc, _rt, deadline) {
            unsafe { mach2::mach_time::mach_wait_until(deadline.as_bigint().as_u64()) }
        },

        mach_absolute_time(_gc, _rt) {
            unsafe { mach2::mach_time::mach_absolute_time() }
        }
    );
}

pub struct Builtins(pub HashMap<&'static str, Box<dyn BuiltinFunction>>);

impl Builtins {
    #[allow(dead_code)]
    pub fn register(&mut self, name: &'static str, f: Box<dyn BuiltinFunction>) {
        self.0.insert(name, f);
    }
}

builtins!(
    is_darwin(gc, _rt) {
        cfg!(target_os = "macos")
    },

    is_arm64(gc, _rt) {
        cfg!(target_arch = "aarch64")
    },

    repr(gc, rt, value) {
        let mut s = String::new();
        let _ = format_repr(rt, &mut s, &value);
        s
    },

    syscall3(gc, _rt, code, arg1, arg2, arg3) {
        unsafe { libc::syscall(sys_code(code), arg1.as_int(), arg2.as_int(), arg3.as_int()) }
    },

    array_push(gc, _rt, array, elem) {
        let mut array = array.as_array();
        array.push(gc, elem)?;
    },

    ptr(gc, _rt, value) {
       value.as_raw_ptr() as usize
    },

    str(gc, _rt, value) {
        value.to_string()
    },

    blob(gc, _rt, size) {
        let mut array = Array::with_capacity(gc, size.as_bigint().as_usize())?;

        // Since `with_capacity` zeroes the array, we can safely set the length
        array.len = array.cap;

        gc.alloc(Blob(array))?
    },

    write_usize(gc, _rt, addr, offset, data) {
        write::<usize>(addr, offset, data.as_bigint().as_usize())
    },

    read_usize(gc, _rt, addr, offset) {
        read::<usize>(addr, offset)
    },

    read_u32(gc, _rt, addr, offset) {
        read::<u32>(addr, offset)
    }
);

impl Default for Builtins {
    fn default() -> Self {
        let mut builtins: HashMap<&'static str, Box<dyn BuiltinFunction>> = HashMap::default();
        builtins.insert("is_darwin", Box::new(Fn0(is_darwin)));
        builtins.insert("is_arm64", Box::new(Fn0(is_arm64)));
        builtins.insert("repr", Box::new(Fn1(repr)));
        builtins.insert("blob", Box::new(Fn1(blob)));
        builtins.insert("ptr", Box::new(Fn1(ptr)));
        builtins.insert("str", Box::new(Fn1(str)));
        builtins.insert("read_usize", Box::new(Fn2(read_usize)));
        builtins.insert("read_u32", Box::new(Fn2(read_u32)));
        builtins.insert("write_usize", Box::new(Fn3(write_usize)));
        builtins.insert("array_push", Box::new(Fn2(array_push)));
        builtins.insert("syscall3", Box::new(Fn4(syscall3)));

        #[cfg(target_os = "macos")]
        {
            builtins.insert("mach_wait_until", Box::new(Fn1(mac::mach_wait_until)));
            builtins.insert("mach_timebase_info", Box::new(Fn1(mac::mach_timebase_info)));
            builtins.insert("mach_absolute_time", Box::new(Fn0(mac::mach_absolute_time)));
        }

        Self(builtins)
    }
}
