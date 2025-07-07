use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{self, Write},
    fs, mem,
    ops::*,
    path::{Path, PathBuf},
};

use wyhash2::WyHash;

use crate::{
    ast::Stmt,
    builtins::{BuiltinFunction, Fn0, Fn1, Fn2, Fn4},
    bytecode::{self, Op},
    collections::Stack,
    compiler::{Compiler, GlobalContext, Package},
    frame::Frame,
    gc::{Gc, Handle, Trace},
    lexer::{Lexer, Span},
    module::{Metadata, Module},
    parser::{self},
    runtime::{
        error::{Call, ErrorKind, RuntimeError},
        function::{Resumable, ResumableState},
        value::{self, IntoAtom, OneOf, Tag, Type, TypeAssert, Value},
        Array, BigInt, Class, Context, Fn, Method, Object, Runtime, Str,
    },
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("ImportError: {0}")]
    Import(#[from] Box<crate::error::Error>),
    #[error("RuntimeError: {0}")]
    Runtime(#[from] RuntimeError),
}

pub fn parse(source: &str) -> Result<Vec<Stmt>, crate::error::Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;
    let parser = parser::Parser::new(tokens);

    Ok(parser.parse()?)
}

pub fn compile(
    source: impl AsRef<Path>,
    ctx: &mut GlobalContext,
) -> Result<Package, crate::error::Error> {
    let source = fs::read_to_string(source)?;
    let program = parse(&source)?;
    let compiler = Compiler::default();

    Ok(compiler.compile(ctx, program)?)
}

macro_rules! impl_binary {
    (final $fn:ident: $op:tt $($float:pat)?) => {
        fn $fn(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
            let (lhs, rhs) = self.stack.operands();

            if lhs.is_int() && rhs.is_int() {
                self.stack.push(lhs.as_int().$fn(&rhs.as_int()).into_atom(gc)?);
                return Ok(());
            }

            self.stack.push(match (lhs.tag(), rhs.tag()) {
                (Tag::BigInt, Tag::BigInt) | (Tag::Int, Tag::BigInt) | (Tag::BigInt, Tag::Int) => {
                    lhs.as_bigint().$fn(&rhs.as_bigint()).into_atom(gc)?
                }
                $($float => lhs.as_float().$fn(&rhs.as_float()).into_atom(gc)?,)?
                _ => return Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty())),
            });

            Ok(())
        }
    };

    ($fn:ident: &) => { impl_binary!(final $fn: &); };
    ($fn:ident: |) => { impl_binary!(final $fn: |); };
    ($fn:ident: <<) => { impl_binary!(final $fn: <<); };
    ($fn:ident: >>) => { impl_binary!(final $fn: >>); };
    ($fn:ident: $op:tt) => { impl_binary!(final $fn: $op (Tag::Float, Tag::Float)); };

    ($(($fn:ident: $op:tt)),+) => {
        impl<'gc, const S: usize> Vm<'gc, S> {
            $(impl_binary!($fn: $op);)+
        }
    };
}

pub struct Vm<'gc, const S: usize> {
    std: Context,
    frame: Frame<'gc>,
    search_path: PathBuf,
    modules: Vec<Module<'gc>>,
    stack: Stack<Value<'gc>, S>,
    call_stack: Vec<Frame<'gc>>,
    global_context: GlobalContext,
    package: Handle<'gc, Class<'gc>>,
    #[cfg(feature = "profiler")]
    profiler: crate::profiler::VmProfiler,
    packages: HashMap<String, Handle<'gc, Object<'gc>>, WyHash>,
}

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

fn syscall4<'gc>(
    gc: &mut Gc<'gc>,
    _rt: &dyn Runtime,
    arg1: Value<'gc>,
    arg2: Value<'gc>,
    arg3: Value<'gc>,
    arg4: Value<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    unsafe {
        libc::syscall(
            arg1.as_int() as i32,
            arg2.as_int(),
            arg3.as_int(),
            arg4.as_int(),
        )
    }
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

impl<'gc, const S: usize> Runtime<'gc> for Vm<'gc, S> {
    fn get_atom(&self, idx: u32) -> &str {
        self.global_context.atoms[idx as usize].as_str()
    }

    fn get_module(&self, idx: usize) -> &Metadata {
        self.modules[idx].metadata()
    }

    fn span(&self) -> Span {
        self.span()
    }
}

pub struct Builtins(HashMap<&'static str, Box<dyn BuiltinFunction>>);

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
        builtins.insert("repr", Box::new(Fn1(repr)));
        builtins.insert("data_ptr", Box::new(Fn1(data_ptr)));
        builtins.insert("ptr", Box::new(Fn1(ptr)));
        builtins.insert("str", Box::new(Fn1(str)));
        builtins.insert("read_usize", Box::new(Fn2(read_usize)));
        builtins.insert("array_push", Box::new(Fn2(array_push)));
        builtins.insert("syscall4", Box::new(Fn4(syscall4)));

        Self(builtins)
    }
}

fn array_idx(elem: Value, len: usize) -> usize {
    let i = elem.as_bigint();

    match i.as_i64() {
        n if n < 0 => (len as i64 + n) as usize,
        _ => i.as_usize(),
    }
}

impl<'gc, const S: usize> Vm<'gc, S> {
    pub fn new(
        gc: &mut Gc<'gc>,
        global_context: GlobalContext,
        search_path: PathBuf,
        mut package: Package,
    ) -> Result<Self, Error> {
        let frame = Frame::from_package(gc, Context::with_module(0), &mut package)?;
        let module = Module::new(0, Metadata::default(), package);
        let package = gc.alloc(Class::new(Cow::Borrowed("Package"), 1))?;

        Ok(Self {
            frame,
            package,
            search_path,
            global_context,
            std: Context::default(),
            modules: vec![module],
            call_stack: vec![],
            stack: Stack::default(),
            packages: HashMap::default(),
            #[cfg(feature = "profiler")]
            profiler: crate::profiler::VmProfiler::default(),
        })
    }

    fn span(&self) -> Span {
        self.modules[self.frame.context().module].span_at(self.frame.offset)
    }

    fn unsupported(&self, op: &'static str, left: Type, right: Type) -> Error {
        ErrorKind::UnsupportedOp { left, right, op }
            .at(self.span())
            .into()
    }

    fn index_out_of_bounds(&self, idx: usize) -> Error {
        ErrorKind::IndexOutOfBounds(idx).at(self.span()).into()
    }

    fn arg_count_mismatch(&self, func: &Fn, arg_count: u32) -> Error {
        ErrorKind::ArgCountMismatch {
            func_name: func.name.clone(),
            func_arg_count: func.arg_count,
            arg_count,
        }
        .at(self.span())
        .into()
    }

    fn gc_tick(&mut self, gc: &mut Gc) {
        if !gc.ready {
            return;
        }

        self.mark_sweep(gc);
    }

    fn is_package(&self, class: &Class<'gc>) -> bool {
        class.name == "Package" && class.context.module == self.std.module
    }

    fn module(&mut self) -> &mut Module<'gc> {
        &mut self.modules[self.frame.context().module]
    }

    fn mark_sweep(&mut self, gc: &mut Gc) {
        self.frame.trace(gc);
        self.package.trace(gc);
        self.packages.values().for_each(|handle| gc.mark(handle));
        self.call_stack.iter().for_each(|frame| frame.trace(gc));
        self.modules.iter().for_each(|module| module.trace(gc));
        self.stack.iter().for_each(|value| value.trace(gc));

        gc.sweep();
    }

    #[inline]
    fn jump(&mut self, pos: u32) {
        self.frame.offset = pos as usize * bytecode::SIZE;
    }

    fn load_member(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let recv = self.stack.pop();

        if recv.is_object() {
            return self.load_attr(gc, recv, idx);
        }

        let member = &self.global_context.atoms[idx as usize];

        if let Some(mut class) =
            self.modules[self.std.module].load_class_by_name(gc, recv.ty().name())?
        {
            if let Some(func) = class.load_method(gc, member)? {
                self.stack.push(gc.alloc(Method::new(recv, func))?);
                return Ok(());
            }
        }

        Err(ErrorKind::UnknownField {
            ty: recv.ty(),
            field: member.to_string(),
        }
        .at(self.span())
        .into())
    }

    fn store_member(&mut self, member: u32) -> Result<(), Error> {
        let value = self.stack.pop();
        let object = self.stack.pop();

        if !object.is_object() {
            self.assert_type(object.ty(), Type::Object)?;
        }

        object.as_object().set_attr(member, value);

        Ok(())
    }

    fn load_export(
        &mut self,
        gc: &mut Gc<'gc>,
        object: Handle<'gc, Object<'gc>>,
        member: u32,
    ) -> Result<(), Error> {
        let member = &self.global_context.atoms[member as usize];
        let module_id = object
            .get_attr(value::MODULE)
            .ok_or_else(|| ErrorKind::MissingAttribute("module").at(self.span()))?
            .as_int();
        let module = &mut self.modules[module_id as usize];

        if let Some(handle) = module.load_fn_by_name(gc, member)? {
            self.stack.push(handle);
            return Ok(());
        }

        if let Some(handle) = module.load_class_by_name(gc, member)? {
            self.stack.push(handle);
            return Ok(());
        }

        Err(ErrorKind::UnknownAttr {
            class_name: object.class.name.clone(),
            attribute: member.to_string(),
        }
        .at(self.span())
        .into())
    }

    fn load_attr(
        &mut self,
        gc: &mut Gc<'gc>,
        object: Value<'gc>,
        member: u32,
    ) -> Result<(), Error> {
        let object = object.as_object();

        if let Some(value) = object.get_attr(member) {
            self.stack.push(value);
            return Ok(());
        }

        if self.is_package(&object.class) {
            return self.load_export(gc, object, member);
        }

        self.load_method(gc, object, member)
    }

    fn load_method(
        &mut self,
        gc: &mut Gc<'gc>,
        mut object: Handle<'gc, Object<'gc>>,
        member: u32,
    ) -> Result<(), Error> {
        let name = &self.global_context.atoms[member as usize];
        let func = object.class.load_method(gc, name)?.ok_or_else(|| {
            ErrorKind::UnknownAttr {
                class_name: object.class.name.clone(),
                attribute: name.to_string(),
            }
            .at(self.span())
        })?;

        let method = Method::new(Handle::clone(&object).into(), func);
        let handle = gc.alloc(method)?;

        object.set_attr(member, Handle::clone(&handle).into());
        self.stack.push(handle);

        Ok(())
    }

    fn load_class(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let class = self.module().load_class(gc, idx as usize)?;
        self.stack.push(class);
        Ok(())
    }

    fn load_fn(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let f = self.module().load_fn(gc, idx as usize)?;
        self.stack.push(f);
        Ok(())
    }

    fn return_local(&mut self, idx: u32) -> Result<(), Error> {
        self.load_local(idx);
        self.ret(false)
    }

    fn load_local(&mut self, idx: u32) {
        self.stack
            .push(Value::clone(&self.frame.locals[idx as usize]));
    }

    fn store_local(&mut self, idx: u32) {
        self.frame.locals[idx as usize] = self.stack.pop();
    }

    fn assert_type(&self, left: Type, right: impl Into<TypeAssert>) -> Result<(), Error> {
        let right = right.into();

        if right != left {
            return Err(ErrorKind::TypeMismatch { left, right }
                .at(self.span())
                .into());
        }

        Ok(())
    }

    fn jump_if_false(&mut self, idx: u32) -> Result<(), Error> {
        let value = self.stack.pop();

        if value.as_atom() == value::FALSE {
            self.jump(idx);
            return Ok(());
        }

        Ok(())
    }

    fn jump_cond(&mut self, idx: u32, cond: u32) -> Result<(), Error> {
        let value = self.stack.pop();

        if value.as_atom() == cond {
            self.stack.push(cond);
            self.jump(idx);
        }

        Ok(())
    }

    fn init_class(
        &mut self,
        gc: &mut Gc<'gc>,
        class: Handle<'gc, Class<'gc>>,
        arg_count: u32,
    ) -> Result<(), Error> {
        let init = class.init.clone();
        let handle = gc.alloc(Object::new(class))?;

        if let Some(f) = init {
            self.stack.push(gc.alloc(Method::new(handle.into(), f))?);
            self.call(gc, arg_count)?;
        } else {
            self.stack.push(handle);
        }

        self.gc_tick(gc);

        Ok(())
    }

    fn call_builtin(
        &mut self,
        gc: &mut Gc<'gc>,
        Builtins(builtins): &mut Builtins,
        (idx, arg_count): (u16, u16),
    ) -> Result<(), Error> {
        let name = self.module().load_const(gc, idx as usize)?.as_str();
        let f = builtins.get_mut(name.as_str()).ok_or_else(|| {
            ErrorKind::UnknownBuiltin {
                name: name.to_string(),
            }
            .at(self.span())
        })?;

        let args = self.stack.slice_to_end(arg_count as usize).to_vec();
        let return_value = f.call(gc, self, args)?;

        self.stack.push(return_value);
        self.gc_tick(gc);

        Ok(())
    }

    fn call_fn(&mut self, gc: &mut Gc<'gc>, (idx, arg_count): (u16, u16)) -> Result<(), Error> {
        let f = self.module().load_fn(gc, idx as usize)?;
        self.fn_call(gc, f, arg_count as u32)
    }

    fn call(&mut self, gc: &mut Gc<'gc>, arg_count: u32) -> Result<(), Error> {
        let callee = self.stack.pop();

        match callee.tag() {
            Tag::Fn => self.fn_call(gc, callee.as_fn(), arg_count),
            Tag::Resumable => self.resumable_call(gc, callee.as_resumable(), arg_count),
            Tag::Method => self.method_call(gc, callee.as_method(), arg_count),
            Tag::Class => self.init_class(gc, callee.as_class(), arg_count),
            _ => Err(ErrorKind::NotCallable(callee.ty()).at(self.span()).into()),
        }
    }

    fn resumable_call(
        &mut self,
        gc: &mut Gc<'gc>,
        mut resumable: Handle<'gc, Resumable<'gc>>,
        arg_count: u32,
    ) -> Result<(), Error> {
        if arg_count != 0 {
            return Err(self.arg_count_mismatch(&resumable.func, arg_count));
        }

        match resumable.state {
            ResumableState::Idle | ResumableState::Running => {
                resumable.state = ResumableState::Running;

                let mut frame = Frame::new(Handle::clone(&resumable.func));
                let mut locals = resumable.locals.clone();
                locals.push(Handle::clone(&resumable).into());

                frame.resumable = true;
                frame.offset = resumable.offset;
                frame.locals = locals;

                self.set_frame(frame);
                self.gc_tick(gc);
            }
            ResumableState::Completed => {
                self.stack.push(Value::new(Tag::Atom, value::NIL as u64));
            }
        };

        Ok(())
    }

    fn method_call(
        &mut self,
        gc: &mut Gc,
        method: Handle<'gc, Method<'gc>>,
        arg_count: u32,
    ) -> Result<(), Error> {
        if arg_count != method.func.arg_count - 1 {
            return Err(self.arg_count_mismatch(&method.func, arg_count));
        }

        self.set_frame(Frame::new(Handle::clone(&method.func)));

        let locals = self.stack.slice_to_end(arg_count as usize);

        self.frame.locals = Vec::with_capacity(locals.len() + 1);
        self.frame.locals.push(Value::clone(&method.recv));
        self.frame.locals.extend_from_slice(locals);
        self.gc_tick(gc);

        Ok(())
    }

    fn fn_call(
        &mut self,
        gc: &mut Gc<'gc>,
        func: Handle<'gc, Fn>,
        arg_count: u32,
    ) -> Result<(), Error> {
        if arg_count != func.arg_count {
            return Err(self.arg_count_mismatch(&func, arg_count));
        }

        let locals = self.stack.slice_to_end(arg_count as usize).to_vec();

        if func.resumable {
            self.stack.push(gc.alloc(Resumable::new(func, locals))?);
            return Ok(());
        }

        self.set_frame(Frame::new(func));
        self.frame.locals = locals;
        self.gc_tick(gc);

        Ok(())
    }

    fn tail_call(&mut self, gc: &mut Gc, arg_count: u32) -> Result<(), Error> {
        self.frame.handle = self.stack.pop().as_fn();
        self.frame.offset = 0;
        self.frame.returned = false;
        self.frame.locals = self.stack.slice_to_end(arg_count as usize).to_vec();
        self.gc_tick(gc);

        Ok(())
    }

    fn make_array(&mut self, gc: &mut Gc<'gc>, size: u32) -> Result<(), Error> {
        let array = self.stack.slice_to_end(size as usize);
        let array = Array::copy_from_slice(gc, array)?;

        self.stack.push(gc.alloc(array)?);
        self.gc_tick(gc);

        Ok(())
    }

    fn make_slice(&mut self, gc: &mut Gc<'gc>, opts: u32) -> Result<(), Error> {
        let (from, to) = match opts {
            0 => (None, None),
            1 => (Some(self.stack.pop()), None),
            2 => (None, Some(self.stack.pop())),
            3 => {
                let to = self.stack.pop();
                let from = self.stack.pop();
                (Some(from), Some(to))
            }
            _ => unreachable!(),
        };

        if let Some(from) = from.as_ref() {
            self.assert_type(from.ty(), OneOf(Type::Int, Type::BigInt))?;
        }

        if let Some(to) = to.as_ref() {
            self.assert_type(to.ty(), OneOf(Type::Int, Type::BigInt))?;
        }

        let array = self.stack.pop();

        match array.ty() {
            Type::Array => {
                let array = array.as_array();
                let new_array = gc.alloc(self.slice_array(&array, from, to)?)?;
                self.stack.push(new_array);
            }
            Type::Str => {
                let array = array.as_str();
                let new_array = self.slice_array(&array.0, from, to)?;
                let new_str = gc.alloc(Str(new_array))?;
                self.stack.push(new_str);
            }
            ty => self.assert_type(ty, OneOf(Type::Array, Type::Str))?,
        }

        self.gc_tick(gc);

        Ok(())
    }

    fn slice_array<T: Trace>(
        &mut self,
        array: &Array<'gc, T>,
        from: Option<Value<'gc>>,
        to: Option<Value<'gc>>,
    ) -> Result<Array<'gc, T>, Error> {
        let from = from.map(|v| array_idx(v, array.len())).unwrap_or(0);
        let mut to = to.map(|v| array_idx(v, array.len())).unwrap_or(array.len());

        if from > array.len() {
            return Ok(Array::default());
        }

        if to > array.len() {
            to = array.len();
        }

        Ok(array.slice(from, to))
    }

    fn store_elem(&mut self) -> Result<(), Error> {
        let value = self.stack.pop();
        let elem = self.stack.pop();
        let array = self.stack.pop();

        self.assert_type(elem.ty(), OneOf(Type::Int, Type::BigInt))?;

        match array.ty() {
            Type::Array => {
                let mut array = array.as_array();
                let idx = array_idx(elem, array.len());
                let elem = array
                    .get_mut(idx)
                    .ok_or_else(|| self.index_out_of_bounds(idx))?;

                *elem = value;
            }
            Type::Str => {
                self.assert_type(value.ty(), OneOf(Type::Int, Type::BigInt))?;

                let array = &mut array.as_str().0;
                let idx = array_idx(elem, array.len());
                let elem = array
                    .get_mut(idx)
                    .ok_or_else(|| self.index_out_of_bounds(idx))?;

                *elem = value.as_bigint().as_usize() as u8;
            }
            _ => self.assert_type(array.ty(), OneOf(Type::Array, Type::Str))?,
        }

        Ok(())
    }

    fn load_elem(&mut self) -> Result<(), Error> {
        let elem = self.stack.pop();
        let array = self.stack.pop();

        self.assert_type(elem.ty(), OneOf(Type::Int, Type::BigInt))?;

        let elem = match array.ty() {
            Type::Array => {
                let handle = array.as_array();
                let idx = array_idx(elem, handle.len());
                handle
                    .get(idx)
                    .cloned()
                    .ok_or_else(|| self.index_out_of_bounds(idx))?
            }
            Type::Str => {
                let handle = &array.as_str().0;
                let idx = array_idx(elem, handle.len());
                handle
                    .get(idx)
                    .copied()
                    .map(Value::from)
                    .ok_or_else(|| self.index_out_of_bounds(idx))?
            }
            _ => return self.assert_type(array.ty(), OneOf(Type::Array, Type::Str)),
        };

        self.stack.push(elem);

        Ok(())
    }

    fn make_path(&self, name: &str) -> PathBuf {
        self.search_path
            .join("stdlib")
            .join(format!("{name}.atom"))
    }

    fn set_frame(&mut self, frame: Frame<'gc>) {
        self.call_stack.push(mem::replace(&mut self.frame, frame));
    }

    fn import_path(&mut self, gc: &mut Gc<'gc>, name: &str) -> Result<usize, Error> {
        let id = self.modules.len();

        // Parse and compile module
        let mut package = compile(self.make_path(name), &mut self.global_context)
            .map_err(Box::new)
            .map_err(Error::Import)?;

        // Initialize module and frame
        let meta = Metadata::new(name.to_string());
        let frame = Frame::from_package(gc, Context::with_module(id), &mut package)?;

        self.modules.push(Module::new(id, meta, package));

        // Setup package class
        let class = Handle::clone(&self.package);
        let mut object = gc.alloc(Object::new(class))?;
        object.set_attr(value::MODULE, BigInt::from(id).into_atom(gc)?);

        // Insert the package object as the first variable (which is `self`)
        self.modules[id]
            .vars
            .insert(0, Handle::clone(&object).into());
        self.packages
            .insert(name.to_string(), Handle::clone(&object));

        // Push package to the stack (it will be stored)
        self.stack.push(object);

        // Activate the frame
        self.set_frame(frame);
        self.gc_tick(gc);

        Ok(id)
    }

    fn import(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let name = self.module().load_const(gc, idx as usize)?.as_str();

        if let Some(handle) = self.packages.get(name.as_str()) {
            self.stack.push(Handle::clone(handle));
            return Ok(());
        }

        self.import_path(gc, name.as_str())?;

        Ok(())
    }

    fn not(&mut self) -> Result<(), Error> {
        let value = self.stack.pop();

        self.stack.push(if value.as_atom() == value::TRUE {
            Value::new(Tag::Atom, value::FALSE as u64)
        } else {
            Value::new(Tag::Atom, value::TRUE as u64)
        });

        Ok(())
    }

    fn eq_op(&mut self) -> Result<bool, Error> {
        let (lhs, rhs) = self.stack.operands();

        if lhs == rhs {
            return Ok(true);
        }

        let ty = lhs.ty();

        if ty != rhs.ty() {
            return Ok(false);
        }

        match ty {
            Type::Int => Ok(lhs.as_bigint() == rhs.as_bigint()),
            Type::Float => Ok(lhs.as_float() == rhs.as_float()),
            Type::Str => Ok(lhs.as_str() == rhs.as_str()),
            Type::Atom => Ok(lhs.as_atom() == rhs.as_atom()),
            _ => Err(self.unsupported("==", lhs.ty(), rhs.ty())),
        }
    }

    fn eq(&mut self) -> Result<(), Error> {
        let value = self.eq_op()?;
        self.stack.push(Value::from(value));
        Ok(())
    }

    fn ne(&mut self) -> Result<(), Error> {
        let value = self.eq_op()?;
        self.stack.push(Value::from(!value));
        Ok(())
    }

    fn concat(&mut self, gc: &mut Gc<'gc>, lhs: Value<'gc>, rhs: Value<'gc>) -> Result<(), Error> {
        let value = match lhs.ty() {
            Type::Array => {
                let lhs = lhs.as_array();
                let rhs = rhs.as_array();

                [lhs.as_slice(), rhs.as_slice()].concat().into_atom(gc)
            }
            Type::Str => {
                let lhs = lhs.as_str();
                let rhs = rhs.as_str();

                [lhs.as_str(), rhs.as_str()].concat().into_atom(gc)
            }
            _ => unreachable!(),
        }?;

        self.stack.push(value);
        Ok(())
    }

    fn bit_xor(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();
        let value = match (lhs.tag(), rhs.tag()) {
            (Tag::Int, Tag::Int) => (lhs.as_int() ^ rhs.as_int()).into_atom(gc)?,
            (Tag::BigInt, Tag::BigInt) | (Tag::BigInt, Tag::Int) | (Tag::Int, Tag::BigInt) => {
                lhs.as_bigint().bitxor(&rhs.as_bigint()).into_atom(gc)?
            }
            (Tag::Array, Tag::Array) | (Tag::Str, Tag::Str) => return self.concat(gc, lhs, rhs),
            _ => return Err(self.unsupported("^", lhs.ty(), rhs.ty())),
        };

        self.stack.push(value);
        Ok(())
    }

    fn store(&mut self, idx: u32) -> Result<(), Error> {
        let var = self.stack.pop();
        self.module().vars.insert(idx, var);
        Ok(())
    }

    fn load(&mut self, idx: u32) {
        let value = Value::clone(&self.module().vars[&idx]);
        self.stack.push(value);
    }

    fn load_atom(&mut self, idx: u32) {
        self.stack.push(Value::new(Tag::Atom, idx as u64));
    }

    fn type_assert(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();

        if !rhs.is_class() {
            return self.assert_type(rhs.ty(), Type::Class);
        }

        let rhs = rhs.as_class();

        if lhs.is_object() && lhs.as_object().class == rhs {
            self.stack.push(true.into_atom(gc)?);
            return Ok(());
        }

        if let Some(lhs) = self.modules[self.std.module].load_class_by_name(gc, lhs.ty().name())? {
            self.stack.push((lhs == rhs).into_atom(gc)?);
            return Ok(());
        }

        self.stack.push(false.into_atom(gc)?);
        Ok(())
    }

    fn load_const(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let value = Value::clone(&self.module().load_const(gc, idx as usize)?);
        self.stack.push(value);

        Ok(())
    }

    fn load_const_int(&mut self, i: u32) {
        self.stack.push(Value::new(Tag::Int, i as u64));
    }

    fn discard(&mut self) {
        let _ = self.stack.pop();
    }

    fn ret(&mut self, yielded: bool) -> Result<(), Error> {
        self.frame.returned = true;

        if let Some(mut resumable) = self.frame.as_resumable() {
            if yielded {
                resumable.offset = self.frame.offset;
            } else {
                resumable.state = ResumableState::Completed;
            }
        }

        if let Some(frame) = self.call_stack.pop() {
            self.frame = frame;
        }

        Ok(())
    }

    #[inline(always)]
    fn eval(&mut self, gc: &mut Gc<'gc>, builtins: &mut Builtins) -> Result<(), Error> {
        while let Some(bc) = self.frame.next() {
            #[cfg(feature = "profiler")]
            self.profiler.record_instruction(bc.op);

            match bc.op {
                Op::Add => self.add(gc)?,
                Op::Sub => self.sub(gc)?,
                Op::Mul => self.mul(gc)?,
                Op::Div => self.div(gc)?,
                Op::Rem => self.rem(gc)?,
                Op::Eq => self.eq()?,
                Op::Ne => self.ne()?,
                Op::Lt => self.lt(gc)?,
                Op::Lte => self.le(gc)?,
                Op::Gt => self.gt(gc)?,
                Op::Gte => self.ge(gc)?,
                Op::TypeAssert => self.type_assert(gc)?,
                Op::BitwiseAnd => self.bitand(gc)?,
                Op::BitwiseOr => self.bitor(gc)?,
                Op::BitwiseXor => self.bit_xor(gc)?,
                Op::ShiftLeft => self.shl(gc)?,
                Op::ShiftRight => self.shr(gc)?,
                Op::LoadConst => self.load_const(gc, bc.code)?,
                Op::LoadConstInt => self.load_const_int(bc.code),
                Op::LoadAtom => self.load_atom(bc.code),
                Op::LoadMember => self.load_member(gc, bc.code)?,
                Op::StoreMember => self.store_member(bc.code)?,
                Op::LoadFn => self.load_fn(gc, bc.code)?,
                Op::LoadClass => self.load_class(gc, bc.code)?,
                Op::Store => self.store(bc.code)?,
                Op::Load => self.load(bc.code),
                Op::LoadLocal => self.load_local(bc.code),
                Op::StoreLocal => self.store_local(bc.code),
                Op::Discard => self.discard(),
                Op::Call => self.call(gc, bc.code)?,
                Op::CallFn => self.call_fn(gc, bc.code2())?,
                Op::CallBuiltin => self.call_builtin(gc, builtins, bc.code2())?,
                Op::TailCall => self.tail_call(gc, bc.code)?,
                Op::Jump => self.jump(bc.code),
                Op::JumpIfFalse => self.jump_if_false(bc.code)?,
                Op::PushJumpIfTrue => self.jump_cond(bc.code, value::TRUE)?,
                Op::PushJumpIfFalse => self.jump_cond(bc.code, value::FALSE)?,
                Op::MakeArray => self.make_array(gc, bc.code)?,
                Op::MakeSlice => self.make_slice(gc, bc.code)?,
                Op::LoadElement => self.load_elem()?,
                Op::StoreElement => self.store_elem()?,
                Op::UnaryNot => self.not()?,
                Op::Import => self.import(gc, bc.code)?,
                Op::Yield => self.ret(true)?,
                Op::Return => self.ret(false)?,
                Op::ReturnLocal => self.return_local(bc.code)?,
                Op::Nop => unreachable!(),
            }
        }

        Ok(())
    }

    fn add_trace(&mut self, e: Error) -> Error {
        match e {
            Error::Runtime(mut e) => {
                let mut stack_trace = self
                    .call_stack
                    .iter()
                    .chain(&[Frame::new(Handle::clone(&self.frame.handle))])
                    .enumerate()
                    .map(|(i, frame)| {
                        if i > 0 {
                            if let Some(caller) = self.call_stack.get(i - 1) {
                                let module = &self.modules[caller.context().module];
                                let span = module.span_at(caller.offset);

                                return Call::new(span, Some(frame.handle.name.clone()));
                            }
                        }

                        Call::new(Span::default(), None)
                    })
                    .collect::<Vec<_>>();

                stack_trace.reverse();

                e.trace = Some(stack_trace);
                Error::Runtime(e)
            }
            e => e,
        }
    }

    #[cfg(feature = "profiler")]
    pub fn profiler(mut self) -> crate::profiler::VmProfiler {
        mem::take(&mut self.profiler)
    }

    fn bootstrap(&mut self, gc: &mut Gc<'gc>, builtins: &mut Builtins) -> Result<(), Error> {
        let id = self.import_path(gc, "std")?;
        self.std = Context::with_module(id);

        let user_frame = self.call_stack.remove(0);

        self.dispatch(gc, builtins)?;
        self.set_frame(user_frame);
        self.call_stack.clear();

        Ok(())
    }

    fn dispatch(&mut self, gc: &mut Gc<'gc>, builtins: &mut Builtins) -> Result<(), Error> {
        loop {
            self.eval(gc, builtins)?;

            match self.call_stack.pop() {
                Some(frame) => {
                    if !self.frame.returned {
                        self.stack.push(Value::default());
                    }

                    self.frame = frame;
                }
                None => break,
            }
        }

        Ok(())
    }

    pub fn run(&mut self, gc: &mut Gc<'gc>, builtins: &mut Builtins) -> Result<(), Error> {
        #[cfg(feature = "profiler")]
        self.profiler.enter();
        self.bootstrap(gc, builtins)
            .and_then(|_| self.dispatch(gc, builtins))
            .map_err(|e| self.add_trace(e))
    }
}

impl_binary!(
    (lt: <),
    (le: <=),
    (gt: >),
    (ge: >=),
    (add: +),
    (sub: -),
    (mul: *),
    (div: /),
    (rem: %),
    (bitand: &),
    (bitor: |),
    (shl: <<),
    (shr: >>)
);
