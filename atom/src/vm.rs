use std::{
    borrow::Cow,
    collections::HashMap,
    fs, mem,
    ops::*,
    path::{Path, PathBuf},
};

use linear_map::LinearMap;
use wyhash2::WyHash;

use crate::{
    ast::Stmt,
    bytecode::Op,
    collections::Stack,
    compiler::{Compiler, Context, Package},
    error::SpannedError,
    frame::Frame,
    gc::{Gc, Handle, Trace},
    instance::Instance,
    lexer::{Lexer, Span},
    parser::{self},
    runtime::{
        array::Array,
        bigint::BigInt,
        class::{Class, Object},
        error::{Call, ErrorKind, RuntimeError},
        function::{Fn, Method},
        str::Str,
        value::{self, IntoAtom, Tag, Type, Value},
    },
};

fn array_idx(elem: Value, len: usize) -> usize {
    match elem.as_bigint().as_i64() {
        n if n < 0 => len + n as usize,
        n => n as usize,
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FatalErrorKind {
    #[error("invalid extern function '{0}'")]
    InvalidExternFn(String),
}

pub type FatalError = SpannedError<FatalErrorKind>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("ImportError: {0}")]
    Import(#[from] Box<crate::error::Error>),
    #[error("RuntimeError: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("FatalError: {0}")]
    Fatal(#[from] FatalError),
}

fn parse(source: &str) -> Result<Vec<Stmt>, crate::error::Error> {
    let chars = source.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.lex()?;
    let parser = parser::Parser::new(tokens);

    Ok(parser.parse()?)
}

pub fn compile(
    source: impl AsRef<Path>,
    ctx: &mut Context,
    optimize: bool,
) -> Result<Package, crate::error::Error> {
    let source = fs::read_to_string(source)?;
    let program = parse(&source)?;
    let compiler = Compiler::default().with_optimize(optimize);

    Ok(compiler.compile(ctx, program)?)
}

macro_rules! impl_op {
    ($(($fn:ident: $op:tt)),+) => {
        impl<'gc, F: Ffi<'gc>, const S: usize> Vm<'gc, F, S> {
            $(fn $fn(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
                let (lhs, rhs) = self.stack.operands();

                self.stack.push(match (lhs.tag(), rhs.tag()) {
                    (Tag::Int, Tag::Int) => lhs.as_int().$fn(&rhs.as_int()).into_atom(gc)?,
                    (Tag::BigInt, Tag::BigInt) | (Tag::Int, Tag::BigInt) | (Tag::BigInt, Tag::Int) => {
                        lhs.as_bigint().$fn(&rhs.as_bigint()).into_atom(gc)?
                    },
                    (Tag::Float, Tag::Float) => lhs.as_float().$fn(&rhs.as_float()).into_atom(gc)?,
                    _ => return Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty())),
                });

                Ok(())
            })+
        }
    };
}

macro_rules! impl_binary {
    (final $fn:ident: $op:tt $($float:pat)?) => {
        fn $fn(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
            let (lhs, rhs) = self.stack.operands();

            if lhs.is_int() && rhs.is_int() {
                self.stack.push(lhs.as_int().$fn(rhs.as_int()).into_atom(gc)?);
                return Ok(());
            }

            self.stack.push(match (lhs.tag(), rhs.tag()) {
                (Tag::BigInt, Tag::BigInt) | (Tag::Int, Tag::BigInt) | (Tag::BigInt, Tag::Int) => {
                    lhs.as_bigint().$fn(&rhs.as_bigint()).into_atom(gc)?
                }
                $($float => lhs.as_float().$fn(rhs.as_float()).into_atom(gc)?,)?
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
        impl<'gc, F: Ffi<'gc>, const S: usize> Vm<'gc, F, S> {
            $(impl_binary!($fn: $op);)+
        }
    };
}

pub trait Ffi<'gc> {
    fn call(
        &mut self,
        name: &str,
        gc: &mut Gc<'gc>,
        args: Vec<Value<'gc>>,
    ) -> Result<Value<'gc>, Error>;
}

#[derive(Default)]
struct Std<'gc> {
    instance: usize,
    builtins: LinearMap<Cow<'static, str>, Handle<'gc, Class<'gc>>>,
}

impl<'gc> Trace for Std<'gc> {
    fn trace(&self, gc: &mut Gc<'_>) {
        self.builtins.values().for_each(|handle| gc.mark(handle));
    }
}

pub struct Vm<'gc, F: Ffi<'gc>, const S: usize> {
    ffi: F,
    std: Std<'gc>,
    frame: Frame<'gc>,
    search_path: PathBuf,
    compile_context: Context,
    stack: Stack<Value<'gc>, S>,
    call_stack: Vec<Frame<'gc>>,
    instances: Vec<Instance<'gc>>,
    #[cfg(feature = "profiler")]
    profiler: crate::profiler::VmProfiler,
    packages: HashMap<String, Handle<'gc, Object<'gc>>, WyHash>,
}

fn instance_with_frame<'gc>(
    id: usize,
    gc: &mut Gc<'gc>,
    mut package: Package,
) -> Result<(Instance<'gc>, Frame<'gc>), Error> {
    let mut func = gc.alloc(Fn::builder().body(mem::take(&mut package.body)).build())?;
    let consts = mem::take(&mut package.consts)
        .into_iter()
        .map(|c| c.into_atom(gc))
        .collect::<Result<Vec<_>, _>>()?;

    let instance = Instance::new(id, package, consts);
    func.context.instance = id;

    let frame = Frame::new(func);

    Ok((instance, frame))
}

impl<'gc, F: Ffi<'gc>, const S: usize> Vm<'gc, F, S> {
    pub fn new(
        gc: &mut Gc<'gc>,
        compile_context: Context,
        search_path: PathBuf,
        package: Package,
        ffi: F,
    ) -> Result<Self, Error> {
        let (instance, frame) = instance_with_frame(0, gc, package)?;

        Ok(Self {
            ffi,
            frame,
            search_path,
            compile_context,
            std: Std::default(),
            packages: HashMap::default(),
            instances: vec![instance],
            call_stack: vec![],
            stack: Stack::default(),
            #[cfg(feature = "profiler")]
            profiler: crate::profiler::VmProfiler::default(),
        })
    }

    fn unsupported_rhs(&self, lhs: Value, rhs: Value, op: &'static str) -> Error {
        ErrorKind::UnsupportedBinaryOp {
            left: lhs.ty(),
            right: rhs.ty(),
            op,
        }
        .at(self.frame.span())
        .into()
    }

    fn unsupported(&self, op: &'static str, lty: Type, rty: Type) -> Error {
        ErrorKind::UnsupportedOp { lty, rty, op }
            .at(self.frame.span())
            .into()
    }

    fn index_out_of_bounds(&self, idx: usize) -> Error {
        ErrorKind::IndexOutOfBounds(idx)
            .at(self.frame.span())
            .into()
    }

    fn gc_tick(&mut self, gc: &mut Gc) {
        if !gc.ready() {
            return;
        }

        self.mark_sweep(gc);
    }

    fn mark_sweep(&mut self, gc: &mut Gc) {
        self.frame.trace(gc);
        self.packages.values().for_each(|handle| gc.mark(handle));
        self.call_stack.iter().for_each(|frame| frame.trace(gc));
        self.instances
            .iter()
            .for_each(|instance| instance.trace(gc));
        self.stack.iter().for_each(|value| value.trace(gc));

        gc.sweep();
    }

    #[inline]
    fn jump(&mut self, pos: u32) {
        self.frame.offset = pos as usize * 8;
    }

    fn load_member(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let recv = self.stack.pop();

        if recv.is_object() {
            return self.load_attr(gc, recv, idx);
        }

        let member = self.compile_context.get_atom(idx);

        if let Some(mut class) = self.std.builtins.get(recv.ty().name()).cloned() {
            if let Some(func) = class.get_method(gc, member)? {
                let method = Method::new(recv, func);
                let handle = gc.alloc(method)?;

                self.stack.push(handle.into());
                return Ok(());
            }
        }

        Err(ErrorKind::UnknownField {
            ty: recv.ty(),
            field: member.to_string(),
        }
        .at(self.frame.span())
        .into())
    }

    fn store_member(&mut self, member: u32) -> Result<(), Error> {
        let value = self.stack.pop();
        let object = self.stack.pop();

        if !object.is_object() {
            self.assert_type(object.ty(), Type::Object)?;
        }

        let mut object = object.as_object();
        object.set_attr(member, value);

        Ok(())
    }

    fn load_export(
        &mut self,
        gc: &mut Gc<'gc>,
        object: Handle<'gc, Object<'gc>>,
        member: u32,
    ) -> Result<(), Error> {
        let member = self.compile_context.get_atom(member);
        let instance_id = object
            .get_attr(value::INSTANCE)
            .ok_or_else(|| ErrorKind::MissingAttribute("instance").at(self.frame.span()))?
            .as_int();
        let instance = &mut self.instances[instance_id as usize];

        if let Some(handle) = instance.get_fn_by_name(gc, member)? {
            if handle.public {
                self.stack.push(handle.into());
                return Ok(());
            }
        }

        if let Some(handle) = instance.get_class_by_name(gc, member)? {
            if handle.public {
                self.stack.push(handle.into());
                return Ok(());
            }
        }

        Err(ErrorKind::UnknownAttr {
            class_name: object.class.name.clone(),
            attribute: member.to_string(),
        }
        .at(self.frame.span())
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

        if object.class.name == "Package" {
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
        let member_name = self.compile_context.get_atom(member);
        let func = object.class.get_method(gc, member_name)?.ok_or_else(|| {
            ErrorKind::UnknownAttr {
                class_name: object.class.name.clone(),
                attribute: member_name.to_string(),
            }
            .at(self.frame.span())
        })?;

        let method = Method::new(Handle::clone(&object).into(), func);
        let handle = gc.alloc(method)?;

        object.set_attr(member, Handle::clone(&handle).into());
        self.stack.push(handle.into());

        Ok(())
    }

    fn load_class(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let class = self.instances[self.frame.instance()].get_class(gc, idx as usize)?;
        self.stack.push(class.into());
        Ok(())
    }

    fn load_fn(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let f = self.instances[self.frame.instance()].get_fn(gc, idx as usize)?;
        self.stack.push(f.into());
        Ok(())
    }

    fn return_local(&mut self, idx: u32) -> Result<(), Error> {
        self.load_local(idx);
        self.ret()
    }

    fn load_local(&mut self, idx: u32) {
        self.stack
            .push(Value::clone(&self.frame.locals[idx as usize]));
    }

    fn store_local(&mut self, idx: u32) {
        self.frame.locals[idx as usize] = self.stack.pop();
    }

    fn assert_type(&self, left: Type, right: Type) -> Result<(), Error> {
        if left != right {
            return Err(ErrorKind::TypeMismatch { left, right }
                .at(self.frame.span())
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
            self.stack.push(cond.into());
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
        let object = Object::new(class);
        let handle = gc.alloc(object)?;

        if let Some(f) = init {
            let method = Method::new(handle.into(), f);
            let handle = gc.alloc(method)?;

            self.stack.push(handle.into());
            self.call(gc, arg_count)?;
        } else {
            self.stack.push(handle.into());
        }

        self.gc_tick(gc);

        Ok(())
    }

    fn call_extern(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let name = self.instances[self.frame.instance()].consts[idx as usize].as_str();
        let args = mem::take(&mut self.frame.locals);
        let return_value = self.ffi.call(name.as_ref(), gc, args)?;

        self.frame.returned = true;
        self.stack.push(return_value);
        self.gc_tick(gc);

        Ok(())
    }

    fn call_fn(&mut self, gc: &mut Gc<'gc>, (idx, arg_count): (u16, u16)) -> Result<(), Error> {
        let f = self.instances[self.frame.instance()].get_fn(gc, idx as usize)?;
        self.fn_call(gc, f, arg_count as u32)
    }

    fn call(&mut self, gc: &mut Gc<'gc>, arg_count: u32) -> Result<(), Error> {
        let callee = self.stack.pop();

        match callee.ty() {
            Type::Fn => self.fn_call(gc, callee.as_fn(), arg_count),
            Type::Method => self.method_call(gc, callee.as_method(), arg_count),
            Type::Class => self.init_class(gc, callee.as_class(), arg_count),
            ty => Err(ErrorKind::NotCallable(ty).at(self.frame.span()).into()),
        }
    }

    fn method_call(
        &mut self,
        gc: &mut Gc,
        method: Handle<'gc, Method<'gc>>,
        arg_count: u32,
    ) -> Result<(), Error> {
        if arg_count != method.func.arg_count - 1 {
            return Err(ErrorKind::ArgCountMismatch {
                func_name: method.func.name.clone(),
                func_arg_count: method.func.arg_count - 1,
                arg_count,
            }
            .at(self.frame.span())
            .into());
        }

        let frame = Frame::new(Handle::clone(&method.func));
        self.call_stack.push(mem::replace(&mut self.frame, frame));
        let locals = self.stack.slice_to_end(arg_count as usize);

        self.frame.locals = Vec::with_capacity(locals.len() + 1);
        self.frame.locals.push(Value::clone(&method.recv));
        self.frame.locals.extend_from_slice(locals);
        self.gc_tick(gc);

        Ok(())
    }

    fn fn_call(&mut self, gc: &mut Gc, func: Handle<'gc, Fn>, arg_count: u32) -> Result<(), Error> {
        if arg_count != func.arg_count {
            return Err(ErrorKind::ArgCountMismatch {
                func_name: func.name.clone(),
                func_arg_count: func.arg_count,
                arg_count,
            }
            .at(self.frame.span())
            .into());
        }

        self.set_frame(Frame::new(func));
        self.frame.locals = self.stack.slice_to_end(arg_count as usize).to_vec();
        self.gc_tick(gc);

        Ok(())
    }

    fn tail_call(&mut self, gc: &mut Gc, arg_count: u32) -> Result<(), Error> {
        self.frame.offset = 0;
        self.frame.returned = false;
        self.frame.locals = self.stack.slice_to_end(arg_count as usize).to_vec();
        self.gc_tick(gc);

        Ok(())
    }

    fn make_array(&mut self, gc: &mut Gc<'gc>, size: u32) -> Result<(), Error> {
        let array = self.stack.slice_to_end(size as usize);
        let array = Array::copy_from_slice(gc, array)?;
        let handle = gc.alloc(array)?;

        self.stack.push(handle.into());
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
            self.assert_type(from.ty(), Type::Int)?;
        }

        if let Some(to) = to.as_ref() {
            self.assert_type(to.ty(), Type::Int)?;
        }

        let array = self.stack.pop();

        match array.ty() {
            Type::Array => {
                let array = array.as_array();
                let new_array = gc.alloc(self.slice_array(&array, from, to)?)?;
                self.stack.push(new_array.into());
            }
            Type::Str => {
                let array = array.as_str();
                let new_array = self.slice_array(&array.0, from, to)?;
                let new_str = gc.alloc(Str(new_array))?;
                self.stack.push(new_str.into());
            }
            ty => self.assert_type(ty, Type::Array)?,
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

        self.assert_type(elem.ty(), Type::Int)?;

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
                self.assert_type(value.ty(), Type::Int)?;

                let array = &mut array.as_str().0;
                let idx = array_idx(elem, array.len());
                let elem = array
                    .get_mut(idx)
                    .ok_or_else(|| self.index_out_of_bounds(idx))?;

                *elem = value.as_bigint().as_usize() as u8;
            }
            _ => self.assert_type(array.ty(), Type::Array)?,
        }

        Ok(())
    }

    fn load_elem(&mut self) -> Result<(), Error> {
        let elem = self.stack.pop();
        let array = self.stack.pop();

        self.assert_type(elem.ty(), Type::Int)?;

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
            _ => return self.assert_type(array.ty(), Type::Array),
        };

        self.stack.push(elem);

        Ok(())
    }

    fn make_path(&self, name: &str) -> PathBuf {
        self.search_path
            .join("stdlib")
            .join(format!("{}.atom", name))
    }

    fn set_frame(&mut self, frame: Frame<'gc>) {
        self.call_stack.push(mem::replace(&mut self.frame, frame));
    }

    fn import_path(&mut self, gc: &mut Gc<'gc>, name: &str) -> Result<usize, Error> {
        let id = self.instances.len();

        // Parse and compile module
        let module = compile(self.make_path(name), &mut self.compile_context, true)
            .map_err(|e| Error::Import(Box::new(e)))?;

        // Initialize instance and frame
        let (mut instance, frame) = instance_with_frame(id, gc, module)?;

        // Setup package class
        let class = gc.alloc(Class::new("Package", self.frame.instance()))?;
        let mut object = Object::new(class);
        object.set_attr(value::INSTANCE, BigInt::from(id).into_atom(gc)?);

        let handle = gc.alloc(object)?;

        // Insert the package object as the first variable (which is `self`)
        instance.vars.insert(0, Handle::clone(&handle).into());

        self.instances.push(instance);
        self.packages
            .insert(name.to_string(), Handle::clone(&handle));

        // Push package to the stack (it will be stored)
        self.stack.push(handle.into());

        // Activate the frame
        self.set_frame(frame);
        self.gc_tick(gc);

        Ok(id)
    }

    fn import(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let name = self.instances[self.frame.instance()].consts[idx as usize].as_str();

        if let Some(handle) = self.packages.get(name.as_str()) {
            self.stack.push(Handle::clone(handle).into());
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

        Ok(match ty {
            Type::Int => lhs.as_bigint() == rhs.as_bigint(),
            Type::Float => lhs.as_float() == rhs.as_float(),
            Type::Str => lhs.as_str() == rhs.as_str(),
            Type::Atom => lhs.as_atom() == rhs.as_atom(),
            _ => return Err(self.unsupported_rhs(lhs, rhs, "==")),
        })
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
        self.instances[self.frame.instance()].vars.insert(idx, var);
        Ok(())
    }

    fn load(&mut self, idx: u32) {
        self.stack.push(Value::clone(
            &self.instances[self.frame.instance()].vars[&idx],
        ));
    }

    fn load_atom(&mut self, idx: u32) {
        self.stack.push(Value::new(Tag::Atom, idx as u64));
    }

    fn load_const(&mut self, idx: u32) {
        self.stack.push(Value::clone(
            &self.instances[self.frame.instance()].consts[idx as usize],
        ));
    }

    fn discard(&mut self) {
        let _ = self.stack.pop();
    }

    fn ret(&mut self) -> Result<(), Error> {
        if let Some(frame) = self.call_stack.pop() {
            self.frame = frame;
        }

        Ok(())
    }

    #[inline(always)]
    fn eval(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
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
                Op::BitwiseAnd => self.bitand(gc)?,
                Op::BitwiseOr => self.bitor(gc)?,
                Op::BitwiseXor => self.bit_xor(gc)?,
                Op::ShiftLeft => self.shl(gc)?,
                Op::ShiftRight => self.shr(gc)?,
                Op::LoadConst => self.load_const(bc.code),
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
                Op::CallExtern => self.call_extern(gc, bc.code)?,
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
                Op::Return => self.ret()?,
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
                                return Call::new(caller.span(), Some(frame.handle.name.clone()));
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

    fn bootstrap(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        self.std.instance = self.import_path(gc, "std")?;

        let user_frame = self.call_stack.remove(0);

        self.dispatch(gc)?;
        self.set_frame(user_frame);
        self.call_stack.clear();

        let instance = &mut self.instances[self.std.instance];
        let classes = instance
            .package()
            .classes
            .iter()
            .enumerate()
            .filter_map(|(id, c)| if c.public { Some(id) } else { None })
            .collect::<Vec<_>>();

        for id in classes {
            let class = instance.get_class(gc, id)?;
            self.std.builtins.insert(class.name.clone(), class);
        }

        Ok(())
    }

    fn dispatch(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        loop {
            self.eval(gc)?;

            match self.call_stack.pop() {
                Some(frame) => {
                    if !self.frame.returned && !self.call_stack.is_empty() {
                        self.stack.push(Value::default());
                    }

                    self.frame = frame;
                }
                None => break,
            }
        }

        Ok(())
    }

    pub fn run(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        #[cfg(feature = "profiler")]
        self.profiler.enter();
        self.bootstrap(gc)
            .and_then(|_| self.dispatch(gc))
            .map_err(|e| self.add_trace(e))
    }
}

impl_op!(
    (lt: <),
    (le: <=),
    (gt: >),
    (ge: >=)
);

impl_binary!(
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
