use std::{borrow::Cow, collections::HashMap, mem, path::PathBuf};

use bytes::Buf;
use wyhash2::WyHash;

use crate::{
    bytecode::{Bytecode, Op, Serializable},
    compiler::Package,
    error::SpannedError,
    gc::{Gc, Handle, Trace},
    instance::Instance,
    lexer::Span,
    runtime::{
        array::Array,
        class::{Class, Object},
        error::{Call, ErrorKind, RuntimeError},
        function::{Fn, Method},
        str::Str,
        value::{self, IntoAtom, Type, Value},
    },
    stack::Stack,
    utils,
};

fn array_idx(elem: Value, len: usize) -> usize {
    match elem.int() {
        n if n < 0 => (len as i64 + n) as usize,
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
    Import(#[from] Box<crate::Error>),
    #[error("RuntimeError: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("FatalError: {0}")]
    Fatal(#[from] FatalError),
}

struct Frame<'gc> {
    call_site: usize,
    offset: usize,
    locals: Vec<Value<'gc>>,
    instance: usize,
    returned: bool,
    global: bool,
    handle: Handle<'gc, Fn>,
}

impl<'gc> Trace for Frame<'gc> {
    fn trace(&self, gc: &mut Gc) {
        gc.mark(&self.handle);
        self.locals.iter().for_each(|v| v.trace(gc));
    }
}

impl<'gc> Frame<'gc> {
    pub fn new(call_site: usize, handle: Handle<'gc, Fn>) -> Self {
        Self {
            call_site,
            offset: 0,
            locals: vec![],
            instance: handle.inline.instance,
            returned: false,
            handle,
            global: false,
        }
    }

    pub fn with_global(call_site: usize, handle: Handle<'gc, Fn>, instance: usize) -> Self {
        Self {
            call_site,
            offset: 0,
            locals: vec![],
            instance,
            returned: false,
            handle,
            global: true,
        }
    }

    pub fn next(&mut self) -> Option<Bytecode> {
        if self.offset < self.handle.body.len() {
            let bc = Bytecode::deserialize(&mut &self.handle.body[self.offset..self.offset + 5]);
            self.offset += 8;
            return Some(bc);
        }

        None
    }

    /// Get the span given the assumption that we're already at the next bytecode
    pub fn span(&self) -> Span {
        self.span_at(self.offset - 3)
    }

    pub fn span_at(&self, offset: usize) -> Span {
        if offset >= self.handle.body.len() {
            return self.span_at(self.handle.body.len() - 3);
        }

        let mut tail = &self.handle.body[if offset == 0 {
            5..8
        } else {
            offset..offset + 3
        }];
        Span {
            offset: tail.get_uint(3) as u32,
        }
    }
}

macro_rules! impl_binary {
    ($(($name:ident: $op:tt)),+) => {
        impl<'gc, F: Ffi<'gc>, const S: usize> Vm<'gc, F, S> {
            $(
                fn $name(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
                    let (lhs, rhs) = self.stack.operands();
                    let value = if lhs.is_int() && rhs.is_int() {
                        (lhs.int() $op rhs.int()).into_atom(gc)?
                    } else if lhs.is_float() && rhs.is_float() {
                        (lhs.float() $op rhs.float()).into_atom(gc)?
                    } else {
                        return Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty()));
                    };

                    self.stack.push(value);
                    Ok(())
                }
            )+
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

struct Cache<'gc> {
    package_class: Handle<'gc, Class<'gc>>,
    packages: HashMap<String, Handle<'gc, Object<'gc>>, WyHash>,
}

impl<'gc> Trace for Cache<'gc> {
    fn trace(&self, gc: &mut Gc) {
        gc.mark(&self.package_class);
        self.packages.values().for_each(|handle| gc.mark(handle));
    }
}

pub struct Vm<'gc, F: Ffi<'gc>, const S: usize> {
    ffi: F,
    frame: Frame<'gc>,
    cache: Cache<'gc>,
    search_path: PathBuf,
    stack: Stack<Value<'gc>, S>,
    call_stack: Vec<Frame<'gc>>,
    instances: Vec<Instance<'gc>>,
    #[cfg(feature = "profiler")]
    profiler: crate::profiler::VmProfiler,
}

fn root_frame<'gc>(
    id: usize,
    gc: &mut Gc<'gc>,
    mut package: Package,
) -> Result<(Instance<'gc>, Frame<'gc>), Error> {
    let func = gc.alloc(Fn::builder().body(mem::take(&mut package.body)).build())?;
    let consts = mem::take(&mut package.consts)
        .into_iter()
        .map(|c| c.into_atom(gc))
        .collect::<Result<Vec<_>, _>>()?;

    let instance = Instance::new(id, package, consts);
    let frame = Frame::with_global(id, func, id);

    Ok((instance, frame))
}

impl<'gc, F: Ffi<'gc>, const S: usize> Vm<'gc, F, S> {
    pub fn new(
        gc: &mut Gc<'gc>,
        search_path: PathBuf,
        package: Package,
        ffi: F,
    ) -> Result<Self, Error> {
        let (mut instance, frame) = root_frame(0, gc, package)?;
        let cache = Cache {
            packages: HashMap::default(),
            package_class: instance.get_class_by_name(gc, "Package")?.unwrap(), // @TODO: handle error
        };

        Ok(Self {
            ffi,
            frame,
            cache,
            search_path,
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

    fn gc_tick(&mut self, gc: &mut Gc) {
        if !gc.ready() {
            return;
        }

        self.mark_sweep(gc);
    }

    fn mark_sweep(&mut self, gc: &mut Gc) {
        self.cache.trace(gc);
        self.frame.trace(gc);
        self.call_stack.iter().for_each(|frame| frame.trace(gc));
        self.instances
            .iter()
            .for_each(|instance| instance.trace(gc));
        self.stack.iter().for_each(|value| value.trace(gc));
        gc.sweep();
    }

    fn goto(&mut self, pos: u32) {
        self.frame.offset = pos as usize * 8;
    }

    fn load_member(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let object = self.stack.pop();

        if object.is_object() {
            self.load_attr(gc, object, idx as usize)
        } else {
            let member = self.instances[self.frame.instance].consts[idx as usize].str();
            self.load_field(gc, object, member.as_str())
        }
    }

    fn store_member(&mut self, member: u32) -> Result<(), Error> {
        let member = self.instances[self.frame.instance].consts[member as usize].str();
        let value = self.stack.pop();
        let object = self.stack.pop();

        self.check_type(object.ty(), Type::Object)?;

        let mut object = object.object();
        let member = unsafe { member.as_static_str() };

        object.attrs.insert(Cow::Borrowed(member), value);

        Ok(())
    }

    fn load_export(
        &mut self,
        gc: &mut Gc<'gc>,
        object: Handle<'gc, Object<'gc>>,
        member: Handle<'gc, Str<'gc>>,
    ) -> Result<(), Error> {
        let instance_id = object.attrs["instance"].int();
        let instance = &mut self.instances[instance_id as usize];

        if let Some(handle) = instance.get_fn_by_name(gc, member.as_str())? {
            if handle.public {
                self.stack.push(handle.into());
                return Ok(());
            }
        }

        if let Some(handle) = instance.get_class_by_name(gc, member.as_str())? {
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
        member: usize,
    ) -> Result<(), Error> {
        let member = self.instances[self.frame.instance].consts[member].str();
        let object = object.object();

        match object.attrs.get(member.as_str()).copied() {
            Some(value) => {
                self.stack.push(value);
                Ok(())
            }
            None if object.class == self.cache.package_class => {
                self.load_export(gc, object, member)
            }
            None => self.load_method(gc, object, member.as_str()),
        }
    }

    fn load_method(
        &mut self,
        gc: &mut Gc<'gc>,
        mut object: Handle<'gc, Object<'gc>>,
        member: &str,
    ) -> Result<(), Error> {
        let func = object.class.get_method(gc, member)?.ok_or_else(|| {
            ErrorKind::UnknownAttr {
                class_name: object.class.name.clone(),
                attribute: member.to_string(),
            }
            .at(self.frame.span())
        })?;
        let method = Method::new(object.into(), func);
        let handle = gc.alloc(method)?;

        self.stack.push(handle.into());

        Ok(())
    }

    fn load_field(
        &mut self,
        gc: &mut Gc<'gc>,
        object: Value<'gc>,
        member: &str,
    ) -> Result<(), Error> {
        let instance = &mut self.instances[self.frame.instance];

        if let Some(mut class) = instance.get_class_by_name(gc, object.ty().name())? {
            if let Some(func) = class.get_method(gc, member)? {
                let method = Method::new(object, func);
                let handle = gc.alloc(method)?;

                self.stack.push(handle.into());
                return Ok(());
            }
        }

        Err(ErrorKind::UnknownField {
            ty: object.ty(),
            field: member.to_string(),
        }
        .at(self.frame.span())
        .into())
    }

    fn load_class(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let class = self.instances[self.frame.instance].get_class(gc, idx as usize)?;
        self.stack.push(class.into());
        Ok(())
    }

    fn load_fn(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let f = self.instances[self.frame.instance].get_fn(gc, idx as usize)?;
        self.stack.push(f.into());
        Ok(())
    }

    fn return_arg(&mut self, idx: u32) -> Result<(), Error> {
        self.load_arg(idx);
        self.ret();
        Ok(())
    }

    fn load_arg(&mut self, idx: u32) {
        self.stack.push(self.frame.locals[idx as usize]);
    }

    fn check_type(&self, left: Type, right: Type) -> Result<(), Error> {
        if left != right {
            return Err(ErrorKind::TypeMismatch { left, right }
                .at(self.frame.span())
                .into());
        }

        Ok(())
    }

    fn jump_if_false(&mut self, idx: u32) -> Result<(), Error> {
        let value = self.stack.pop();

        if value == Value::FALSE {
            self.goto(idx);
            return Ok(());
        }

        self.check_type(value.ty(), Type::Bool)
    }

    fn jump_cond(&mut self, idx: u32, cond: bool) -> Result<(), Error> {
        let value = self.stack.pop();
        self.check_type(value.ty(), Type::Bool)?;

        if value.bool() == cond {
            self.stack.push(cond.into());
            self.goto(idx);
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
        let name = self.instances[self.frame.instance].consts[idx as usize].str();
        let args = mem::take(&mut self.frame.locals);
        let return_value = self.ffi.call(name.as_ref(), gc, args)?;

        self.frame.returned = true;
        self.stack.push(return_value);
        self.gc_tick(gc);

        Ok(())
    }

    fn call_fn(&mut self, gc: &mut Gc<'gc>, (fn_idx, arg_count): (u16, u16)) -> Result<(), Error> {
        let f = self.instances[self.frame.instance].get_fn(gc, fn_idx as usize)?;
        self.fn_call(gc, f, arg_count as u32)
    }

    fn call(&mut self, gc: &mut Gc<'gc>, arg_count: u32) -> Result<(), Error> {
        let callee = self.stack.pop();

        match callee.ty() {
            Type::Fn => self.fn_call(gc, callee.func(), arg_count),
            Type::Method => self.method_call(gc, callee.method(), arg_count),
            Type::Class => self.init_class(gc, callee.class(), arg_count),
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

        let frame = Frame::new(self.frame.offset, Handle::clone(&method.func));
        self.call_stack.push(mem::replace(&mut self.frame, frame));
        let locals = self.stack.slice_to_end(arg_count as usize);
        self.frame.locals = [&[method.recv], &*locals].concat();
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

        let frame = Frame::new(self.frame.offset, func);
        self.call_stack.push(mem::replace(&mut self.frame, frame));
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
        let array = Array::from_vec(gc, array.to_vec());
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
            self.check_type(from.ty(), Type::Int)?;
        }

        if let Some(to) = to.as_ref() {
            self.check_type(to.ty(), Type::Int)?;
        }

        let array = self.stack.pop();
        self.check_type(array.ty(), Type::Array)?;

        let array = array.array();
        let from = from.map(|v| array_idx(v, array.len())).unwrap_or(0);
        let to = to.map(|v| array_idx(v, array.len())).unwrap_or(array.len());

        if to > array.len() || to < from {
            return Err(ErrorKind::IndexOutOfBounds(to).at(self.frame.span()).into());
        }

        let new_array = gc.alloc(array.slice(from, to))?;

        self.stack.push(new_array.into());
        self.gc_tick(gc);

        Ok(())
    }

    fn prepare_elem(&mut self) -> Result<(Handle<'gc, Array<'gc, Value<'gc>>>, usize), Error> {
        let elem = self.stack.pop();
        let array = self.stack.pop();

        self.check_type(elem.ty(), Type::Int)?;
        self.check_type(array.ty(), Type::Array)?;

        let handle = array.array();
        let idx = array_idx(elem, handle.len());

        Ok((handle, idx))
    }

    fn store_elem(&mut self) -> Result<(), Error> {
        let value = self.stack.pop();
        let (mut array, n) = self.prepare_elem()?;

        match array.get_mut(n) {
            Some(elem) => {
                *elem = value;
                Ok(())
            }
            None => Err(ErrorKind::IndexOutOfBounds(n).at(self.frame.span()).into()),
        }
    }

    fn load_elem(&mut self) -> Result<(), Error> {
        let (array, n) = self.prepare_elem()?;

        match array.get(n).copied() {
            Some(elem) => {
                self.stack.push(elem);
                Ok(())
            }
            None => Err(ErrorKind::IndexOutOfBounds(n).at(self.frame.span()).into()),
        }
    }

    fn import(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let name = self.instances[self.frame.instance].consts[idx as usize].str();

        if let Some(handle) = self.cache.packages.get(name.as_str()) {
            self.stack.push(Handle::clone(handle).into());
            return Ok(());
        }

        let instance_id = self.instances.len();
        let filename = self
            .search_path
            .join("std")
            .join(format!("{}.atom", name.as_str()));
        let module = utils::compile(&filename, true).map_err(|e| Error::Import(Box::new(e)))?;
        let (mut instance, frame) = root_frame(instance_id, gc, module)?;

        let class = Handle::clone(&self.cache.package_class);
        let mut object = Object::new(class);
        object
            .attrs
            .insert(Cow::Borrowed("instance"), instance_id.into_atom(gc)?);

        let handle = gc.alloc(object)?;

        // Insert the package object as the first variable (which is `self`)
        instance.vars.insert(0, Handle::clone(&handle).into());

        self.instances.push(instance);
        self.cache
            .packages
            .insert(name.to_string(), Handle::clone(&handle));
        self.stack.push(handle.into());
        self.call_stack.push(mem::replace(&mut self.frame, frame));
        self.gc_tick(gc);

        Ok(())
    }

    fn not(&mut self) -> Result<(), Error> {
        let value = self.stack.pop();
        self.check_type(value.ty(), Type::Bool)?;
        self.stack.push((!value.bool()).into());
        Ok(())
    }

    fn push_int(&mut self, gc: &mut Gc<'gc>, i: i64) -> Result<(), Error> {
        if i.unsigned_abs() > value::INT_MASK {
            let handle = gc.alloc(i)?;
            self.stack.push(handle.into());
        } else {
            self.stack.push(Value::new_smallint(i));
        }

        Ok(())
    }

    fn eq_op(&mut self) -> Result<bool, Error> {
        let (lhs, rhs) = self.stack.operands();

        if lhs == rhs {
            return Ok(true);
        }

        let ty = lhs.ty();

        if ty != rhs.ty() {
            return Err(self.unsupported_rhs(lhs, rhs, "=="));
        }

        Ok(match ty {
            Type::Int => lhs.int() == rhs.int(),
            Type::Float => lhs.float() == rhs.float(),
            Type::Str => lhs.str() == rhs.str(),
            Type::Bool => lhs.bool() == rhs.bool(),
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

    fn bitwise_and(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();
        if lhs.is_int() && rhs.is_int() {
            self.push_int(gc, lhs.int() & rhs.int())
        } else {
            Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty()))
        }
    }

    fn bitwise_or(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();
        if lhs.is_int() && rhs.is_int() {
            self.push_int(gc, lhs.int() | rhs.int())
        } else {
            Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty()))
        }
    }

    fn bitwise_xor(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();

        if lhs.is_int() && rhs.is_int() {
            self.push_int(gc, lhs.int() ^ rhs.int())
        } else if matches!(lhs.ty(), Type::Array | Type::Str) && lhs.ty() == rhs.ty() {
            self.stack.push(rhs);
            self.load_field(gc, lhs, "concat")?;
            self.call(gc, 1)?;

            Ok(())
        } else {
            Err(self.unsupported("^", lhs.ty(), rhs.ty()))
        }
    }

    fn store(&mut self, idx: u32) -> Result<(), Error> {
        let var = self.stack.pop();
        self.instances[self.frame.instance].vars.insert(idx, var);
        Ok(())
    }

    fn load(&mut self, idx: u32) {
        self.stack
            .push(self.instances[self.frame.instance].vars[&idx]);
    }

    fn load_const(&mut self, idx: u32) {
        self.stack
            .push(self.instances[self.frame.instance].consts[idx as usize]);
    }

    fn discard(&mut self) {
        let _ = self.stack.pop();
    }

    fn ret(&mut self) {
        self.frame.returned = true;
        self.frame.offset = self.frame.handle.body.len();
    }

    fn eval(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        while let Some(bc) = self.frame.next() {
            #[cfg(feature = "profiler")]
            self.profiler.enter_instruction(bc.op);

            match bc.op {
                Op::Add => self.add(gc)?,
                Op::Sub => self.sub(gc)?,
                Op::Mul => self.mul(gc)?,
                Op::Div => self.div(gc)?,
                Op::Rem => self.rem(gc)?,
                Op::Eq => self.eq()?,
                Op::Ne => self.ne()?,
                Op::Lt => self.lt(gc)?,
                Op::Lte => self.lte(gc)?,
                Op::Gt => self.gt(gc)?,
                Op::Gte => self.gte(gc)?,
                Op::BitwiseAnd => self.bitwise_and(gc)?,
                Op::BitwiseOr => self.bitwise_or(gc)?,
                Op::BitwiseXor => self.bitwise_xor(gc)?,
                Op::LoadConst => self.load_const(bc.code),
                Op::LoadMember => self.load_member(gc, bc.code)?,
                Op::StoreMember => self.store_member(bc.code)?,
                Op::LoadFn => self.load_fn(gc, bc.code)?,
                Op::LoadClass => self.load_class(gc, bc.code)?,
                Op::Store => self.store(bc.code)?,
                Op::Load => self.load(bc.code),
                Op::LoadArg => self.load_arg(bc.code),
                Op::Discard => self.discard(),
                Op::Return => self.ret(),
                Op::ReturnArg => self.return_arg(bc.code)?,
                Op::Call => self.call(gc, bc.code)?,
                Op::CallFn => self.call_fn(gc, bc.code2())?,
                Op::CallExtern => self.call_extern(gc, bc.code)?,
                Op::TailCall => self.tail_call(gc, bc.code)?,
                Op::Jump => self.goto(bc.code),
                Op::JumpIfFalse => self.jump_if_false(bc.code)?,
                Op::PushJumpIfTrue => self.jump_cond(bc.code, true)?,
                Op::PushJumpIfFalse => self.jump_cond(bc.code, false)?,
                Op::MakeArray => self.make_array(gc, bc.code)?,
                Op::MakeSlice => self.make_slice(gc, bc.code)?,
                Op::LoadElement => self.load_elem()?,
                Op::StoreElement => self.store_elem()?,
                Op::UnaryNot => self.not()?,
                Op::Import => self.import(gc, bc.code)?,
                Op::Nop => unreachable!(),
            }

            #[cfg(feature = "profiler")]
            self.profiler.record_instruction(bc.op);
        }

        Ok(())
    }

    fn add_trace(&mut self, e: Error) -> Error {
        match e {
            Error::Runtime(mut e) => {
                let call_stack = mem::take(&mut self.call_stack);
                let mut stack_trace = call_stack
                    .into_iter()
                    .map(|frame| {
                        Call::new(
                            frame.span_at(frame.call_site),
                            if frame.global {
                                None
                            } else {
                                Some(frame.handle.name.clone())
                            },
                        )
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

    pub fn run(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        let mut start = || {
            #[cfg(feature = "profiler")]
            self.profiler.enter();

            loop {
                self.eval(gc)?;

                match self.call_stack.pop() {
                    Some(frame) => {
                        if !self.frame.returned && !self.frame.global {
                            self.stack.push(Value::NIL);
                        }

                        self.frame = frame;
                    }
                    None => break,
                }
            }

            Ok(())
        };

        start().map_err(|e| self.add_trace(e))
    }
}

impl_binary!(
    (lt: <),
    (lte: <=),
    (gt: >),
    (gte: >=),
    (add: +),
    (sub: -),
    (mul: *),
    (div: /),
    (rem: %)
);
