use std::{
    borrow::Cow,
    collections::HashMap,
    fs, mem,
    path::{Path, PathBuf},
};

use wyhash2::WyHash;

use crate::{
    ast::Stmt,
    bytecode::Op,
    compiler::{Compiler, Package},
    error::SpannedError,
    frame::Frame,
    gc::{Gc, Handle, Trace},
    instance::Instance,
    lexer::Lexer,
    parser::{self},
    runtime::{
        array::Array,
        class::{Class, Object},
        error::{Call, ErrorKind, RuntimeError},
        function::{Fn, Method},
        str::Str,
        value::{IntoAtom, Type, Value},
    },
    stack::Stack,
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

pub fn compile(source: impl AsRef<Path>, optimize: bool) -> Result<Package, crate::error::Error> {
    let source = fs::read_to_string(source)?;
    let program = parse(&source)?;
    let compiler = Compiler::default().with_optimize(optimize);

    Ok(compiler.compile(program)?)
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

pub struct Vm<'gc, F: Ffi<'gc>, const S: usize> {
    ffi: F,
    frame: Frame<'gc>,
    search_path: PathBuf,
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
        let (instance, frame) = instance_with_frame(0, gc, package)?;

        Ok(Self {
            ffi,
            frame,
            search_path,
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
            None if object.class.name == "Package" => self.load_export(gc, object, member),
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

        let method = Method::new(Handle::clone(&object).into(), func);
        let handle = gc.alloc(method)?;

        object
            .attrs
            .insert(member.to_string().into(), Handle::clone(&handle).into());
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

        self.set_frame(Frame::new(self.frame.offset, func));
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

    fn make_path(&self, name: &str) -> PathBuf {
        self.search_path
            .join("stdlib")
            .join(format!("{}.atom", name))
    }

    fn set_frame(&mut self, frame: Frame<'gc>) {
        self.call_stack.push(mem::replace(&mut self.frame, frame));
    }

    fn import(&mut self, gc: &mut Gc<'gc>, idx: u32) -> Result<(), Error> {
        let name = self.instances[self.frame.instance].consts[idx as usize].str();

        if let Some(handle) = self.packages.get(name.as_str()) {
            self.stack.push(Handle::clone(handle).into());
            return Ok(());
        }

        let id = self.instances.len();

        // Parse and compile module
        let module =
            compile(self.make_path(name.as_str()), true).map_err(|e| Error::Import(Box::new(e)))?;

        // Initialize instance and frame
        let (mut instance, frame) = instance_with_frame(id, gc, module)?;

        // Setup package class
        let class = gc.alloc(Class::new("Package", self.frame.instance))?;
        let object = Object::with_attr(gc, class, vec![("instance", id)])?;
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

        Ok(())
    }

    fn not(&mut self) -> Result<(), Error> {
        let value = self.stack.pop();
        self.check_type(value.ty(), Type::Bool)?;
        self.stack.push((!value.bool()).into());
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
            self.stack.push((lhs.int() & rhs.int()).into_atom(gc)?);
            Ok(())
        } else {
            Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty()))
        }
    }

    fn bitwise_or(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();
        if lhs.is_int() && rhs.is_int() {
            self.stack.push((lhs.int() | rhs.int()).into_atom(gc)?);
            Ok(())
        } else {
            Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty()))
        }
    }

    fn concat(&mut self, gc: &mut Gc<'gc>, lhs: Value<'gc>, rhs: Value<'gc>) -> Result<(), Error> {
        let value = match lhs.ty() {
            Type::Array => {
                let lhs = lhs.array();
                let rhs = rhs.array();

                [lhs.as_slice(), rhs.as_slice()].concat().into_atom(gc)
            }
            Type::Str => {
                let lhs = lhs.str();
                let rhs = rhs.str();

                [lhs.as_str(), rhs.as_str()].concat().into_atom(gc)
            }
            _ => unreachable!(),
        }?;

        self.stack.push(value);
        Ok(())
    }

    fn bitwise_xor(&mut self, gc: &mut Gc<'gc>) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();

        if lhs.is_int() && rhs.is_int() {
            self.stack.push((lhs.int() ^ rhs.int()).into_atom(gc)?);
            Ok(())
        } else if matches!(lhs.ty(), Type::Array | Type::Str) && lhs.ty() == rhs.ty() {
            self.concat(gc, lhs, rhs)
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
