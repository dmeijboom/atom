use std::{collections::HashMap, mem, rc::Rc, time::Duration};

#[cfg(feature = "timings")]
use std::time::Instant;

#[cfg(feature = "tracing")]
use tracing::{instrument, Level};

use crate::{
    collections::{ReuseVec, Stack},
    error::{IntoSpanned, SpannedError},
    gc::{Gc, Handle},
    lexer::Span,
    opcode::{Const, Op, Opcode},
    runtime::{
        array::Array,
        class::{Class, Instance},
        error::{Call, ErrorKind, RuntimeError},
        func::Func,
        module::Module,
        str::Str,
        value::{TryIntoValue, Type, Value},
        Atom,
    },
};

macro_rules! unwrap {
    (Int, $expr:expr) => {
        $expr.int()
    };

    (Float, $expr:expr) => {
        $expr.float()
    };

    (Bool, $expr:expr) => {
        $expr.bool()
    };
}

macro_rules! binary {
    ($self:ident: $($ty:ident)|+, $lhs:expr, $op:tt, $rhs:expr) => {{
        let value = match $lhs.ty() {
            $(Type::$ty => (unwrap!($ty, $lhs) $op unwrap!($ty, $rhs)).into_value(&mut $self.gc)?),+
            , _ => return Err(ErrorKind::UnsupportedOp {
                left: $lhs.ty(),
                right: $rhs.ty(),
                op: stringify!($op),
            }
            .at($self.span)
            .into()),
        };

        $self.push(value)?;

        Ok(())
    }};

    ($self:ident: $($ty:ident)|+, lhs $op:tt rhs) => {{
        let rhs = $self.stack.pop();
        let lhs = $self.stack.pop();

        $self.check_type(lhs.ty(), rhs.ty())?;
        binary!($self: $($ty)|+, lhs, $op, rhs)
    }};

    ($self:ident: lhs $op:tt rhs) => {
        binary!($self: Int | Float | Bool, lhs $op rhs)
    }
}

fn resize<T: Default + Clone>(vec: &mut Vec<T>, len: usize) {
    if vec.len() != len {
        vec.resize_with(len, || T::default());
    }
}

fn array_idx(elem: Value, len: usize) -> usize {
    match elem.int() {
        n if n < 0 => (len as i64 + n) as usize,
        n => n as usize,
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FatalErrorKind {
    #[error("invalid var at '{0}'")]
    InvalidVar(usize),
    #[error("invalid argument at '{0}'")]
    InvalidArg(usize),
    #[error("invalid fn at '{0}'")]
    InvalidFn(usize),
    #[error("invalid class at '{0}'")]
    InvalidClass(usize),
    #[error("invalid extern function '{0}'")]
    InvalidExternFn(String),
    #[error("maximum stack exceeded")]
    MaxStackExceeded,
}

pub type FatalError = SpannedError<FatalErrorKind>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("RuntimeError: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("FatalError: {0}")]
    Fatal(#[from] FatalError),
}

#[derive(Debug, Default)]
struct Frame {
    call: Call,
    // Locals (in reversed order)
    locals: Vec<Value>,
    return_pos: usize,
}

fn top_frame(module: &Module) -> Frame {
    Frame {
        call: Call::new(
            Span::default(),
            Rc::new(Func {
                codes: Rc::clone(&module.codes),
                ..Func::default()
            }),
        ),
        ..Frame::default()
    }
}

fn map_const(gc: &mut Gc, const_: Const) -> Result<Value, Error> {
    Ok(match const_ {
        Const::Nil => Value::NIL,
        Const::Int(n) => n.into_value(gc)?,
        Const::Float(n) => Value::from(n),
        Const::Bool(b) => Value::from(b),
        Const::Str(s) => {
            let str = Str::from_string(gc, s);
            Value::from(gc.alloc(str)?)
        }
    })
}

pub type BoxedFn = Rc<Box<dyn Fn(Atom, Vec<Value>) -> Result<Value, RuntimeError> + 'static>>;

pub trait DynamicLinker {
    fn resolve(&self, name: &str) -> Option<BoxedFn>;
}

#[derive(Default)]
pub struct Timing {
    pub count: u32,
    pub elapsed: Duration,
}

impl Timing {
    pub fn avg(&self) -> Duration {
        self.elapsed / self.count
    }
}

pub struct Vm<L: DynamicLinker, const S: usize, const C: usize> {
    gc: Gc,
    span: Span,
    pos: usize,
    linker: L,
    module: Module,
    returned: bool,
    vars: Vec<Value>,
    consts: [Value; C],
    codes: Rc<[Opcode]>,
    stack: Stack<Value, S>,
    call_stack: ReuseVec<Frame>,
    timing: HashMap<Op, Timing>,
}

impl<L: DynamicLinker, const S: usize, const C: usize> Drop for Vm<L, S, C> {
    fn drop(&mut self) {
        self.gc.sweep();
    }
}

impl<L: DynamicLinker, const S: usize, const C: usize> Vm<L, S, C> {
    pub fn new(mut module: Module, linker: L) -> Result<Self, Error> {
        let mut gc = Gc::default();
        let mut consts = [Value::NIL; C];

        for (i, const_) in mem::take(&mut module.consts).into_iter().enumerate() {
            consts[i] = map_const(&mut gc, const_)?;
        }

        let mut call_stack = ReuseVec::default();
        call_stack.push(top_frame(&module));

        Ok(Self {
            vars: vec![],
            consts,
            linker,
            returned: false,
            gc,
            pos: 0,
            span: Span::default(),
            call_stack,
            timing: HashMap::default(),
            stack: Stack::default(),
            codes: Rc::clone(&module.codes),
            module,
        })
    }

    pub fn timing(&self) -> &HashMap<Op, Timing> {
        &self.timing
    }

    fn try_mark_sweep(&mut self) {
        if !self.gc.ready() {
            return;
        }

        self.vars
            .iter()
            .chain(self.consts.iter())
            .chain(self.stack.iter())
            .chain(self.call_stack.iter().flat_map(|frame| frame.locals.iter()))
            .for_each(|value| {
                if let Some(handle) = value.handle() {
                    self.gc.mark(handle);
                }
            });

        self.gc.sweep();
    }

    fn push(&mut self, value: impl Into<Value>) -> Result<(), Error> {
        if self.stack.is_full() {
            return Err(FatalErrorKind::MaxStackExceeded.at(self.span).into());
        }

        self.stack.push(value.into());
        Ok(())
    }

    fn push_tail_call(&mut self, arg_count: usize) -> Result<(), Error> {
        let frame = self.call_stack.last_mut();
        frame.locals.resize(arg_count, Value::NIL);

        self.returned = false;
        self.pos = 0;

        Ok(())
    }

    fn push_call(&mut self, func: Rc<Func>, arg_count: usize) -> Result<(), Error> {
        let return_pos = self.pos;

        self.pos = 0;
        self.codes = Rc::clone(&func.codes);

        // Check if we can re-use the current frame
        if let Some(frame) = self.call_stack.push_and_reuse() {
            frame.call = Call::new(self.span, func);
            frame.return_pos = return_pos;
            resize(&mut frame.locals, arg_count);
        } else {
            let mut locals = Vec::with_capacity(arg_count);
            resize(&mut locals, arg_count);

            self.call_stack.push(Frame {
                call: Call::new(self.span, func),
                locals,
                return_pos,
            });
        }

        Ok(())
    }

    fn goto(&mut self, n: usize) {
        self.pos = n;

        if let Some(code) = self.codes.get(self.pos) {
            self.span = code.span;
        }
    }

    fn const_name(&self, idx: usize) -> Result<Handle<Str>, Error> {
        let value = self.consts[idx];

        match value.ty() {
            Type::Str => Ok(value.str()),
            _ => unreachable!(),
        }
    }

    fn load_member(&mut self, idx: usize) -> Result<(), Error> {
        let object = self.stack.pop();

        match object.ty() {
            Type::Instance => self.load_attr(object, idx),
            _ => self.load_field(object, idx),
        }
    }

    fn store_member(&mut self, member: usize) -> Result<(), Error> {
        let member = self.const_name(member)?.as_ref().to_string();
        let value = self.stack.pop();
        let object = self.stack.pop();

        self.check_type(object.ty(), Type::Instance)?;

        let mut instance = object.instance();
        instance.attrs.insert(member, value);

        Ok(())
    }

    fn load_attr(&mut self, object: Value, member: usize) -> Result<(), Error> {
        let member = self.const_name(member)?;
        let member = member.as_str();
        let instance = object.instance();

        match instance.attrs.get(member).copied() {
            Some(value) => self.push(value),
            None => match instance.class.methods.get(member) {
                Some(method) => {
                    let method = Rc::clone(method);
                    self.push(object)?;
                    self.push(method)?;
                    Ok(())
                }
                None => Err(ErrorKind::UnknownAttr {
                    class: Rc::clone(&instance.class),
                    attribute: member.to_string(),
                }
                .at(self.span)
                .into()),
            },
        }
    }

    fn load_field(&mut self, object: Value, member: usize) -> Result<(), Error> {
        let member_handle = self.const_name(member)?;
        let member = member_handle.as_str();
        let class = self
            .module
            .classes
            .iter()
            .find(|class| class.name == object.ty().name());

        if let Some(method) = class
            .and_then(|class| class.methods.get(member))
            .map(Rc::clone)
        {
            self.push(object)?;
            self.push(method)?;
            return Ok(());
        }

        Err(ErrorKind::UnknownField {
            ty: object.ty(),
            field: member.to_string(),
        }
        .at(self.span)
        .into())
    }

    fn get_func(&mut self, idx: usize) -> Result<Rc<Func>, Error> {
        Ok(self
            .module
            .funcs
            .get(idx)
            .map(Rc::clone)
            .ok_or_else(|| FatalErrorKind::InvalidFn(idx).at(self.span))?)
    }

    fn get_class(&mut self, idx: usize) -> Result<Rc<Class>, Error> {
        Ok(self
            .module
            .classes
            .get(idx)
            .map(Rc::clone)
            .ok_or_else(|| FatalErrorKind::InvalidClass(idx).at(self.span))?)
    }

    fn load_class(&mut self, idx: usize) -> Result<(), Error> {
        let class = self.get_class(idx)?;
        self.push(class)
    }

    fn load_fn(&mut self, idx: usize) -> Result<(), Error> {
        let func = self.get_func(idx)?;
        self.push(func)
    }

    fn load_var(&self, idx: usize) -> Result<Value, Error> {
        match self.vars.get(idx).copied() {
            Some(value) => Ok(value),
            None => Err(FatalErrorKind::InvalidVar(idx).at(self.span).into()),
        }
    }

    fn load_arg(&mut self, idx: usize) -> Result<(), Error> {
        let frame = self.call_stack.last();

        match frame.locals.get(frame.locals.len() - idx - 1).copied() {
            Some(value) => self.push(value),
            None => Err(FatalErrorKind::InvalidArg(idx).at(self.span).into()),
        }
    }

    fn check_type(&self, left: Type, right: Type) -> Result<(), Error> {
        if left != right {
            return Err(ErrorKind::TypeMismatch { left, right }.at(self.span).into());
        }

        Ok(())
    }

    fn jump_cond(&mut self, idx: usize, push: bool, cond: bool) -> Result<(), Error> {
        let value = self.stack.pop();
        self.check_type(value.ty(), Type::Bool)?;

        if value.bool() == cond {
            if push {
                self.push(cond)?;
            }

            self.goto(idx);
        }

        Ok(())
    }

    fn init_class(&mut self, class: Rc<Class>, arg_count: usize) -> Result<(), Error> {
        let init_fn = class.methods.get("init").map(Rc::clone);
        let instance = Instance::new(class);
        let handle = self.gc.alloc(instance)?;

        if let Some(func) = init_fn {
            self.push(handle)?;
            self.push(func)?;
            self.call(arg_count, false)
        } else {
            self.push(handle)
        }
    }

    fn call_extern(&mut self, idx: usize) -> Result<(), Error> {
        let name = self.const_name(idx)?;
        let name = name.as_str();
        let handler = self
            .linker
            .resolve(name)
            .ok_or_else(|| FatalErrorKind::InvalidExternFn(name.to_string()).at(self.span))?;
        let frame = self.call_stack.last_mut();
        let args = mem::take(&mut frame.locals);
        let ctx = Atom::with_span(&mut self.gc, self.span);
        let return_value = (handler)(ctx, args)?;

        self.returned = true;
        self.push(return_value)
    }

    fn call_fn(&mut self, (fn_idx, arg_count): (u32, u32), tail_call: bool) -> Result<(), Error> {
        let func = self.get_func(fn_idx as usize)?;
        self.fn_call(func, arg_count as usize, tail_call)
    }

    #[inline(always)]
    fn call(&mut self, arg_count: usize, tail_call: bool) -> Result<(), Error> {
        let callee = self.stack.pop();

        match callee.ty() {
            Type::Fn => self.fn_call(callee.func(), arg_count, tail_call),
            Type::Class => self.init_class(callee.class(), arg_count),
            ty => Err(ErrorKind::NotCallable(ty).at(self.span).into()),
        }
    }

    fn fn_call(&mut self, func: Rc<Func>, arg_count: usize, tail_call: bool) -> Result<(), Error> {
        if arg_count != func.arg_count {
            return Err(ErrorKind::ArgCountMismatch { func, arg_count }
                .at(self.span)
                .into());
        }

        let offset = func.receiver as usize;

        if tail_call {
            self.push_tail_call(arg_count + offset)?;
        } else {
            self.push_call(func, arg_count + offset)?;
        }

        let frame = self.call_stack.last_mut();

        if offset == 1 {
            frame.locals[arg_count] = self.stack.pop();
        }

        for i in 0..arg_count {
            frame.locals[i] = self.stack.pop();
        }

        Ok(())
    }

    fn concat(&mut self, lhs: Value, rhs: Value) -> Result<Value, Error> {
        let ty = lhs.ty();
        let value = match ty {
            Type::Array => {
                let lhs = lhs.array();
                let rhs = rhs.array();
                let new_data = lhs.concat(&rhs);
                let array = Array::from_vec(&mut self.gc, new_data);
                Value::from(self.gc.alloc(array)?)
            }
            Type::Str => {
                let lhs = lhs.str();
                let rhs = rhs.str();
                let new_data = lhs.0.concat(&rhs.0);
                let array = Array::from_vec(&mut self.gc, new_data);
                Value::from(self.gc.alloc(Str(array))?)
            }
            _ => unreachable!(),
        };

        Ok(value)
    }

    fn make_array(&mut self, size: usize) -> Result<(), Error> {
        let mut values = vec![];

        for _ in 0..size {
            values.push(self.stack.pop());
        }

        values.reverse();

        let array = Array::from_vec(&mut self.gc, values);
        let handle = self.gc.alloc(array)?;

        self.push(handle)?;

        Ok(())
    }

    fn make_slice(&mut self, opts: usize) -> Result<(), Error> {
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
            return Err(ErrorKind::IndexOutOfBounds(to).at(self.span).into());
        }

        let new_array = self.gc.alloc(array.slice(from, to))?;
        self.push(new_array)?;

        Ok(())
    }

    fn prepare_elem(&mut self) -> Result<(Handle<Array<Value>>, usize), Error> {
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
            None => Err(ErrorKind::IndexOutOfBounds(n).at(self.span).into()),
        }
    }

    fn load_elem(&mut self) -> Result<(), Error> {
        let (array, n) = self.prepare_elem()?;

        match array.get(n).copied() {
            Some(elem) => self.push(elem),
            None => Err(ErrorKind::IndexOutOfBounds(n).at(self.span).into()),
        }
    }

    fn not(&mut self) -> Result<(), Error> {
        let value = self.stack.pop();
        self.check_type(value.ty(), Type::Bool)?;
        self.push(!value.bool())
    }

    fn eq(&mut self) -> Result<(), Error> {
        binary!(self: lhs == rhs)
    }

    fn ne(&mut self) -> Result<(), Error> {
        binary!(self: lhs != rhs)
    }

    fn lt(&mut self) -> Result<(), Error> {
        binary!(self: lhs < rhs)
    }

    fn lte(&mut self) -> Result<(), Error> {
        binary!(self: lhs <= rhs)
    }

    fn gt(&mut self) -> Result<(), Error> {
        binary!(self: lhs > rhs)
    }

    fn gte(&mut self) -> Result<(), Error> {
        binary!(self: lhs >= rhs)
    }

    fn add(&mut self) -> Result<(), Error> {
        binary!(self: Int | Float, lhs + rhs)
    }

    fn sub(&mut self) -> Result<(), Error> {
        binary!(self: Int | Float, lhs - rhs)
    }

    fn mul(&mut self) -> Result<(), Error> {
        binary!(self: Int | Float, lhs * rhs)
    }

    fn div(&mut self) -> Result<(), Error> {
        binary!(self: Int | Float, lhs / rhs)
    }

    fn rem(&mut self) -> Result<(), Error> {
        binary!(self: Int | Float, lhs % rhs)
    }

    fn bitwise_and(&mut self) -> Result<(), Error> {
        binary!(self: Int, lhs & rhs)
    }

    fn bitwise_or(&mut self) -> Result<(), Error> {
        binary!(self: Int, lhs | rhs)
    }

    fn bitwise_xor(&mut self) -> Result<(), Error> {
        let rhs = self.stack.pop();
        let lhs = self.stack.pop();

        self.check_type(lhs.ty(), rhs.ty())?;

        if matches!(lhs.ty(), Type::Array | Type::Str) {
            let value = self.concat(lhs, rhs)?;
            self.push(value)?;

            return Ok(());
        }

        binary!(self: Int, lhs, ^, rhs)
    }

    fn store(&mut self, idx: usize) -> Result<(), Error> {
        if self.vars.len() <= idx {
            resize(&mut self.vars, idx + 1);
        }

        self.vars[idx] = self.stack.pop();
        Ok(())
    }

    fn load(&mut self, idx: usize) -> Result<(), Error> {
        let value = self.load_var(idx)?;
        self.push(value)
    }

    fn load_const(&mut self, idx: usize) -> Result<(), Error> {
        self.push(self.consts[idx])
    }

    fn discard(&mut self) {
        self.stack.pop();
    }

    fn ret(&mut self) -> Result<(), Error> {
        self.returned = true;
        self.pos = self.codes.len();

        Ok(())
    }

    #[inline(always)]
    #[cfg_attr(feature = "tracing", instrument(level = Level::DEBUG, skip(self), ret(Debug)))]
    fn eval_loop(&mut self) -> Result<(), Error> {
        while self.pos < self.codes.len() {
            let opcode = &self.codes[self.pos];

            self.pos += 1;
            self.span = opcode.span;

            #[cfg(feature = "timings")]
            let (op, now) = (opcode.op(), Instant::now());

            match opcode.op() {
                Op::Add => self.add()?,
                Op::Sub => self.sub()?,
                Op::Mul => self.mul()?,
                Op::Div => self.div()?,
                Op::Rem => self.rem()?,
                Op::Eq => self.eq()?,
                Op::Ne => self.ne()?,
                Op::Lt => self.lt()?,
                Op::Lte => self.lte()?,
                Op::Gt => self.gt()?,
                Op::Gte => self.gte()?,
                Op::BitwiseAnd => self.bitwise_and()?,
                Op::BitwiseOr => self.bitwise_or()?,
                Op::BitwiseXor => self.bitwise_xor()?,
                Op::LoadConst => self.load_const(opcode.code())?,
                Op::LoadMember => self.load_member(opcode.code())?,
                Op::StoreMember => self.store_member(opcode.code())?,
                Op::LoadFn => self.load_fn(opcode.code())?,
                Op::LoadClass => self.load_class(opcode.code())?,
                Op::Store => self.store(opcode.code())?,
                Op::Load => self.load(opcode.code())?,
                Op::LoadArg => self.load_arg(opcode.code())?,
                Op::Discard => self.discard(),
                Op::Return => self.ret()?,
                Op::CallFn => self.call_fn(opcode.code2(), false)?,
                Op::Call => self.call(opcode.code(), false)?,
                Op::CallExtern => self.call_extern(opcode.code())?,
                Op::TailCall => self.call(opcode.code(), true)?,
                Op::TailCallFn => self.call_fn(opcode.code2(), true)?,
                Op::Jump => self.goto(opcode.code()),
                Op::JumpIfFalse => self.jump_cond(opcode.code(), false, false)?,
                Op::PushJumpIfTrue => self.jump_cond(opcode.code(), true, true)?,
                Op::PushJumpIfFalse => self.jump_cond(opcode.code(), true, false)?,
                Op::MakeArray => self.make_array(opcode.code())?,
                Op::MakeSlice => self.make_slice(opcode.code())?,
                Op::LoadElement => self.load_elem()?,
                Op::StoreElement => self.store_elem()?,
                Op::UnaryNot => self.not()?,
            }

            self.try_mark_sweep();

            #[cfg(feature = "timings")]
            {
                let entry = self.timing.entry(op).or_insert(Timing::default());
                entry.elapsed += now.elapsed();
                entry.count += 1;
            }
        }

        Ok(())
    }

    fn add_trace(&mut self, e: Error) -> Error {
        match e {
            Error::Runtime(mut e) => {
                let call_stack = mem::take(&mut self.call_stack);
                e.trace = Some(
                    call_stack
                        .into_iter()
                        .skip(1)
                        .rev()
                        .map(|s| s.call)
                        .collect::<Vec<_>>(),
                );
                Error::Runtime(e)
            }
            Error::Fatal(e) => Error::Fatal(e),
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        let mut start = || {
            loop {
                self.eval_loop()?;

                if let Some(frame) = self.call_stack.pop() {
                    if !self.returned {
                        self.stack.push(Value::NIL);
                    }

                    self.returned = false;
                    self.pos = frame.return_pos;

                    if !self.call_stack.is_empty() {
                        self.codes = Rc::clone(&self.call_stack.last().call.func.codes);
                        continue;
                    }
                }

                break;
            }

            Ok(())
        };

        start().map_err(|e| self.add_trace(e))
    }
}
