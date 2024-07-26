use std::{
    mem::{self, size_of},
    rc::Rc,
};

#[cfg(feature = "tracing")]
use tracing::{instrument, Level};

use crate::{
    collections::{ReuseVec, Stack},
    compiler::Module,
    error::{IntoSpanned, SpannedError},
    gc::Gc,
    lexer::Span,
    opcode::{Const, Op, Opcode},
    runtime::{
        class::{Class, Instance},
        error::{Call, ErrorKind, RuntimeError},
        function::{Exec, Func},
        std::{array::Array, str::Str, Context, StdLib},
        value::{Type, Value},
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
        $self.push(match $lhs.ty() {
            $(Type::$ty => Value::from((unwrap!($ty, $lhs) $op unwrap!($ty, $rhs)))),+
            , _ => return Err(ErrorKind::UnsupportedOp {
                left: $lhs.ty(),
                right: $rhs.ty(),
                op: stringify!($op),
            }
            .at($self.span)
            .into()),
        })?;

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

#[derive(Debug, thiserror::Error)]
pub enum FatalErrorKind {
    #[error("invalid var at: {0}")]
    InvalidVar(usize),
    #[error("invalid argument at: {0}")]
    InvalidArg(usize),
    #[error("invalid fn at: {0}")]
    InvalidFn(usize),
    #[error("invalid class at: {0}")]
    InvalidClass(usize),
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

const MAX_STACK_SIZE: usize = 250000 / size_of::<Value>();
const MAX_CONST_SIZE: usize = 100000 / size_of::<Value>();

fn top_frame(module: &Module) -> Frame {
    Frame {
        call: Call::new(
            Span::default(),
            Rc::new(Func {
                exec: Exec::Vm(Rc::clone(&module.codes)),
                ..Func::default()
            }),
        ),
        ..Frame::default()
    }
}

fn map_const(gc: &mut Gc, const_: Const) -> Result<Value, Error> {
    Ok(match const_ {
        Const::Int(n) => Value::from(n),
        Const::Float(n) => Value::from(n),
        Const::Bool(b) => Value::from(b),
        Const::Str(s) => {
            let str = Str::from_string(gc, s);
            Value::from(gc.alloc(str)?)
        }
    })
}

pub struct Vm<'a> {
    gc: Gc,
    span: Span,
    pos: usize,
    std: &'a StdLib,
    module: Module,
    returned: bool,
    vars: Vec<Value>,
    codes: Rc<[Opcode]>,
    call_stack: ReuseVec<Frame>,
    consts: [Value; MAX_CONST_SIZE],
    stack: Stack<Value, MAX_STACK_SIZE>,
}

impl<'a> Vm<'a> {
    pub fn new(std: &'a StdLib, mut module: Module) -> Result<Self, Error> {
        let mut gc = Gc::default();
        let mut consts = [Value::NIL; MAX_CONST_SIZE];

        for (i, const_) in mem::take(&mut module.consts).into_iter().enumerate() {
            consts[i] = map_const(&mut gc, const_)?;
        }

        let mut call_stack = ReuseVec::default();
        call_stack.push(top_frame(&module));

        Ok(Self {
            std,
            vars: vec![],
            consts,
            returned: false,
            gc,
            pos: 0,
            span: Span::default(),
            call_stack,
            stack: Stack::default(),
            codes: Rc::clone(&module.codes),
            module,
        })
    }

    fn try_collect_gc(&mut self) {
        if !self.gc.should_run() {
            return;
        }

        let mut try_mark = |value: &Value| {
            if let Some(handle) = value.handle() {
                self.gc.mark(handle);
            }
        };

        for const_ in self.consts.iter() {
            try_mark(const_);
        }

        for item in self.stack.iter() {
            try_mark(item);
        }

        for frame in self.call_stack.iter() {
            for local in frame.locals.iter() {
                try_mark(local);
            }
        }

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
        self.codes = match func.exec {
            Exec::Vm(ref codes) => Rc::clone(codes),
            _ => unreachable!(),
        };

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

    fn const_name(&self, idx: usize) -> Result<&str, Error> {
        let value = self.consts[idx];

        match value.ty() {
            Type::Str => {
                let str = self.gc.get(value.str());
                Ok(str.as_str())
            }
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
        let member = self.const_name(member)?.to_string();
        let value = self.stack.pop();
        let object = self.stack.pop();

        self.check_type(object.ty(), Type::Instance)?;

        let instance = self.gc.get_mut(object.instance());
        instance.attrs.insert(member, value);

        Ok(())
    }

    fn load_attr(&mut self, object: Value, member: usize) -> Result<(), Error> {
        let member = self.const_name(member)?;
        let instance = self.gc.get(object.instance());

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
        let member = self.const_name(member)?;
        let ty = self.std.types.get(&object.ty());

        if let Some(ty) = ty {
            if let Some(field) = ty.field(member) {
                let value = field.call(Context::with_span(&mut self.gc, self.span), object)?;
                self.push(value)?;
                return Ok(());
            }

            if let Some(method) = ty.method(member) {
                self.push(object)?;
                self.push(Rc::clone(method))?;
                return Ok(());
            }
        }

        Err(ErrorKind::UnknownField {
            ty: object.ty(),
            field: member.to_string(),
        }
        .at(self.span)
        .into())
    }

    fn load_native_func(&mut self, idx: usize) -> Result<(), Error> {
        self.push(Rc::clone(&self.std.funcs[idx]))
    }

    fn get_func(&mut self, idx: usize) -> Result<Rc<Func>, Error> {
        Ok(Rc::clone(self.module.funcs.get(idx).ok_or_else(|| {
            FatalErrorKind::InvalidFn(idx).at(self.span)
        })?))
    }

    fn get_class(&mut self, idx: usize) -> Result<Rc<Class>, Error> {
        Ok(Rc::clone(self.module.classes.get(idx).ok_or_else(
            || FatalErrorKind::InvalidClass(idx).at(self.span),
        )?))
    }

    fn load_class(&mut self, idx: usize) -> Result<(), Error> {
        let class = self.get_class(idx)?;
        self.push(class)
    }

    fn load_func(&mut self, idx: usize) -> Result<(), Error> {
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

        let (offset, receiver) = func
            .receiver
            .as_ref()
            .map(|_| (1, Some(self.stack.pop())))
            .unwrap_or_default();

        match &func.exec {
            Exec::Vm(_) => {
                if tail_call {
                    self.push_tail_call(arg_count + offset)?;
                } else {
                    self.push_call(func, arg_count + offset)?;
                }

                let frame = self.call_stack.last_mut();

                if let Some(receiver) = receiver {
                    frame.locals[arg_count] = receiver;
                }

                for i in 0..arg_count {
                    frame.locals[i] = self.stack.pop();
                }
            }
            Exec::Handler(handler) => {
                let mut args = vec![];
                resize(&mut args, arg_count + offset);

                if let Some(receiver) = receiver {
                    args[0] = receiver;
                }

                for i in 0..arg_count {
                    args[i + offset] = self.stack.pop();
                }

                let value = (handler)(Context::with_span(&mut self.gc, self.span), args)?;
                self.push(value)?;
            }
        }

        Ok(())
    }

    fn concat(&mut self, lhs: Value, rhs: Value) -> Result<Value, Error> {
        let ty = lhs.ty();
        let value = match ty {
            Type::Array => {
                let lhs = self.gc.get(lhs.array());
                let rhs = self.gc.get(rhs.array());
                let new_data = lhs.concat(rhs);
                let array = Array::from_vec(&mut self.gc, new_data);
                Value::from(self.gc.alloc(array)?)
            }
            Type::Str => {
                let Str(lhs) = self.gc.get(lhs.str());
                let Str(rhs) = self.gc.get(rhs.str());
                let new_data = lhs.concat(rhs);
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
        self.try_collect_gc();

        Ok(())
    }

    fn prepare_elem(&mut self) -> Result<(&mut Array<Value>, usize), Error> {
        let elem = self.stack.pop();
        let array = self.stack.pop();

        self.check_type(elem.ty(), Type::Int)?;
        self.check_type(array.ty(), Type::Array)?;

        let array = self.gc.get_mut(array.array());
        let n = match elem.int() {
            n if n < 0 => array.len() as i64 + n,
            n => n,
        };

        Ok((array, n as usize))
    }

    fn store_elem(&mut self) -> Result<(), Error> {
        let value = self.stack.pop();
        let (array, n) = self.prepare_elem()?;

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
            self.try_collect_gc();

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
                Op::LoadFunc => self.load_func(opcode.code())?,
                Op::LoadClass => self.load_class(opcode.code())?,
                Op::LoadNativeFunc => self.load_native_func(opcode.code())?,
                Op::Store => self.store(opcode.code())?,
                Op::Load => self.load(opcode.code())?,
                Op::LoadArg => self.load_arg(opcode.code())?,
                Op::Discard => self.discard(),
                Op::Return => self.ret()?,
                Op::Call => self.call(opcode.code(), false)?,
                Op::TailCall => self.call(opcode.code(), true)?,
                Op::Jump => self.goto(opcode.code()),
                Op::JumpIfFalse => self.jump_cond(opcode.code(), false, false)?,
                Op::PushJumpIfTrue => self.jump_cond(opcode.code(), true, true)?,
                Op::PushJumpIfFalse => self.jump_cond(opcode.code(), true, false)?,
                Op::MakeArray => self.make_array(opcode.code())?,
                Op::LoadElement => self.load_elem()?,
                Op::StoreElement => self.store_elem()?,
                Op::UnaryNot => self.not()?,
            }
        }

        Ok(())
    }

    fn add_trace(&mut self, e: Error) -> Error {
        match e {
            Error::Runtime(mut e) => {
                let call_stack = mem::take(&mut self.call_stack);
                e.trace = Some(call_stack.into_iter().map(|s| s.call).collect::<Vec<_>>());
                Error::Runtime(e)
            }
            Error::Fatal(e) => Error::Fatal(e),
        }
    }

    pub fn run(&mut self) -> Result<Option<Value>, Error> {
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
                        self.codes = self.call_stack.last().call.func.codes();
                        continue;
                    }
                }

                break;
            }

            Ok(self.stack.try_pop())
        };

        start().map_err(|e| self.add_trace(e))
    }
}
