use std::{
    collections::HashMap,
    mem,
    ops::{Add, Div, Mul, Rem, Sub},
    rc::Rc,
    time::Duration,
};

#[cfg(feature = "timings")]
use std::time::Instant;

#[cfg(feature = "tracing")]
use tracing::{instrument, Level};

use crate::{
    collections::Stack,
    error::{IntoSpanned, SpannedError},
    gc::{Gc, Handle, Trace},
    lexer::Span,
    opcode::{Const, Op, Opcode},
    runtime::{
        array::Array,
        class::{Class, Object},
        error::{Call, ErrorKind, RuntimeError},
        func::Func,
        module::Module,
        str::Str,
        value::{self, TryIntoValue, Type, Value},
        Atom,
    },
};

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
    #[error("invalid extern function '{0}'")]
    InvalidExternFn(String),
}

pub type FatalError = SpannedError<FatalErrorKind>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("RuntimeError: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("FatalError: {0}")]
    Fatal(#[from] FatalError),
}

#[derive(Debug)]
struct Frame {
    span: Span,
    offset: usize,
    func: Rc<Func>,
    locals: Vec<Value>,
    receiver: Option<Value>,
}

impl Frame {
    pub fn new(span: Span, func: Rc<Func>) -> Self {
        Self {
            span,
            func,
            offset: 0,
            locals: vec![],
            receiver: None,
        }
    }

    pub fn with_receiver(mut self, receiver: Value) -> Self {
        self.receiver = Some(receiver);
        self
    }

    pub fn with_locals(mut self, locals: Vec<Value>) -> Self {
        self.locals = locals;
        self
    }

    pub fn next(&mut self) -> Option<Opcode> {
        if self.offset < self.func.body.len() {
            let buff = &self.func.body[self.offset..self.offset + 16];
            self.offset += 16;
            Some(Opcode::deserialize(buff))
        } else {
            None
        }
    }
}

fn top_frame(module: &mut Module) -> Frame {
    Frame::new(
        Span::default(),
        Rc::new(Func {
            body: mem::take(&mut module.body),
            ..Func::default()
        }),
    )
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

#[derive(Default, Clone)]
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
    linker: L,
    module: Module,
    returned: bool,
    frame: Frame,
    vars: Vec<Value>,
    consts: [Value; C],
    stack: Stack<Value, S>,
    call_stack: Vec<Frame>,
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

        Ok(Self {
            vars: vec![],
            consts,
            linker,
            returned: false,
            gc,
            frame: top_frame(&mut module),
            span: Span::default(),
            call_stack: vec![],
            timing: HashMap::default(),
            stack: Stack::default(),
            module,
        })
    }

    fn unsupported_rhs(&self, lhs: Value, rhs: Value, op: &'static str) -> Error {
        ErrorKind::UnsupportedBinaryOp {
            left: lhs.ty(),
            right: rhs.ty(),
            op,
        }
        .at(self.span)
        .into()
    }

    fn unsupported(&self, op: &'static str, ty: Type) -> Error {
        ErrorKind::UnsupportedOp { ty, op }.at(self.span).into()
    }

    pub fn timing(&self) -> Vec<(Op, Timing)> {
        let mut entries = self
            .timing
            .iter()
            .map(|(op, timing)| (*op, timing.clone()))
            .collect::<Vec<_>>();

        entries.sort_by_key(|(_, timing)| timing.elapsed);
        entries.reverse();
        entries
    }

    fn gc_tick(&mut self) {
        if !self.gc.ready() {
            return;
        }

        self.mark_sweep();
    }

    fn mark_sweep(&mut self) {
        self.vars
            .iter()
            .chain(self.consts.iter())
            .chain(self.stack.iter())
            .chain(
                self.call_stack
                    .iter()
                    .flat_map(|frame| frame.locals.iter().chain(frame.receiver.as_ref())),
            )
            .for_each(|value| value.trace(&mut self.gc));

        self.gc.sweep();
    }

    fn push_tail_call(&mut self, arg_count: usize) {
        self.frame.offset = 0;
        self.frame.locals.resize(arg_count, Value::NIL);
        self.returned = false;
    }

    fn goto(&mut self, pos: usize) {
        let offset = pos * 16;
        self.frame.offset = offset;
        self.span = Span::deserialize(&self.frame.func.body[offset + 8..offset + 16]);
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
            Type::Object => self.load_attr(object, idx),
            _ => self.load_field(object, idx),
        }
    }

    fn store_member(&mut self, member: usize) -> Result<(), Error> {
        let member = self.const_name(member)?;
        let value = self.stack.pop();
        let object = self.stack.pop();

        self.check_type(object.ty(), Type::Object)?;

        let mut object = object.object();
        object.attrs.insert(member, value);

        Ok(())
    }

    fn load_attr(&mut self, object: Value, member: usize) -> Result<(), Error> {
        let member = self.const_name(member)?;
        let object = object.object();

        if object.attrs.is_empty() {
            return self.load_method(object, member);
        }

        match object.attrs.get(&member).copied() {
            Some(value) => {
                self.stack.push(value);
                Ok(())
            }
            None => self.load_method(object, member),
        }
    }

    fn load_method(&mut self, object: Handle<Object>, member: Handle<Str>) -> Result<(), Error> {
        let member = member.as_str();

        match object.class.methods.get(member) {
            Some(method) => {
                let method = Rc::clone(method);
                self.stack.push(object.into());
                self.stack.push(method.into());

                Ok(())
            }
            None => Err(ErrorKind::UnknownAttr {
                class: Rc::clone(&object.class),
                attribute: member.to_string(),
            }
            .at(self.span)
            .into()),
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
            self.stack.push(object);
            self.stack.push(method.into());
            return Ok(());
        }

        Err(ErrorKind::UnknownField {
            ty: object.ty(),
            field: member.to_string(),
        }
        .at(self.span)
        .into())
    }

    fn load_class(&mut self, idx: usize) {
        let class = Rc::clone(&self.module.classes[idx]);
        self.stack.push(class.into());
    }

    fn load_fn(&mut self, idx: usize) {
        let func = Rc::clone(&self.module.funcs[idx]);
        self.stack.push(func.into());
    }

    fn return_arg(&mut self, idx: usize) {
        self.load_arg(idx);
        self.ret();
    }

    fn load_arg(&mut self, idx: usize) {
        self.stack.push(self.frame.locals[idx]);
    }

    fn load_self(&mut self) -> Result<(), Error> {
        let receiver = self
            .frame
            .receiver
            .ok_or_else(|| ErrorKind::NoReceiver.at(self.span))?;

        self.stack.push(receiver);
        Ok(())
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
                self.stack.push(cond.into());
            }

            self.goto(idx);
        }

        Ok(())
    }

    fn init_class(&mut self, class: Rc<Class>, arg_count: usize) -> Result<(), Error> {
        let init_fn = class.methods.get("init").map(Rc::clone);
        let object = Object::new(class);
        let handle = self.gc.alloc(object)?;

        if let Some(func) = init_fn {
            self.stack.push(handle.into());
            self.stack.push(func.into());
            self.call(arg_count, false)?;
        } else {
            self.stack.push(handle.into());
        }

        self.gc_tick();

        Ok(())
    }

    fn call_extern(&mut self, idx: usize) -> Result<(), Error> {
        let name = self.const_name(idx)?;
        let name = name.as_str();
        let handler = self
            .linker
            .resolve(name)
            .ok_or_else(|| FatalErrorKind::InvalidExternFn(name.to_string()).at(self.span))?;
        let args = mem::take(&mut self.frame.locals);
        let mut atom = Atom::new(&mut self.gc).with_span(self.span);

        if let Some(receiver) = self.frame.receiver {
            atom = atom.with_receiver(receiver);
        }

        let return_value = (handler)(atom, args)?;

        self.returned = true;
        self.stack.push(return_value);
        self.gc_tick();

        Ok(())
    }

    fn call_fn(&mut self, (fn_idx, arg_count): (u32, u32), tail_call: bool) -> Result<(), Error> {
        self.fn_call(
            Rc::clone(&self.module.funcs[fn_idx as usize]),
            arg_count as usize,
            tail_call,
        )
    }

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

        if tail_call {
            self.push_tail_call(arg_count);
        } else {
            let mut frame = Frame::new(self.span, func).with_locals(vec![Value::NIL; arg_count]);

            if frame.func.method {
                frame = frame.with_receiver(self.stack.pop());
            }

            self.call_stack.push(mem::replace(&mut self.frame, frame));
        }

        self.stack.copy_to(&mut self.frame.locals, arg_count);
        self.gc_tick();

        Ok(())
    }

    fn concat(&mut self, lhs: Value, rhs: Value) -> Result<Value, Error> {
        let value = match lhs.ty() {
            Type::Array => {
                let lhs = lhs.array();
                let rhs = rhs.array();
                let array = Array::from_vec(&mut self.gc, lhs.concat(&rhs));
                Value::from(self.gc.alloc(array)?)
            }
            Type::Str => {
                let lhs = lhs.str();
                let rhs = rhs.str();
                let array = Array::from_vec(&mut self.gc, lhs.0.concat(&rhs.0));
                Value::from(self.gc.alloc(Str(array))?)
            }
            _ => unreachable!(),
        };

        Ok(value)
    }

    fn make_array(&mut self, size: usize) -> Result<(), Error> {
        let mut values = vec![];
        self.stack.copy_to(&mut values, size);

        let array = Array::from_vec(&mut self.gc, values);
        let handle = self.gc.alloc(array)?;

        self.stack.push(handle.into());
        self.gc_tick();

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

        self.stack.push(new_array.into());
        self.gc_tick();

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
            Some(elem) => {
                self.stack.push(elem);
                Ok(())
            }
            None => Err(ErrorKind::IndexOutOfBounds(n).at(self.span).into()),
        }
    }

    fn not(&mut self) -> Result<(), Error> {
        let value = self.stack.pop();
        self.check_type(value.ty(), Type::Bool)?;
        self.stack.push((!value.bool()).into());
        Ok(())
    }

    fn push_int(&mut self, i: i64) -> Result<(), Error> {
        if i.unsigned_abs() > value::INT_MASK {
            let handle = self.gc.alloc(i)?;
            self.stack.push(handle.into());
        } else {
            self.stack.push(Value::try_from(i).unwrap());
        }

        Ok(())
    }

    fn eq_op(&mut self) -> Result<bool, Error> {
        let (lhs, rhs) = self.stack.operands();
        let ty = lhs.ty();

        if ty != rhs.ty() {
            return Ok(false);
        }

        if lhs == rhs {
            return Ok(true);
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

    fn binary_numeric<I, F>(&mut self, op: &'static str, i: I, f: F) -> Result<(), Error>
    where
        I: FnOnce(i64, i64) -> i64,
        F: FnOnce(f64, f64) -> f64,
    {
        let (lhs, rhs) = self.stack.operands();

        match lhs.ty() {
            Type::Int if rhs.is_int() => self.push_int(i(lhs.int(), rhs.int())),
            Type::Float if rhs.is_float() => {
                self.stack.push(Value::from(f(lhs.float(), rhs.float())));
                Ok(())
            }
            ty => Err(self.unsupported(op, ty)),
        }
    }

    fn compare_numeric<I, F>(&mut self, op: &'static str, i: I, f: F) -> Result<(), Error>
    where
        I: FnOnce(&i64, &i64) -> bool,
        F: FnOnce(&f64, &f64) -> bool,
    {
        let (lhs, rhs) = self.stack.operands();
        let value = match lhs.ty() {
            Type::Int if rhs.is_int() => Value::from(i(&lhs.int(), &rhs.int())),
            Type::Float if rhs.is_float() => Value::from(f(&lhs.float(), &rhs.float())),
            ty => return Err(self.unsupported(op, ty)),
        };

        self.stack.push(value);
        Ok(())
    }

    fn lt(&mut self) -> Result<(), Error> {
        self.compare_numeric("<", i64::lt, f64::lt)
    }

    fn lte(&mut self) -> Result<(), Error> {
        self.compare_numeric("<=", i64::le, f64::le)
    }

    fn gt(&mut self) -> Result<(), Error> {
        self.compare_numeric(">", i64::gt, f64::gt)
    }

    fn gte(&mut self) -> Result<(), Error> {
        self.compare_numeric(">=", i64::ge, f64::ge)
    }

    fn add(&mut self) -> Result<(), Error> {
        self.binary_numeric("+", i64::add, f64::add)
    }

    fn sub(&mut self) -> Result<(), Error> {
        self.binary_numeric("-", i64::sub, f64::sub)
    }

    fn mul(&mut self) -> Result<(), Error> {
        self.binary_numeric("*", i64::mul, f64::mul)
    }

    fn div(&mut self) -> Result<(), Error> {
        self.binary_numeric("/", i64::div, f64::div)
    }

    fn rem(&mut self) -> Result<(), Error> {
        self.binary_numeric("%", i64::rem, f64::rem)
    }

    fn bitwise_and(&mut self) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();

        match lhs.ty() {
            Type::Int if rhs.is_int() => self.push_int(lhs.int() & rhs.int()),
            ty => Err(self.unsupported("&", ty)),
        }
    }

    fn bitwise_or(&mut self) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();

        match lhs.ty() {
            Type::Int if rhs.is_int() => self.push_int(lhs.int() | rhs.int()),
            ty => Err(self.unsupported("|", ty)),
        }
    }

    fn bitwise_xor(&mut self) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();
        let ty = lhs.ty();

        match ty {
            Type::Int if rhs.is_int() => self.push_int(lhs.int() ^ rhs.int()),
            Type::Array | Type::Str if ty == rhs.ty() => {
                let value = self.concat(lhs, rhs)?;

                self.stack.push(value);
                self.gc_tick();

                Ok(())
            }
            ty => Err(self.unsupported("^", ty)),
        }
    }

    fn store(&mut self, idx: usize) {
        if self.vars.len() <= idx {
            resize(&mut self.vars, idx + 1);
        }

        self.vars[idx] = self.stack.pop();
    }

    fn load(&mut self, idx: usize) {
        self.stack.push(self.vars[idx]);
    }

    fn load_const(&mut self, idx: usize) {
        self.stack.push(self.consts[idx]);
    }

    fn discard(&mut self) {
        self.stack.pop();
    }

    fn ret(&mut self) {
        self.returned = true;
        self.frame.offset = self.frame.func.body.len();
    }

    #[cfg_attr(feature = "tracing", instrument(level = Level::DEBUG, skip(self), ret(Debug)))]
    fn eval(&mut self) -> Result<(), Error> {
        while let Some(code) = self.frame.next() {
            self.span = code.span();

            #[cfg(feature = "timings")]
            let (op, now) = (code.op(), Instant::now());

            match code.op() {
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
                Op::LoadConst => self.load_const(code.code()),
                Op::LoadMember => self.load_member(code.code())?,
                Op::StoreMember => self.store_member(code.code())?,
                Op::LoadFn => self.load_fn(code.code()),
                Op::LoadClass => self.load_class(code.code()),
                Op::Store => self.store(code.code()),
                Op::Load => self.load(code.code()),
                Op::LoadArg => self.load_arg(code.code()),
                Op::LoadSelf => self.load_self()?,
                Op::Discard => self.discard(),
                Op::Return => self.ret(),
                Op::ReturnArg => self.return_arg(code.code()),
                Op::Call => self.call(code.code(), false)?,
                Op::CallFn => self.call_fn(code.code2(), false)?,
                Op::CallExtern => self.call_extern(code.code())?,
                Op::TailCall => self.call(code.code(), true)?,
                Op::TailCallFn => self.call_fn(code.code2(), true)?,
                Op::Jump => self.goto(code.code()),
                Op::JumpIfFalse => self.jump_cond(code.code(), false, false)?,
                Op::PushJumpIfTrue => self.jump_cond(code.code(), true, true)?,
                Op::PushJumpIfFalse => self.jump_cond(code.code(), true, false)?,
                Op::MakeArray => self.make_array(code.code())?,
                Op::MakeSlice => self.make_slice(code.code())?,
                Op::LoadElement => self.load_elem()?,
                Op::StoreElement => self.store_elem()?,
                Op::UnaryNot => self.not()?,
            }

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
                let mut stack_trace = call_stack
                    .into_iter()
                    .rev()
                    .map(|s| Call::new(s.span, s.func))
                    .collect::<Vec<_>>();

                stack_trace.reverse();

                e.trace = Some(stack_trace);
                Error::Runtime(e)
            }
            Error::Fatal(e) => Error::Fatal(e),
        }
    }

    pub fn run(&mut self) -> Result<(), Error> {
        let mut start = || {
            loop {
                self.eval()?;

                match self.call_stack.pop() {
                    Some(frame) => {
                        if !self.returned {
                            self.stack.push(Value::NIL);
                        }

                        self.returned = false;
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
