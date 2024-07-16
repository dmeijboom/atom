use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    mem::{self, size_of},
    rc::Rc,
};

use safe_gc::Heap;
#[cfg(feature = "tracing")]
use tracing::{instrument, Level};

use crate::{
    opcode::{Const, Op, Opcode},
    compiler::Module,
    lexer::Span,
    reuse_vec::ReuseVec,
    runtime::{
        self,
        error::{Call, ErrorKind},
        function::{Exec, Func},
        std::StdLib,
        value::{HeapValue, Type, Value},
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
            $(Type::$ty => (unwrap!($ty, $lhs) $op unwrap!($ty, $rhs)).into()),+
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
        let rhs = $self.pop();
        let lhs = $self.pop();

        $self.check_type(lhs.ty(), rhs.ty())?;
        binary!($self: $($ty)|+, lhs, $op, rhs)
    }};

    ($self:ident: lhs $op:tt rhs) => {
        binary!($self: Int | Float | Bool, lhs $op rhs)
    }
}

fn resize<T: Default + Clone>(vec: &mut Vec<T>, len: usize) {
    match vec.len().cmp(&len) {
        Ordering::Less => vec.resize(len, T::default()),
        Ordering::Equal => {}
        Ordering::Greater => vec.truncate(len),
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
    #[error("maximum stack exceeded")]
    MaxStackExceeded,
    #[error("call stack is empty")]
    CallStackEmpty,
}

impl FatalErrorKind {
    pub fn at(self, span: Span) -> FatalError {
        FatalError { kind: self, span }
    }
}

#[derive(Debug)]
pub struct FatalError {
    pub kind: FatalErrorKind,
    pub span: Span,
}

impl Display for FatalError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl std::error::Error for FatalError {}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("RuntimeError: {0}")]
    Runtime(#[from] runtime::error::Error),
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

pub struct Vm<'a> {
    heap: Heap,
    span: Span,
    pos: usize,
    std: &'a StdLib,
    module: Module,
    returned: bool,
    vars: Vec<Value>,
    stack_len: usize,
    codes: Rc<[Opcode]>,
    call_stack: ReuseVec<Frame>,
    stack: [Value; MAX_STACK_SIZE],
    consts: [Value; MAX_CONST_SIZE],
}

impl<'a> Vm<'a> {
    pub fn new(std: &'a StdLib, mut module: Module) -> Self {
        let mut heap = Heap::default();
        let mut consts = [Value::NIL; MAX_CONST_SIZE];

        for (i, const_) in module.consts.drain(..).enumerate() {
            consts[i] = match const_ {
                Const::Int(i) => Value::from(i),
                Const::Float(f) => Value::from(f),
                Const::Bool(b) => Value::from(b),
                Const::Str(s) => {
                    let root = heap.alloc(HeapValue::Buffer(s.into_bytes()));
                    Value::new_str(root.unrooted())
                }
            };
        }

        let mut call_stack = ReuseVec::default();
        call_stack.push(top_frame(&module));

        Self {
            std,
            vars: vec![],
            consts,
            returned: false,
            heap,
            pos: 0,
            span: Span::default(),
            stack_len: 0,
            call_stack,
            stack: [Value::NIL; MAX_STACK_SIZE],
            codes: Rc::clone(&module.codes),
            module,
        }
    }

    fn push(&mut self, value: Value) -> Result<(), Error> {
        if self.stack_len == self.stack.len() {
            return Err(FatalErrorKind::MaxStackExceeded.at(self.span).into());
        }

        self.stack[self.stack_len] = value;
        self.stack_len += 1;

        Ok(())
    }

    fn pop_n(&mut self, n: usize) -> Vec<Value> {
        let mut values = Vec::with_capacity(n);

        for _ in 0..n {
            values.push(self.pop());
        }

        values.reverse();
        values
    }

    fn pop(&mut self) -> Value {
        let value = self.stack[self.stack_len - 1];
        self.stack_len -= 1;
        value
    }

    fn call_frame(&mut self) -> Result<&mut Frame, Error> {
        Ok(self
            .call_stack
            .last_mut()
            .ok_or_else(|| FatalErrorKind::CallStackEmpty.at(self.span))?)
    }

    fn push_tail_call(&mut self, arg_count: usize) -> Result<(), Error> {
        let frame = self.call_frame()?;
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
            frame.locals.truncate(arg_count);
            frame.return_pos = return_pos;
        } else {
            self.call_stack.push(Frame {
                call: Call::new(self.span, func),
                locals: Vec::with_capacity(arg_count),
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
                let heap = value.heap();
                let heap_value = self.heap.get(heap);

                unsafe { Ok(std::str::from_utf8_unchecked(heap_value.buffer())) }
            }
            _ => unreachable!(),
        }
    }

    fn load_member(&mut self, idx: usize) -> Result<(), Error> {
        let object = self.pop();
        let member = self.const_name(idx)?;
        let field = self
            .std
            .types
            .get(&object.ty())
            .and_then(|t| t.field(member))
            .ok_or_else(|| {
                ErrorKind::UnknownField {
                    ty: object.ty(),
                    field: member.to_string(),
                }
                .at(self.span)
            })?;

        let value = field.call(&mut self.heap, object)?;
        self.push(value)?;

        Ok(())
    }

    fn load_native_func(&mut self, idx: usize) -> Result<(), Error> {
        self.push(Value::new_func(Rc::clone(&self.std.funcs[idx])))
    }

    fn load_func(&mut self, idx: usize) -> Result<(), Error> {
        let func = self
            .module
            .funcs
            .get(idx)
            .ok_or_else(|| FatalErrorKind::InvalidFn(idx).at(self.span))?;

        self.push(Value::new_func(Rc::clone(func)))?;
        Ok(())
    }

    fn load_var(&self, idx: usize) -> Result<Value, Error> {
        match self.vars.get(idx).copied() {
            Some(value) => Ok(value),
            None => Err(FatalErrorKind::InvalidVar(idx).at(self.span).into()),
        }
    }

    fn load_arg(&mut self, idx: usize) -> Result<(), Error> {
        let frame = self.call_frame()?;

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
        let value = self.pop();
        self.check_type(value.ty(), Type::Bool)?;

        if value.bool() == cond {
            if push {
                self.push(cond.into())?;
            }

            self.goto(idx);
        }

        Ok(())
    }

    fn call(&mut self, arg_count: usize, tail_call: bool) -> Result<(), Error> {
        let callee = self.pop();

        match callee.ty() {
            Type::Fn => {
                let func = callee.func();

                if arg_count != func.arg_count {
                    return Err(ErrorKind::ArgCountMismatch { func, arg_count }
                        .at(self.span)
                        .into());
                }

                match &func.exec {
                    Exec::Vm(_) => {
                        if tail_call {
                            self.push_tail_call(arg_count)?;
                        } else {
                            self.push_call(func, arg_count)?;
                        }

                        for _ in 0..arg_count {
                            let value = self.pop();
                            self.call_frame()?.locals.push(value);
                        }
                    }
                    Exec::Handler(handler) => {
                        let args = self.pop_n(arg_count);
                        let value = (handler)(&mut self.heap, args)?;
                        self.push(value)?;
                    }
                }

                Ok(())
            }
            ty => Err(ErrorKind::NotCallable(ty).at(self.span).into()),
        }
    }

    fn concat(&mut self, lhs: Value, rhs: Value) -> Result<Value, Error> {
        let ty = lhs.ty();
        let lhs = self.heap.get::<HeapValue>(lhs.heap());
        let rhs = self.heap.get::<HeapValue>(rhs.heap());

        Ok(match ty {
            Type::Array => {
                let out = [lhs.array(), rhs.array()].concat();
                Value::new_array(self.heap.alloc(HeapValue::Array(out)).unrooted())
            }
            Type::Str => {
                let out = [lhs.buffer(), rhs.buffer()].concat();
                Value::new_str(self.heap.alloc(HeapValue::Buffer(out)).unrooted())
            }
            _ => unreachable!(),
        })
    }

    fn make_array(&mut self, size: usize) -> Result<(), Error> {
        let mut values = vec![];

        for _ in 0..size {
            values.push(self.pop());
        }

        values.reverse();

        let handle = self.heap.alloc(HeapValue::Array(values));
        self.push(Value::new_array(handle.unrooted()))?;

        Ok(())
    }

    fn load_elem(&mut self) -> Result<(), Error> {
        let elem = self.pop();
        let array = self.pop();

        self.check_type(elem.ty(), Type::Int)?;
        self.check_type(array.ty(), Type::Array)?;

        let heap = self.heap.get::<HeapValue>(array.heap());
        let array = heap.array();
        let n = match elem.int() {
            n if n < 0 => array.len() as i64 + n,
            n => n,
        } as usize;

        match array.get(n).copied() {
            Some(elem) => self.push(elem),
            None => Err(ErrorKind::IndexOutOfBounds(n).at(self.span).into()),
        }
    }

    fn not(&mut self) -> Result<(), Error> {
        let value = self.pop();
        self.check_type(value.ty(), Type::Bool)?;
        self.push((!value.bool()).into())
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

    fn bitwise_and(&mut self) -> Result<(), Error> {
        binary!(self: Int, lhs & rhs)
    }

    fn bitwise_or(&mut self) -> Result<(), Error> {
        binary!(self: Int, lhs | rhs)
    }

    fn bitwise_xor(&mut self) -> Result<(), Error> {
        let rhs = self.pop();
        let lhs = self.pop();

        self.check_type(lhs.ty(), rhs.ty())?;

        if matches!(lhs.ty(), Type::Array | Type::Str) {
            let value = self.concat(lhs, rhs)?;
            self.push(value)?;
            return Ok(());
        }

        binary!(self: Int, lhs, ^, rhs)
    }

    fn store(&mut self, idx: usize) -> Result<(), Error> {
        resize(&mut self.vars, idx + 1);
        let value = self.pop();
        self.vars[idx] = value;

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
        self.pop();
    }

    fn ret(&mut self) -> Result<(), Error> {
        self.returned = true;
        self.pos = self.codes.len();

        Ok(())
    }

    #[cfg_attr(feature = "tracing", instrument(level = Level::DEBUG, skip(self), ret(Debug)))]
    fn eval(&mut self, op_code: Opcode) -> Result<(), Error> {
        match op_code.op() {
            Op::Add => self.add()?,
            Op::Sub => self.sub()?,
            Op::Mul => self.mul()?,
            Op::Div => self.div()?,
            Op::Eq => self.eq()?,
            Op::Ne => self.ne()?,
            Op::Lt => self.lt()?,
            Op::Lte => self.lte()?,
            Op::Gt => self.gt()?,
            Op::Gte => self.gte()?,
            Op::BitwiseAnd => self.bitwise_and()?,
            Op::BitwiseOr => self.bitwise_or()?,
            Op::BitwiseXor => self.bitwise_xor()?,
            Op::LoadConst => self.load_const(op_code.code())?,
            Op::LoadMember => self.load_member(op_code.code())?,
            Op::LoadFunc => self.load_func(op_code.code())?,
            Op::LoadNativeFunc => self.load_native_func(op_code.code())?,
            Op::Store => self.store(op_code.code())?,
            Op::Load => self.load(op_code.code())?,
            Op::LoadArg => self.load_arg(op_code.code())?,
            Op::Discard => self.discard(),
            Op::Return => self.ret()?,
            Op::Call => self.call(op_code.code(), false)?,
            Op::TailCall => self.call(op_code.code(), true)?,
            Op::JumpIfFalse => self.jump_cond(op_code.code(), false, false)?,
            Op::PushJumpIfTrue => self.jump_cond(op_code.code(), true, true)?,
            Op::PushJumpIfFalse => self.jump_cond(op_code.code(), true, false)?,
            Op::MakeArray => self.make_array(op_code.code())?,
            Op::LoadElement => self.load_elem()?,
            Op::UnaryNot => self.not()?,
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

    fn next(&mut self) -> Result<Option<Opcode>, Error> {
        match self.codes.get(self.pos) {
            Some(op_code) => {
                self.pos += 1;
                self.span = op_code.span;

                Ok(Some(op_code.clone()))
            }
            None => Ok(None),
        }
    }

    fn post_call(&mut self) -> Result<bool, Error> {
        if let Some(frame) = self.call_stack.pop() {
            self.returned = false;
            self.pos = frame.return_pos;

            if let Some(frame) = self.call_stack.last() {
                self.codes = frame.call.func.codes();
                return Ok(true);
            }
        }

        Ok(false)
    }

    pub fn run(&mut self) -> Result<Option<Value>, Error> {
        let mut start = || loop {
            match self.next()? {
                Some(op_code) => self.eval(op_code)?,
                None => {
                    if !self.returned {
                        self.push(Value::NIL)?;
                    }

                    if !self.post_call()? {
                        return Ok(if self.stack_len > 0 {
                            Some(self.pop())
                        } else {
                            None
                        });
                    }
                }
            }
        };

        start().map_err(|e| self.add_trace(e))
    }
}
