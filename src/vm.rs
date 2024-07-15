use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    mem::{self, size_of},
    rc::Rc,
};

use safe_gc::Heap;
use tinyvec::TinyVec;
#[cfg(feature = "tracing")]
use tracing::{instrument, Level};

use crate::{
    codes::{BinaryOp, CompareOp, Const, Cursor, Func, Op},
    compiler::Module,
    lexer::Span,
    runtime::{
        self,
        error::{Call, ErrorKind},
        std::Registry,
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
    ($($ty:ident)|+, $lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            $(Type::$ty => (unwrap!($ty, $lhs) $op unwrap!($ty, $rhs)).into()),+
            , _ => unreachable!()
        }
    };

    ($lhs:ident $op:tt $rhs:ident) => {
        binary!(Int | Float | Bool, $lhs $op $rhs)
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
    #[error("invalid const at: {0}")]
    InvalidConst(usize),
    #[error("invalid var at: {0}")]
    InvalidVar(usize),
    #[error("invalid argument at: {0}")]
    InvalidArg(usize),
    #[error("invalid fn at: {0}")]
    InvalidFn(usize),
    #[error("stack is empty")]
    StackEmpty,
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

#[derive(Debug, Default, Clone)]
struct Frame {
    call: Call,
    // Locals (in reversed order)
    locals: TinyVec<[Value; 8]>,
    return_addr: Cursor,
}

const DEFAULT_STACK_SIZE: usize = 200000 / size_of::<Value>();

pub struct Vm {
    heap: Heap,
    span: Span,
    std: Registry,
    module: Module,
    cursor: Cursor,
    returned: bool,
    vars: Vec<Value>,
    stack_len: usize,
    call_stack: Vec<Frame>,
    stack: [Value; DEFAULT_STACK_SIZE],
}

impl Vm {
    pub fn new(module: Module) -> Self {
        Self {
            vars: vec![],
            returned: false,
            heap: Heap::default(),
            span: Span::default(),
            std: runtime::std::registry(),
            call_stack: vec![],
            stack_len: 0,
            stack: [Value::NIL; DEFAULT_STACK_SIZE],
            cursor: Cursor::new(Rc::clone(&module.codes)),
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

    fn pop(&mut self) -> Result<Value, Error> {
        if self.stack_len == 0 {
            return Err(FatalErrorKind::StackEmpty.at(self.span).into());
        }

        let value = self.stack[self.stack_len - 1];
        self.stack_len -= 1;

        Ok(value)
    }

    fn call_frame(&mut self) -> Result<&mut Frame, Error> {
        Ok(self
            .call_stack
            .last_mut()
            .ok_or_else(|| FatalErrorKind::CallStackEmpty.at(self.span))?)
    }

    fn pop_frame(&mut self) -> Option<Frame> {
        self.call_stack.pop()
    }

    fn replace_cursor(&mut self, cursor: impl Into<Cursor>) -> Cursor {
        mem::replace(&mut self.cursor, cursor.into())
    }

    fn push_tail_call(&mut self, arg_count: usize) -> Result<(), Error> {
        let return_addr = self.cursor.clone();
        let state = self.call_frame()?;

        state.return_addr = return_addr;
        state.locals.resize(arg_count, Value::NIL);

        self.returned = false;
        self.cursor.goto(0);

        Ok(())
    }

    fn push_call(&mut self, func: Rc<Func>, arg_count: usize) -> Result<(), Error> {
        let return_addr = self.replace_cursor(&func.codes);

        self.call_stack.push(Frame {
            call: Call::new(self.span, func),
            locals: TinyVec::with_capacity(arg_count),
            return_addr,
        });

        Ok(())
    }

    fn goto(&mut self, n: usize) {
        self.cursor.goto(n);

        if let Some(code) = self.cursor.cur() {
            self.span = code.span;
        }
    }

    fn next(&mut self) -> Option<Op> {
        if let Some(code) = self.cursor.next() {
            self.span = code.span;
            Some(code.op)
        } else {
            None
        }
    }

    fn get_const(&self, idx: usize) -> Result<&Const, Error> {
        self.module
            .consts
            .get(idx)
            .ok_or_else(|| FatalErrorKind::InvalidConst(idx).at(self.span).into())
    }

    fn load_const(&mut self, idx: usize) -> Result<Value, Error> {
        let const_ = self.get_const(idx)?.clone();
        match const_ {
            Const::Int(i) => Ok(i.into()),
            Const::Float(f) => Ok(f.into()),
            Const::Bool(b) => Ok(b.into()),
            Const::Str(s) => {
                let handle = self.heap.alloc(HeapValue::Buffer(s.into_bytes()));
                Ok(Value::new_str(handle.unrooted()))
            }
        }
    }

    fn const_name(&self, idx: usize) -> Result<&str, Error> {
        match self.get_const(idx)? {
            Const::Str(name) => Ok(name.as_str()),
            _ => unreachable!(),
        }
    }

    fn load_member(&mut self, idx: usize) -> Result<(), Error> {
        let object = self.pop()?;
        let member = self.const_name(idx)?;
        let field = self
            .std
            .get(object.ty())
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
        let value = self.pop()?;
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
        let callee = self.pop()?;

        match callee.ty() {
            Type::Fn => {
                let func = callee.func();

                if tail_call {
                    self.push_tail_call(arg_count)?;
                } else {
                    self.push_call(func, arg_count)?;
                }

                for _ in 0..arg_count {
                    let value = self.pop()?;
                    self.call_frame()?.locals.push(value);
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
            let value = self.pop()?;
            values.push(value);
        }

        values.reverse();

        let handle = self.heap.alloc(HeapValue::Array(values));
        self.push(Value::new_array(handle.unrooted()))?;

        Ok(())
    }

    fn load_elem(&mut self) -> Result<(), Error> {
        let elem = self.pop()?;
        let array = self.pop()?;

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

    pub fn repr(&self, value: &Value) -> Result<String, Error> {
        let ty = value.ty();

        Ok(match value.ty() {
            Type::Str | Type::Array => match self.heap.get(value.heap()) {
                HeapValue::Buffer(buff) => match ty {
                    Type::Str => format!("\"{}\"", String::from_utf8_lossy(buff)),
                    _ => unreachable!(),
                },
                HeapValue::Array(items) => {
                    let mut s = String::from("[");

                    for (i, item) in items.iter().enumerate() {
                        if i > 0 {
                            s.push_str(", ");
                        }

                        s.push_str(&self.repr(item)?);
                    }

                    s.push(']');
                    s
                }
            },
            Type::Int => format!("{}", value.int()),
            Type::Float => format!("{}", value.float()),
            Type::Bool => format!("{}", value.bool()),
            Type::Fn => format!("{}(..)", value.func().name),
            Type::Nil => "Nil".to_string(),
        })
    }

    fn compare(&mut self, op: CompareOp) -> Result<(), Error> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;

        self.check_type(lhs.ty(), rhs.ty())?;
        self.push(match op {
            CompareOp::Eq => binary!(lhs == rhs),
            CompareOp::Ne => binary!(lhs != rhs),
            CompareOp::Lt => binary!(lhs < rhs),
            CompareOp::Lte => binary!(lhs <= rhs),
            CompareOp::Gt => binary!(lhs > rhs),
            CompareOp::Gte => binary!(lhs >= rhs),
        })
    }

    fn binary(&mut self, op: BinaryOp) -> Result<(), Error> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;

        self.check_type(lhs.ty(), rhs.ty())?;

        let value = match op {
            BinaryOp::Add if lhs.is_number() => binary!(Int | Float, lhs + rhs),
            BinaryOp::Sub if lhs.is_number() => binary!(Int | Float, lhs - rhs),
            BinaryOp::Mul if lhs.is_number() => binary!(Int | Float, lhs * rhs),
            BinaryOp::Div if lhs.is_number() => binary!(Int | Float, lhs / rhs),
            BinaryOp::BitwiseOr if lhs.ty() == Type::Int => binary!(Int, lhs | rhs),
            BinaryOp::BitwiseAnd if lhs.ty() == Type::Int => binary!(Int, lhs & rhs),
            BinaryOp::BitwiseXor if lhs.ty() == Type::Int => binary!(Int, lhs ^ rhs),
            BinaryOp::BitwiseXor if matches!(lhs.ty(), Type::Array | Type::Str) => {
                self.concat(lhs, rhs)?
            }
            op => {
                return Err(ErrorKind::UnsupportedOp {
                    left: lhs.ty(),
                    right: rhs.ty(),
                    op,
                }
                .at(self.span)
                .into())
            }
        };

        self.push(value)
    }

    fn store(&mut self, idx: usize) -> Result<(), Error> {
        resize(&mut self.vars, idx + 1);
        let value = self.pop()?;
        self.vars[idx] = value;

        Ok(())
    }

    fn load(&mut self, idx: usize) -> Result<(), Error> {
        let value = self.load_var(idx)?;
        self.push(value)
    }

    #[cfg_attr(feature = "tracing", instrument(level = Level::DEBUG, skip(self), ret(Debug)))]
    fn eval(&mut self, op: Op) -> Result<(), Error> {
        match op {
            Op::LoadConst(idx) => {
                let value = self.load_const(idx)?;
                self.push(value)?;
            }
            Op::LoadMember(idx) => self.load_member(idx)?,
            Op::LoadFunc(idx) => self.load_func(idx)?,
            Op::CompareOp(op) => self.compare(op)?,
            Op::BinaryOp(op) => self.binary(op)?,
            Op::Store(idx) => self.store(idx)?,
            Op::Load(idx) => self.load(idx)?,
            Op::LoadArg(idx) => self.load_arg(idx)?,
            Op::Discard => {
                self.pop()?;
            }
            Op::Return => {
                self.returned = true;
                self.cursor.goto_end();
            }
            Op::Call(args) => self.call(args, false)?,
            Op::TailCall(args) => self.call(args, true)?,
            Op::JumpIfFalse(idx) => self.jump_cond(idx, false, false)?,
            Op::PushJumpIfTrue(idx) => self.jump_cond(idx, true, true)?,
            Op::PushJumpIfFalse(idx) => self.jump_cond(idx, true, false)?,
            Op::MakeArray(len) => self.make_array(len)?,
            Op::LoadElement => self.load_elem()?,
            Op::UnaryNot => {
                let value = self.pop()?;
                self.check_type(value.ty(), Type::Bool)?;
                self.push((!value.bool()).into())?;
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
        loop {
            while let Some(op) = self.next() {
                self.eval(op).map_err(|e| self.add_trace(e))?;
            }

            match self.pop_frame() {
                Some(state) => {
                    if !self.returned {
                        self.push(Value::NIL)?;
                    }

                    self.returned = false;
                    self.cursor = state.return_addr;
                }
                None => break,
            }
        }

        Ok(self.pop().ok())
    }
}
