use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    mem,
};

use broom::{Handle, Heap};

use crate::{
    codes::{BinaryOp, CompareOp, Const, Cursor, Op},
    compiler::Module,
    lexer::Span,
    runtime::{
        self,
        error::ErrorKind,
        std::Registry,
        value::{HeapValue, Type, Value, ValueKind},
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

#[derive(Default)]
pub struct Gc {
    heap: Heap<HeapValue>,
    span: Span,
}

impl Gc {
    pub fn at(&mut self, span: Span) {
        self.span = span;
    }

    pub fn read(&self, handle: Handle<HeapValue>) -> Result<&HeapValue, runtime::error::Error> {
        self.heap
            .get(handle)
            .ok_or_else(|| ErrorKind::Segfault.at(self.span))
    }

    pub fn alloc(&mut self, value: HeapValue) -> Handle<HeapValue> {
        self.heap.insert(value).handle()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum FatalErrorKind {
    #[error("invalid const at: {0}")]
    InvalidConst(usize),
    #[error("invalid var at: {0}")]
    InvalidVar(usize),
    #[error("stack is empty")]
    StackEmpty,
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

struct Call {
    span: Span,
    name: usize,
    receiver: Option<Type>,
}

impl Call {
    pub fn new(span: Span, name: usize) -> Self {
        Call {
            span,
            name,
            receiver: None,
        }
    }
}

pub struct Vm {
    gc: Gc,
    span: Span,
    std: Registry,
    module: Module,
    cursor: Cursor,
    stack: Vec<Value>,
    call_stack: Vec<Call>,
    vars: HashMap<usize, Value>,
}

impl Vm {
    pub fn new(module: Module) -> Self {
        Self {
            stack: vec![],
            gc: Gc::default(),
            call_stack: vec![],
            vars: HashMap::new(),
            span: Span::default(),
            std: runtime::std::registry(),
            cursor: Cursor::new(&module.codes),
            module,
        }
    }

    fn push_call(&mut self, name: usize) {
        self.call_stack.push(Call::new(self.span, name));
    }

    fn pop_call(&mut self) -> Option<Call> {
        self.call_stack.pop()
    }

    fn with_cursor(&mut self, cursor: Cursor) -> Cursor {
        mem::replace(&mut self.cursor, cursor)
    }

    fn restore_cursor(&mut self, cursor: Cursor) {
        self.cursor = cursor;
    }

    fn goto(&mut self, n: usize) {
        self.cursor.goto(n);

        if let Some(code) = self.cursor.cur() {
            self.span = code.span;
            self.gc.at(code.span);
        }
    }

    fn next(&mut self) -> Option<Op> {
        if let Some(code) = self.cursor.next() {
            self.span = code.span;
            self.gc.at(code.span);

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
                let handle = self.gc.alloc(HeapValue::Buffer(s.into_bytes()));
                Ok(Value::new_str(handle))
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

        let value = field.call(&mut self.gc, object)?;
        self.stack.push(value);

        Ok(())
    }

    fn load_func(&mut self, idx: usize) -> Result<(), Error> {
        let name = self.const_name(idx)?;
        let func = self
            .module
            .funcs
            .get(name)
            .ok_or_else(|| ErrorKind::UnknownFunc(name.to_string()).at(self.span))?;

        self.stack.push(Value::new_func(func.clone()));
        Ok(())
    }

    fn load_var(&self, idx: usize) -> Result<Value, Error> {
        match self.vars.get(&idx) {
            Some(value) => Ok(value.clone()),
            None => Err(FatalErrorKind::InvalidVar(idx).at(self.span).into()),
        }
    }

    fn pop(&mut self) -> Result<Value, Error> {
        self.stack
            .pop()
            .ok_or_else(|| FatalErrorKind::StackEmpty.at(self.span).into())
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
                self.stack.push(cond.into());
            }

            self.goto(idx);
        }

        Ok(())
    }

    fn call(&mut self, args: usize) -> Result<(), Error> {
        if args > 0 {
            unimplemented!()
        }

        let callee = self.pop()?;

        match callee.ty() {
            Type::Fn => {
                let func = callee.func();
                let previous = self.with_cursor(Cursor::new(&func.codes));

                self.push_call(func.name);

                if let Some(return_value) = self.run()? {
                    self.stack.push(return_value);
                }

                self.restore_cursor(previous);
                self.pop_call();

                Ok(())
            }
            ty => Err(ErrorKind::NotCallable(ty).at(self.span).into()),
        }
    }

    fn concat(&mut self, lhs: Value, rhs: Value) -> Result<Value, Error> {
        let ty = lhs.ty();
        let lhs = self.gc.read(lhs.heap())?;
        let rhs = self.gc.read(rhs.heap())?;

        Ok(match ty {
            Type::Array => {
                let out = [lhs.array(), rhs.array()].concat();
                Value::new_array(self.gc.alloc(HeapValue::Array(out)))
            }
            Type::Str => {
                let out = [lhs.buffer(), rhs.buffer()].concat();
                Value::new_str(self.gc.alloc(HeapValue::Buffer(out)))
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

        let handle = self.gc.alloc(HeapValue::Array(values));
        self.stack.push(Value::new_array(handle));

        Ok(())
    }

    fn load_elem(&mut self) -> Result<(), Error> {
        let elem = self.pop()?;
        let array = self.pop()?;

        self.check_type(elem.ty(), Type::Int)?;
        self.check_type(array.ty(), Type::Array)?;

        let heap = self.gc.read(array.heap())?;
        let array = heap.array();
        let n = match elem.int() {
            n if n < 0 => array.len() as i64 + n,
            n => n,
        } as usize;

        match array.get(n) {
            Some(elem) => {
                self.stack.push(elem.clone());
                Ok(())
            }
            None => Err(ErrorKind::IndexOutOfBounds(n).at(self.span).into()),
        }
    }

    pub fn repr(&self, value: &Value) -> Result<String, Error> {
        let ty = value.ty();

        Ok(match value.kind() {
            ValueKind::Heap(handle) => match self.gc.read(*handle)? {
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
            kind => format!("{kind}"),
        })
    }

    fn eval(&mut self, op: Op) -> Result<(), Error> {
        match op {
            Op::LoadConst(idx) => {
                let value = self.load_const(idx)?;
                self.stack.push(value);
            }
            Op::LoadMember(idx) => self.load_member(idx)?,
            Op::LoadFunc(idx) => self.load_func(idx)?,
            Op::CompareOp(op) => {
                let rhs = self.pop()?;
                let lhs = self.pop()?;

                self.check_type(lhs.ty(), rhs.ty())?;
                self.stack.push(match op {
                    CompareOp::Eq => binary!(lhs == rhs),
                    CompareOp::Ne => binary!(lhs != rhs),
                    CompareOp::Lt => binary!(lhs < rhs),
                    CompareOp::Lte => binary!(lhs <= rhs),
                    CompareOp::Gt => binary!(lhs > rhs),
                    CompareOp::Gte => binary!(lhs >= rhs),
                });
            }
            Op::BinaryOp(op) => {
                let rhs = self.pop()?;
                let lhs = self.pop()?;

                self.check_type(lhs.ty(), rhs.ty())?;

                let value = match op {
                    BinaryOp::Add if lhs.kind().is_number() => binary!(Int | Float, lhs + rhs),
                    BinaryOp::Sub if lhs.kind().is_number() => binary!(Int | Float, lhs - rhs),
                    BinaryOp::Mul if lhs.kind().is_number() => binary!(Int | Float, lhs * rhs),
                    BinaryOp::Div if lhs.kind().is_number() => binary!(Int | Float, lhs / rhs),
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

                self.stack.push(value);
            }
            Op::Store(idx) => {
                let value = self.pop()?;
                self.vars.insert(idx, value);
            }
            Op::Load(idx) => {
                let value = self.load_var(idx)?;
                self.stack.push(value);
            }
            Op::Discard => {
                self.stack.pop();
            }
            // @TODO: implement when we have functions
            Op::Return => {}
            Op::Call(args) => self.call(args)?,
            Op::JumpIfTrue(idx) => self.jump_cond(idx, false, true)?,
            Op::JumpIfFalse(idx) => self.jump_cond(idx, false, false)?,
            Op::PushJumpIfTrue(idx) => self.jump_cond(idx, true, true)?,
            Op::PushJumpIfFalse(idx) => self.jump_cond(idx, true, false)?,
            Op::MakeArray(len) => self.make_array(len)?,
            Op::LoadElement => self.load_elem()?,
            Op::UnaryNot => {
                let value = self.pop()?;
                self.check_type(value.ty(), Type::Bool)?;
                self.stack.push((!value.bool()).into());
            }
        }

        Ok(())
    }

    fn build_trace(&self) -> Result<Vec<runtime::error::Call>, Error> {
        self.call_stack
            .iter()
            .map(|call| {
                let name = self.const_name(call.name)?;
                Ok(runtime::error::Call::new(call.span, name.to_string()))
            })
            .collect::<Result<Vec<_>, _>>()
    }

    fn add_trace(&self, e: Error) -> Error {
        match e {
            Error::Runtime(mut e) => {
                e.trace = self.build_trace().ok();
                Error::Runtime(e)
            }
            Error::Fatal(e) => Error::Fatal(e),
        }
    }

    pub fn run(&mut self) -> Result<Option<Value>, Error> {
        while let Some(op) = self.next() {
            self.eval(op).map_err(|e| self.add_trace(e))?;
        }

        Ok(self.stack.pop())
    }
}
