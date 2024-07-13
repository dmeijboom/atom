use std::collections::HashMap;

use broom::{Handle, Heap};

use crate::{
    codes::{BinaryOp, CompareOp, Const, Op},
    compiler::Module,
    lexer::Span,
    runtime::{
        self,
        error::{Error, ErrorKind},
        std::Registry,
        value::{HeapValue, Type, Value, ValueKind},
    },
};

macro_rules! compare_op {
    ($lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            Type::Int => Value::new_bool($lhs.int() $op $rhs.int()),
            Type::Float => Value::new_bool($lhs.float() $op $rhs.float()),
            Type::Bool => Value::new_bool($lhs.bool() $op $rhs.bool()),
            _ => unreachable!()
        }
    };
}

macro_rules! binary_op {
    ($lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            Type::Int => Value::new_int($lhs.int() $op $rhs.int()),
            Type::Float => Value::new_float($lhs.float() $op $rhs.float()),
            _ => unreachable!(),
        }
    };
}

macro_rules! binary_op_int {
    ($lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            Type::Int => Value::new_int($lhs.int() $op $rhs.int()),
            _ => unreachable!(),
        }
    };
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

    pub fn read(&self, handle: Handle<HeapValue>) -> Result<&HeapValue, Error> {
        self.heap
            .get(handle)
            .ok_or_else(|| ErrorKind::Segfault.at(self.span))
    }

    pub fn alloc(&mut self, value: HeapValue) -> Handle<HeapValue> {
        self.heap.insert(value).handle()
    }
}

pub struct Vm {
    n: usize,
    gc: Gc,
    std: Registry,
    module: Module,
    stack: Vec<Value>,
    vars: HashMap<usize, Value>,
}

impl Vm {
    pub fn new(module: Module) -> Self {
        Vm {
            module,
            n: 0,
            stack: vec![],
            gc: Gc::default(),
            vars: HashMap::new(),
            std: runtime::std::registry(),
        }
    }

    fn goto(&mut self, n: usize) {
        self.n = n;
    }

    fn next(&mut self) -> Option<(Span, Op)> {
        if let Some(code) = self.module.codes.get(self.n) {
            self.n += 1;
            self.gc.at(code.span);
            Some((code.span, code.op))
        } else {
            None
        }
    }

    fn get_const(&self, span: Span, idx: usize) -> Result<&Const, Error> {
        self.module
            .consts
            .get(idx)
            .ok_or_else(|| ErrorKind::InvalidConst(idx).at(span))
    }

    fn load_const(&mut self, span: Span, idx: usize) -> Result<Value, Error> {
        let const_ = self.get_const(span, idx)?.clone();
        match const_ {
            Const::Int(i) => Ok(Value::new_int(i)),
            Const::Float(f) => Ok(Value::new_float(f)),
            Const::Bool(b) => Ok(Value::new_bool(b)),
            Const::Str(s) => {
                let handle = self.gc.alloc(HeapValue::Buffer(s.into_bytes()));
                Ok(Value::new_str(handle))
            }
        }
    }

    fn load_member(&mut self, span: Span, idx: usize) -> Result<(), Error> {
        let object = self.pop(span)?;
        let member = match self.get_const(span, idx)? {
            Const::Str(name) => name,
            _ => unreachable!(),
        };
        let field = self
            .std
            .get(object.ty())
            .and_then(|t| t.field(member))
            .ok_or_else(|| {
                ErrorKind::NoSuchField {
                    ty: object.ty(),
                    field: member.clone(),
                }
                .at(span)
            })?;

        let value = field.call(&mut self.gc, object)?;
        self.stack.push(value);

        Ok(())
    }

    fn load_var(&self, span: Span, idx: usize) -> Result<Value, Error> {
        match self.vars.get(&idx) {
            Some(value) => Ok(value.clone()),
            None => Err(ErrorKind::InvalidVar(idx).at(span)),
        }
    }

    fn pop(&mut self, span: Span) -> Result<Value, Error> {
        self.stack
            .pop()
            .ok_or_else(|| ErrorKind::StackEmpty.at(span))
    }

    fn check_type(&self, span: Span, left: Type, right: Type) -> Result<(), Error> {
        if left != right {
            return Err(ErrorKind::TypeMismatch { left, right }.at(span));
        }

        Ok(())
    }

    fn jump_cond(&mut self, span: Span, idx: usize, b: bool) -> Result<(), Error> {
        let value = self.pop(span)?;
        self.check_type(span, value.ty(), Type::Bool)?;

        if value.bool() == b {
            self.stack.push(Value::new_bool(b));
            self.goto(idx);
        }

        Ok(())
    }

    fn concat(&mut self, lhs: Value, rhs: Value) -> Result<Value, Error> {
        let lhs = self.gc.read(lhs.heap())?;
        let rhs = self.gc.read(rhs.heap())?;
        let out = [lhs.buffer(), rhs.buffer()].concat();
        let handle = self.gc.alloc(HeapValue::Buffer(out));

        Ok(Value::new_str(handle))
    }

    fn make_array(&mut self, span: Span, size: usize) -> Result<(), Error> {
        let mut values = vec![];

        for _ in 0..size {
            let value = self.pop(span)?;
            values.push(value);
        }

        values.reverse();

        let handle = self.gc.alloc(HeapValue::Array(values));
        self.stack.push(Value::new_array(handle));

        Ok(())
    }

    fn load_elem(&mut self, span: Span) -> Result<(), Error> {
        let elem = self.pop(span)?;
        let array = self.pop(span)?;

        self.check_type(span, elem.ty(), Type::Int)?;
        self.check_type(span, array.ty(), Type::Array)?;

        let n = elem.int() as usize;
        let heap = self.gc.read(array.heap())?;

        match heap.array().get(n) {
            Some(elem) => {
                self.stack.push(elem.clone());
                Ok(())
            }
            None => Err(ErrorKind::IndexOutOfBounds(n).at(span)),
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

    fn eval(&mut self, span: Span, op: Op) -> Result<(), Error> {
        match op {
            Op::LoadConst(idx) => {
                let value = self.load_const(span, idx)?;
                self.stack.push(value);
            }
            Op::LoadMember(idx) => self.load_member(span, idx)?,
            Op::CompareOp(op) => {
                let rhs = self.pop(span)?;
                let lhs = self.pop(span)?;

                self.check_type(span, lhs.ty(), rhs.ty())?;

                let value = match op {
                    CompareOp::Eq => compare_op!(lhs == rhs),
                    CompareOp::Ne => compare_op!(lhs != rhs),
                    CompareOp::Lt => compare_op!(lhs < rhs),
                    CompareOp::Lte => compare_op!(lhs <= rhs),
                    CompareOp::Gt => compare_op!(lhs > rhs),
                    CompareOp::Gte => compare_op!(lhs >= rhs),
                };

                self.stack.push(value);
            }
            Op::BinaryOp(op) => {
                let rhs = self.pop(span)?;
                let lhs = self.pop(span)?;

                self.check_type(span, lhs.ty(), rhs.ty())?;

                let value = match op {
                    BinaryOp::Add if lhs.kind().is_number() => binary_op!(lhs + rhs),
                    BinaryOp::Sub if lhs.kind().is_number() => binary_op!(lhs - rhs),
                    BinaryOp::Mul if lhs.kind().is_number() => binary_op!(lhs * rhs),
                    BinaryOp::Div if lhs.kind().is_number() => binary_op!(lhs / rhs),
                    BinaryOp::BitwiseOr if lhs.ty() == Type::Int => binary_op_int!(lhs | rhs),
                    BinaryOp::BitwiseAnd if lhs.ty() == Type::Int => binary_op_int!(lhs & rhs),
                    BinaryOp::BitwiseXor if lhs.ty() == Type::Int => binary_op_int!(lhs ^ rhs),
                    BinaryOp::BitwiseXor if lhs.ty() == Type::Str => self.concat(lhs, rhs)?,
                    op => {
                        return Err(ErrorKind::UnsupportedOp {
                            left: lhs.ty(),
                            right: rhs.ty(),
                            op,
                        }
                        .at(span))
                    }
                };

                self.stack.push(value);
            }
            Op::Store(idx) => {
                let value = self.pop(span)?;
                self.vars.insert(idx, value);
            }
            Op::Load(idx) => {
                let value = self.load_var(span, idx)?;
                self.stack.push(value);
            }
            Op::Discard => {
                self.stack.pop();
            }
            // @TODO: implement when we have functions
            Op::Return => {}
            Op::JumpIfTrue(idx) => self.jump_cond(span, idx, true)?,
            Op::JumpIfFalse(idx) => self.jump_cond(span, idx, false)?,
            Op::MakeArray(len) => self.make_array(span, len)?,
            Op::LoadElement => self.load_elem(span)?,
            Op::UnaryNot => {
                let value = self.pop(span)?;
                self.check_type(span, value.ty(), Type::Bool)?;
                self.stack.push(Value::new_bool(!value.bool()));
            }
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<Option<Value>, Error> {
        while let Some((span, op)) = self.next() {
            self.eval(span, op)?;
        }

        Ok(self.stack.pop())
    }
}
