use std::collections::HashMap;

use broom::Heap;

use crate::{
    codes::{BinaryOp, CompareOp, Const, Op},
    compiler::Module,
    lexer::Span,
    runtime::{Error, ErrorKind, HeapValue, Type, Value, ValueKind},
};

macro_rules! compare_op {
    ($lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            Type::Int => Value::bool($lhs.kind().int() $op $rhs.kind().int()),
            Type::Float => Value::bool($lhs.kind().float() $op $rhs.kind().float()),
            Type::Bool => Value::bool($lhs.kind().bool() $op $rhs.kind().bool()),
            _ => unreachable!()
        }
    };
}

macro_rules! binary_op {
    ($lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            Type::Int => Value::int($lhs.kind().int() $op $rhs.kind().int()),
            Type::Float => Value::float($lhs.kind().float() $op $rhs.kind().float()),
            _ => unreachable!(),
        }
    };
}

macro_rules! binary_op_int {
    ($lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            Type::Int => Value::int($lhs.kind().int() $op $rhs.kind().int()),
            _ => unreachable!(),
        }
    };
}

pub struct Vm {
    n: usize,
    heap: Heap<HeapValue>,
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
            vars: HashMap::new(),
            heap: Heap::new(),
        }
    }

    fn goto(&mut self, n: usize) {
        self.n = n;
    }

    fn next(&mut self) -> Option<(Span, Op)> {
        if let Some(code) = self.module.codes.get(self.n) {
            self.n += 1;
            Some((code.span, code.op))
        } else {
            None
        }
    }

    fn load_const(&mut self, span: Span, idx: usize) -> Result<Value, Error> {
        let const_ = self
            .module
            .consts
            .get(idx)
            .cloned()
            .ok_or_else(|| ErrorKind::InvalidConst(idx).at(span))?;

        match const_ {
            Const::Int(i) => Ok(Value::int(i)),
            Const::Float(f) => Ok(Value::float(f)),
            Const::Bool(b) => Ok(Value::bool(b)),
            Const::Str(s) => {
                let value = self.heap.insert(HeapValue::Buffer(s.into_bytes()));
                Ok(Value::str(value.handle()))
            }
        }
    }

    fn load_var(&self, span: Span, idx: usize) -> Result<Value, Error> {
        match self.vars.get(&idx) {
            Some(value) => Ok(value.clone()),
            None => Err(ErrorKind::InvalidVar(idx).at(span)),
        }
    }

    fn pop_stack(&mut self, span: Span) -> Result<Value, Error> {
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
        let value = self.pop_stack(span)?;
        self.check_type(span, value.ty(), Type::Bool)?;

        if value.kind().bool() == b {
            self.stack.push(Value::bool(b));
            self.goto(idx);
        }

        Ok(())
    }

    fn concat(&mut self, span: Span, lhs: Value, rhs: Value) -> Result<Value, Error> {
        let left = match self.heap.get(lhs.kind().heap()) {
            Some(HeapValue::Buffer(b)) => b,
            None => return Err(ErrorKind::Segfault.at(span)),
            _ => unreachable!(),
        };
        let right = match self.heap.get(rhs.kind().heap()) {
            Some(HeapValue::Buffer(b)) => b,
            _ => unreachable!(),
        };

        let out = [left.as_slice(), right.as_slice()].concat();
        let value = self.heap.insert(HeapValue::Buffer(out));

        Ok(Value::str(value.handle()))
    }

    fn make_array(&mut self, span: Span, size: usize) -> Result<(), Error> {
        let mut values = vec![];

        for _ in 0..size {
            let value = self.pop_stack(span)?;
            values.push(value);
        }

        values.reverse();

        let value = self.heap.insert(HeapValue::Array(values));
        self.stack.push(Value::array(value.handle()));

        Ok(())
    }

    fn load_elem(&mut self, span: Span) -> Result<(), Error> {
        let elem = self.pop_stack(span)?;
        let array = self.pop_stack(span)?;

        self.check_type(span, elem.ty(), Type::Int)?;
        self.check_type(span, array.ty(), Type::Array)?;

        let n = elem.kind().int() as usize;
        let heap = self
            .heap
            .get(array.kind().heap())
            .ok_or_else(|| ErrorKind::Segfault.at(span))?;

        match heap {
            HeapValue::Array(array) => match array.get(n) {
                Some(elem) => {
                    self.stack.push(elem.clone());
                    Ok(())
                }
                None => Err(ErrorKind::IndexOutOfBounds(n).at(span)),
            },
            _ => unreachable!(),
        }
    }

    pub fn repr(&self, value: &Value) -> String {
        let ty = value.ty();

        match value.kind_ref() {
            ValueKind::Heap(handle) => match self.heap.get(*handle) {
                Some(value) => match value {
                    HeapValue::Buffer(buff) => match ty {
                        Type::Str => format!("\"{}\"", String::from_utf8_lossy(&buff)),
                        _ => unreachable!(),
                    },
                    HeapValue::Array(items) => {
                        let mut s = String::from("[");

                        for (i, item) in items.iter().enumerate() {
                            if i > 0 {
                                s.push_str(", ");
                            }

                            s.push_str(&self.repr(item));
                        }

                        s.push(']');
                        s
                    }
                },
                None => "<nilptr>".to_string(),
            },
            kind => format!("{kind}"),
        }
    }

    fn eval(&mut self, span: Span, op: Op) -> Result<(), Error> {
        match op {
            Op::LoadConst(idx) => {
                let value = self.load_const(span, idx)?;
                self.stack.push(value);
            }
            Op::CompareOp(op) => {
                let rhs = self.pop_stack(span)?;
                let lhs = self.pop_stack(span)?;

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
                let rhs = self.pop_stack(span)?;
                let lhs = self.pop_stack(span)?;

                self.check_type(span, lhs.ty(), rhs.ty())?;

                let value = match op {
                    BinaryOp::Add if lhs.kind_ref().is_number() => binary_op!(lhs + rhs),
                    BinaryOp::Sub if lhs.kind_ref().is_number() => binary_op!(lhs - rhs),
                    BinaryOp::Mul if lhs.kind_ref().is_number() => binary_op!(lhs * rhs),
                    BinaryOp::Div if lhs.kind_ref().is_number() => binary_op!(lhs / rhs),
                    BinaryOp::BitwiseOr if lhs.ty() == Type::Int => binary_op_int!(lhs | rhs),
                    BinaryOp::BitwiseAnd if lhs.ty() == Type::Int => binary_op_int!(lhs & rhs),
                    BinaryOp::BitwiseXor if lhs.ty() == Type::Int => binary_op_int!(lhs ^ rhs),
                    BinaryOp::BitwiseXor if lhs.ty() == Type::Str => self.concat(span, lhs, rhs)?,
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
                let value = self.pop_stack(span)?;
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
