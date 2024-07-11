use std::collections::HashMap;

use crate::{
    codes::{BinaryOp, Const, Op},
    compiler::Module,
    lexer::Span,
    runtime::{Error, ErrorKind, Type, Value},
};

macro_rules! binary_op {
    ($lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            Type::Int => Value::Int($lhs.int() $op $rhs.int()),
            Type::Float => Value::Float($lhs.float() $op $rhs.float()),
            _ => unreachable!(),
        }
    };
}

macro_rules! binary_op_int {
    ($lhs:ident $op:tt $rhs:ident) => {
        match $lhs.ty() {
            Type::Int => Value::Int($lhs.int() $op $rhs.int()),
            _ => unreachable!(),
        }
    };
}

pub struct Vm {
    n: usize,
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
        }
    }

    fn next(&mut self) -> Option<(Span, Op)> {
        if let Some(code) = self.module.codes.get(self.n) {
            self.n += 1;
            Some((code.span, code.op))
        } else {
            None
        }
    }

    fn load_const(&self, span: Span, idx: usize) -> Result<Value, Error> {
        let const_ = self
            .module
            .consts
            .get(idx)
            .cloned()
            .ok_or_else(|| ErrorKind::InvalidConst(idx).at(span))?;

        match const_ {
            Const::Int(i) => Ok(Value::Int(i)),
            Const::Float(f) => Ok(Value::Float(f)),
            Const::Bool(b) => Ok(Value::Bool(b)),
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

    fn eval(&mut self, span: Span, op: Op) -> Result<(), Error> {
        match op {
            Op::LoadConst(idx) => {
                let value = self.load_const(span, idx)?;
                self.stack.push(value);
            }
            Op::BinaryOp(op) => {
                let rhs = self.pop_stack(span)?;
                let lhs = self.pop_stack(span)?;

                if lhs.ty() != rhs.ty() {
                    return Err(ErrorKind::BinaryTypeMismatch {
                        left: lhs.ty(),
                        right: rhs.ty(),
                    }
                    .at(span));
                }

                let value = match op {
                    BinaryOp::Add if lhs.is_number() => binary_op!(lhs + rhs),
                    BinaryOp::Sub if lhs.is_number() => binary_op!(lhs - rhs),
                    BinaryOp::Mul if lhs.is_number() => binary_op!(lhs * rhs),
                    BinaryOp::Div if lhs.is_number() => binary_op!(lhs / rhs),
                    BinaryOp::BitwiseOr if lhs.ty() == Type::Int => binary_op_int!(lhs | rhs),
                    BinaryOp::BitwiseAnd if lhs.ty() == Type::Int => binary_op_int!(lhs & rhs),
                    BinaryOp::BitwiseXor if lhs.ty() == Type::Int => binary_op_int!(lhs ^ rhs),
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
        }

        Ok(())
    }

    pub fn run(mut self) -> Result<Option<Value>, Error> {
        while let Some((span, op)) = self.next() {
            self.eval(span, op)?;
        }

        Ok(self.stack.pop())
    }
}
