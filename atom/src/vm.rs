use std::mem;

use bytes::Buf;

use crate::{
    bytecode::{Bytecode, Op, Serializable},
    error::SpannedError,
    gc::{Gc, Handle, Trace},
    instance::Instance,
    lexer::Span,
    runtime::{
        array::Array,
        class::{Class, Object},
        error::{Call, ErrorKind, RuntimeError},
        function::Fn,
        value::{self, TryIntoValue, Type, Value},
        Module,
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
    #[error("RuntimeError: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("FatalError: {0}")]
    Fatal(#[from] FatalError),
}

struct Frame {
    call_site: usize,
    offset: usize,
    returned: bool,
    instance_id: usize,
    function: Handle<Fn>,
    locals: Vec<Value>,
}

impl Trace for Frame {
    fn trace(&self, gc: &mut Gc) {
        gc.mark(&self.function);
        self.locals.iter().for_each(|v| v.trace(gc));
    }
}

impl Frame {
    pub fn new(instance_id: usize, call_site: usize, function: Handle<Fn>) -> Self {
        Self {
            call_site,
            function,
            offset: 0,
            instance_id,
            returned: false,
            locals: vec![],
        }
    }

    pub fn next(&mut self) -> Option<Bytecode> {
        if self.offset < self.function.body.len() {
            let bc = Bytecode::deserialize(&mut &self.function.body[self.offset..self.offset + 5]);
            self.offset += 8;
            return Some(bc);
        }

        None
    }

    /// Get the span given the assumption that we're already at the next bytecode
    pub fn span(&self) -> Span {
        self.span_at(self.offset - 3)
    }

    pub fn span_at(&self, offset: usize) -> Span {
        let mut tail = &self.function.body[offset..offset + 3];
        Span {
            offset: tail.get_uint(3) as u32,
        }
    }
}

macro_rules! impl_binary {
    ($(($name:ident: $op:tt)),+) => {
        impl<F: Ffi, const C: usize, const S: usize> Vm<F, C, S> {
            $(
                fn $name(&mut self) -> Result<(), Error> {
                    let (lhs, rhs) = self.stack.operands();
                    let value = if lhs.is_int() && rhs.is_int() {
                        (lhs.int() $op rhs.int()).into_value(&mut self.gc)?
                    } else if lhs.is_float() && rhs.is_float() {
                        (lhs.float() $op rhs.float()).into_value(&mut self.gc)?
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

pub trait Ffi {
    fn call(&mut self, name: &str, gc: &mut Gc, args: Vec<Value>) -> Result<Value, Error>;
}

pub struct Vm<F: Ffi, const C: usize, const S: usize> {
    gc: Gc,
    ffi: F,
    frame: Frame,
    stack: Stack<Value, S>,
    call_stack: Vec<Frame>,
    instances: Vec<Instance<C>>,
    #[cfg(feature = "profiler")]
    profiler: crate::profiler::VmProfiler,
}

impl<F: Ffi, const C: usize, const S: usize> Drop for Vm<F, C, S> {
    fn drop(&mut self) {
        self.gc.sweep();
    }
}

impl<F: Ffi, const C: usize, const S: usize> Vm<F, C, S> {
    pub fn with(mut module: Module, ffi: F) -> Result<Self, Error> {
        let mut gc = Gc::default();
        let func = gc.alloc(Fn {
            body: mem::take(&mut module.body),
            ..Fn::default()
        })?;

        let mut consts = [Value::NIL; C];

        for (i, const_) in mem::take(&mut module.consts).into_iter().enumerate() {
            consts[i] = const_.into_value(&mut gc)?;
        }

        let instance = Instance::new(0, module, consts);
        let frame = Frame::new(0, 0, func);

        Ok(Self {
            gc,
            ffi,
            frame,
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

    fn gc_tick(&mut self) {
        if !self.gc.ready() {
            return;
        }

        self.mark_sweep();
    }

    fn mark_sweep(&mut self) {
        self.frame.trace(&mut self.gc);
        self.call_stack
            .iter()
            .for_each(|frame| frame.trace(&mut self.gc));
        self.instances
            .iter()
            .for_each(|instance| instance.trace(&mut self.gc));
        self.stack
            .iter()
            .for_each(|value| value.trace(&mut self.gc));

        self.gc.sweep();
    }

    fn goto(&mut self, pos: u32) {
        self.frame.offset = pos as usize * 8;
    }

    fn load_member(&mut self, idx: u32) -> Result<(), Error> {
        let object = self.stack.pop();

        if object.is_object() {
            self.load_attr(object, idx as usize)
        } else {
            self.load_field(object, idx as usize)
        }
    }

    fn store_member(&mut self, member: u32) -> Result<(), Error> {
        let member = self.instances[self.frame.instance_id].consts[member as usize].str();
        let value = self.stack.pop();
        let object = self.stack.pop();

        self.check_type(object.ty(), Type::Object)?;

        let mut object = object.object();
        object.attrs.insert(member, value);

        Ok(())
    }

    fn load_attr(&mut self, object: Value, member: usize) -> Result<(), Error> {
        let member = self.instances[self.frame.instance_id].consts[member].str();
        let object = object.object();

        if object.attrs.is_empty() {
            return self.load_method(object, member.as_str());
        }

        match object.attrs.get(&member).copied() {
            Some(value) => {
                self.stack.push(value);
                Ok(())
            }
            None => self.load_method(object, member.as_str()),
        }
    }

    fn load_method(&mut self, object: Handle<Object>, member: &str) -> Result<(), Error> {
        let method = self.instances[self.frame.instance_id]
            .get_method(&mut self.gc, &object.class, member)?
            .ok_or_else(|| {
                ErrorKind::UnknownAttr {
                    class: object.class.clone(),
                    attribute: member.to_string(),
                }
                .at(self.frame.span())
            })?;

        self.stack.push(object.into());
        self.stack.push(method.into());

        Ok(())
    }

    fn load_field(&mut self, object: Value, member: usize) -> Result<(), Error> {
        let handle = self.instances[self.frame.instance_id].consts[member].str();
        self.load_field_by_name(object, handle.as_str())
    }

    fn load_field_by_name(&mut self, object: Value, member: &str) -> Result<(), Error> {
        let instance = &mut self.instances[self.frame.instance_id];

        if let Some(class) = instance.get_class_by_name(&mut self.gc, object.ty().name())? {
            if let Some(method) = instance.get_method(&mut self.gc, &class, member)? {
                self.stack.push(object);
                self.stack.push(method.into());
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

    fn load_class(&mut self, idx: u32) -> Result<(), Error> {
        let class = self.instances[self.frame.instance_id].get_class(&mut self.gc, idx as usize)?;
        self.stack.push(class.into());
        Ok(())
    }

    fn load_fn(&mut self, idx: u32) -> Result<(), Error> {
        let f = self.instances[self.frame.instance_id].get_fn(&mut self.gc, idx as usize)?;
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

    fn init_class(&mut self, class: Handle<Class>, arg_count: u32) -> Result<(), Error> {
        let init = class.init.clone();
        let object = Object::new(class);
        let handle = self.gc.alloc(object)?;

        if let Some(f) = init {
            self.stack.push(handle.into());
            self.stack.push(f.into());
            self.call(arg_count)?;
        } else {
            self.stack.push(handle.into());
        }

        self.gc_tick();

        Ok(())
    }

    fn call_extern(&mut self, idx: u32) -> Result<(), Error> {
        let name = self.instances[self.frame.instance_id].consts[idx as usize].str();
        let args = mem::take(&mut self.frame.locals);
        let return_value = self.ffi.call(name.as_ref(), &mut self.gc, args)?;

        self.frame.returned = true;
        self.stack.push(return_value);
        self.gc_tick();

        Ok(())
    }

    fn call_fn(&mut self, (fn_idx, arg_count): (u16, u16)) -> Result<(), Error> {
        let f = self.instances[self.frame.instance_id].get_fn(&mut self.gc, fn_idx as usize)?;
        self.fn_call(f, arg_count as u32)
    }

    fn call(&mut self, arg_count: u32) -> Result<(), Error> {
        let callee = self.stack.pop();

        match callee.ty() {
            Type::Fn => self.fn_call(callee.func(), arg_count),
            Type::Class => self.init_class(callee.class(), arg_count),
            ty => Err(ErrorKind::NotCallable(ty).at(self.frame.span()).into()),
        }
    }

    fn fn_call(&mut self, f: Handle<Fn>, arg_count: u32) -> Result<(), Error> {
        let is_method = f.method;
        let num_args = if is_method { arg_count + 1 } else { arg_count };

        if num_args != f.arg_count {
            return Err(ErrorKind::ArgCountMismatch { func: f, arg_count }
                .at(self.frame.span())
                .into());
        }

        let frame = Frame::new(f.instance_id, self.frame.offset, f);

        self.call_stack.push(mem::replace(&mut self.frame, frame));
        self.frame.locals = self.stack.slice_to_end(num_args as usize);

        // During a method call the receiver and arguments are swapped
        if is_method && self.frame.locals.len() > 1 {
            let end = self.frame.locals.len() - 1;
            self.frame.locals.swap(0, end);
        }

        self.gc_tick();

        Ok(())
    }

    fn tail_call(&mut self, arg_count: u32) -> Result<(), Error> {
        self.frame.offset = 0;
        self.frame.returned = false;
        self.frame.locals = self.stack.slice_to_end(arg_count as usize);
        self.gc_tick();

        Ok(())
    }

    fn make_array(&mut self, size: u32) -> Result<(), Error> {
        let array = self.stack.slice_to_end(size as usize);
        let array = Array::from_vec(&mut self.gc, array);
        let handle = self.gc.alloc(array)?;

        self.stack.push(handle.into());
        self.gc_tick();

        Ok(())
    }

    fn make_slice(&mut self, opts: u32) -> Result<(), Error> {
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

    fn import(&mut self, _idx: u32) -> Result<(), Error> {
        todo!()
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
            self.stack.push(Value::new_smallint(i));
        }

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

    fn bitwise_and(&mut self) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();
        if lhs.is_int() && rhs.is_int() {
            self.push_int(lhs.int() & rhs.int())
        } else {
            Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty()))
        }
    }

    fn bitwise_or(&mut self) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();
        if lhs.is_int() && rhs.is_int() {
            self.push_int(lhs.int() | rhs.int())
        } else {
            Err(self.unsupported(stringify!($op), lhs.ty(), rhs.ty()))
        }
    }

    fn bitwise_xor(&mut self) -> Result<(), Error> {
        let (lhs, rhs) = self.stack.operands();

        if lhs.is_int() && rhs.is_int() {
            self.push_int(lhs.int() ^ rhs.int())
        } else if matches!(lhs.ty(), Type::Array | Type::Str) && lhs.ty() == rhs.ty() {
            self.stack.push(rhs);
            self.load_field_by_name(lhs, "concat")?;
            self.call(1)?;

            Ok(())
        } else {
            Err(self.unsupported("^", lhs.ty(), rhs.ty()))
        }
    }

    fn store(&mut self, idx: u32) -> Result<(), Error> {
        let var = self.stack.pop();
        self.instances[self.frame.instance_id].vars.insert(idx, var);
        Ok(())
    }

    fn load(&mut self, idx: u32) {
        self.stack
            .push(self.instances[self.frame.instance_id].vars[&idx]);
    }

    fn load_const(&mut self, idx: u32) {
        self.stack
            .push(self.instances[self.frame.instance_id].consts[idx as usize]);
    }

    fn discard(&mut self) {
        let _ = self.stack.pop();
    }

    fn ret(&mut self) {
        self.frame.returned = true;
        self.frame.offset = self.frame.function.body.len();
    }

    fn eval(&mut self) -> Result<(), Error> {
        while let Some(bc) = self.frame.next() {
            #[cfg(feature = "profiler")]
            self.profiler.enter_instruction(bc.op);

            match bc.op {
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
                Op::LoadConst => self.load_const(bc.code),
                Op::LoadMember => self.load_member(bc.code)?,
                Op::StoreMember => self.store_member(bc.code)?,
                Op::LoadFn => self.load_fn(bc.code)?,
                Op::LoadClass => self.load_class(bc.code)?,
                Op::Store => self.store(bc.code)?,
                Op::Load => self.load(bc.code),
                Op::LoadArg => self.load_arg(bc.code),
                Op::Discard => self.discard(),
                Op::Return => self.ret(),
                Op::ReturnArg => self.return_arg(bc.code)?,
                Op::Call => self.call(bc.code)?,
                Op::CallFn => self.call_fn(bc.code2())?,
                Op::CallExtern => self.call_extern(bc.code)?,
                Op::TailCall => self.tail_call(bc.code)?,
                Op::Jump => self.goto(bc.code),
                Op::JumpIfFalse => self.jump_if_false(bc.code)?,
                Op::PushJumpIfTrue => self.jump_cond(bc.code, true)?,
                Op::PushJumpIfFalse => self.jump_cond(bc.code, false)?,
                Op::MakeArray => self.make_array(bc.code)?,
                Op::MakeSlice => self.make_slice(bc.code)?,
                Op::LoadElement => self.load_elem()?,
                Op::StoreElement => self.store_elem()?,
                Op::UnaryNot => self.not()?,
                Op::Import => self.import(bc.code)?,
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
                    .rev()
                    .map(|frame| {
                        Call::new(
                            if frame.call_site == 0 {
                                Span::default()
                            } else {
                                frame.span_at(frame.call_site)
                            },
                            frame.function,
                        )
                    })
                    .collect::<Vec<_>>();

                stack_trace.reverse();

                e.trace = Some(stack_trace);
                Error::Runtime(e)
            }
            Error::Fatal(e) => Error::Fatal(e),
        }
    }

    #[cfg(feature = "profiler")]
    pub fn profiler(mut self) -> crate::profiler::VmProfiler {
        mem::take(&mut self.profiler)
    }

    pub fn run(&mut self) -> Result<(), Error> {
        let mut start = || {
            #[cfg(feature = "profiler")]
            self.profiler.enter();

            loop {
                self.eval()?;

                match self.call_stack.pop() {
                    Some(frame) => {
                        if !self.frame.returned {
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
