use std::{
    collections::HashMap,
    mem,
    ops::{Add, Div, Mul, Rem, Sub},
    rc::Rc,
    time::Duration,
};

#[cfg(feature = "timings")]
use std::time::Instant;

use nohash_hasher::IntMap;
#[cfg(feature = "tracing")]
use tracing::{instrument, Level};
use wyhash2::WyHash;

use crate::{
    collections::Stack,
    error::{IntoSpanned, SpannedError},
    gc::{Gc, Handle, Trace},
    lexer::Span,
    opcode::{Op, Opcode},
    runtime::{
        array::Array,
        class::{Class, Object},
        error::{Call, ErrorKind, RuntimeError},
        function::Fn,
        module::Module,
        str::Str,
        value::{self, TryIntoValue, Type, Value},
        Atom,
    },
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

#[derive(Debug)]
struct Frame {
    call_site: Span,
    offset: usize,
    returned: bool,
    function: Handle<Fn>,
    locals: Vec<Value>,
    receiver: Option<Value>,
}

impl Frame {
    pub fn new(call_site: Span, function: Handle<Fn>) -> Self {
        Self {
            call_site,
            function,
            offset: 0,
            returned: false,
            locals: vec![],
            receiver: None,
        }
    }

    pub fn with_receiver(mut self, receiver: Value) -> Self {
        self.receiver = Some(receiver);
        self
    }

    pub fn next(&mut self) -> Option<Opcode> {
        if self.offset < self.function.body.len() {
            let code = Opcode::deserialize(&self.function.body[self.offset..self.offset + 16]);
            self.offset += 16;
            Some(code)
        } else {
            None
        }
    }
}

fn top_frame(module: &mut Module, gc: &mut Gc) -> Result<Frame, Error> {
    Ok(Frame::new(
        Span::default(),
        gc.alloc(Fn {
            body: mem::take(&mut module.body),
            ..Fn::default()
        })?,
    ))
}

pub type BoxedFn =
    Rc<Box<dyn std::ops::Fn(Atom, Vec<Value>) -> Result<Value, RuntimeError> + 'static>>;

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

#[derive(Default)]
struct Cache {
    functions: IntMap<usize, Handle<Fn>>,
    classes: IntMap<usize, Handle<Class>>,
    named_classes: HashMap<String, Handle<Class>, WyHash>,
    methods: IntMap<usize, HashMap<String, Handle<Fn>, WyHash>>,
}

impl Trace for Cache {
    fn trace(&self, gc: &mut Gc) {
        self.classes
            .values()
            .map(Handle::boxed)
            .chain(self.named_classes.values().map(Handle::boxed))
            .chain(self.functions.values().map(Handle::boxed))
            .chain(
                self.methods
                    .values()
                    .flat_map(|m| m.values().map(Handle::boxed)),
            )
            .for_each(|h| {
                h.trace(gc);
                gc.mark(h);
            });
    }
}

struct Context<const C: usize> {
    cache: Cache,
    module: Module,
    consts: [Value; C],
    vars: IntMap<usize, Value>,
}

impl<const C: usize> Trace for Context<C> {
    fn trace(&self, gc: &mut Gc) {
        self.consts
            .iter()
            .chain(self.vars.values())
            .for_each(|value| value.trace(gc));

        self.cache.trace(gc);
    }
}

impl<const C: usize> Context<C> {
    fn load_class_by_name(
        &mut self,
        gc: &mut Gc,
        name: &str,
    ) -> Result<Option<Handle<Class>>, Error> {
        if let Some(class) = self.cache.named_classes.get(name) {
            return Ok(Some(Handle::clone(class)));
        }

        let Some(class) = self
            .module
            .classes
            .iter()
            .find(|class| class.name == name)
            .cloned()
        else {
            return Ok(None);
        };

        let handle = gc.alloc(class.clone())?;
        self.cache
            .named_classes
            .insert(name.to_string(), Handle::clone(&handle));

        Ok(Some(handle))
    }

    fn load_class(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Class>, Error> {
        if let Some(class) = self.cache.classes.get(&idx).cloned() {
            return Ok(class);
        }

        let class = self.module.classes[idx].clone();
        let handle = gc.alloc(class)?;

        self.cache.classes.insert(idx, Handle::clone(&handle));

        Ok(handle)
    }

    fn load_fn(&mut self, gc: &mut Gc, idx: usize) -> Result<Handle<Fn>, Error> {
        if let Some(f) = self.cache.functions.get(&idx).cloned() {
            return Ok(f);
        }

        let f = self.module.functions[idx].clone();
        let handle = gc.alloc(f)?;

        self.cache.functions.insert(idx, Handle::clone(&handle));

        Ok(handle)
    }

    fn load_method(
        &mut self,
        gc: &mut Gc,
        class: &Handle<Class>,
        name: &str,
    ) -> Result<Option<Handle<Fn>>, Error> {
        if let Some(entry) = self.cache.methods.get(&class.addr()) {
            if let Some(method) = entry.get(name) {
                return Ok(Some(Handle::clone(method)));
            }
        }

        let Some(method) = class.methods.get(name).cloned() else {
            return Ok(None);
        };

        let handle = gc.alloc(method.clone())?;
        let methods = self.cache.methods.entry(class.addr()).or_default();

        methods.insert(name.to_string(), Handle::clone(&handle));

        Ok(Some(handle))
    }
}

pub struct Vm<L: DynamicLinker, const S: usize, const C: usize> {
    gc: Gc,
    linker: L,
    span: Span,
    frame: Frame,
    context: Context<C>,
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
    pub fn with_module(mut module: Module, linker: L) -> Result<Self, Error> {
        let mut gc = Gc::default();
        let mut consts = [Value::NIL; C];

        for (i, const_) in mem::take(&mut module.consts).into_iter().enumerate() {
            consts[i] = const_.into_value(&mut gc)?;
        }

        let frame = top_frame(&mut module, &mut gc)?;
        let context = Context {
            cache: Cache::default(),
            module,
            consts,
            vars: IntMap::default(),
        };

        Ok(Self {
            gc,
            linker,
            frame,
            context,
            span: Span::default(),
            call_stack: vec![],
            timing: HashMap::default(),
            stack: Stack::default(),
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
        self.context.trace(&mut self.gc);
        self.stack
            .iter()
            .chain(
                self.call_stack
                    .iter()
                    .flat_map(|frame| frame.locals.iter().chain(frame.receiver.as_ref())),
            )
            .for_each(|value| value.trace(&mut self.gc));

        self.gc.sweep();
    }

    fn goto(&mut self, pos: usize) {
        let offset = pos * 16;
        self.frame.offset = offset;
        self.span = Span::deserialize(&self.frame.function.body[offset + 8..offset + 16]);
    }

    fn load_member(&mut self, idx: usize) -> Result<(), Error> {
        let object = self.stack.pop();

        match object.ty() {
            Type::Object => self.load_attr(object, idx),
            _ => self.load_field(object, idx),
        }
    }

    fn store_member(&mut self, member: usize) -> Result<(), Error> {
        let member = self.context.consts[member].str();
        let value = self.stack.pop();
        let object = self.stack.pop();

        self.check_type(object.ty(), Type::Object)?;

        let mut object = object.object();
        object.attrs.insert(member, value);

        Ok(())
    }

    fn load_attr(&mut self, object: Value, member: usize) -> Result<(), Error> {
        let member = self.context.consts[member].str();
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

        match self
            .context
            .load_method(&mut self.gc, &object.class, member)?
        {
            Some(method) => {
                self.stack.push(object.into());
                self.stack.push(method.into());

                Ok(())
            }
            None => Err(ErrorKind::UnknownAttr {
                class: object.class.clone(),
                attribute: member.to_string(),
            }
            .at(self.span)
            .into()),
        }
    }

    fn load_field(&mut self, object: Value, member: usize) -> Result<(), Error> {
        let member_handle = self.context.consts[member].str();
        let member = member_handle.as_str();
        let class = self
            .context
            .load_class_by_name(&mut self.gc, object.ty().name())?;

        if let Some(method) = class.and_then(|class| class.methods.get(member).cloned()) {
            let method = self.gc.alloc(method)?;
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

    fn load_class(&mut self, idx: usize) -> Result<(), Error> {
        let class = self.context.load_class(&mut self.gc, idx)?;
        self.stack.push(class.into());
        Ok(())
    }

    fn load_fn(&mut self, idx: usize) -> Result<(), Error> {
        let f = self.context.load_fn(&mut self.gc, idx)?;
        self.stack.push(f.into());
        Ok(())
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

    fn jump_if_false(&mut self, idx: usize) -> Result<(), Error> {
        let value = self.stack.pop();

        if value == Value::FALSE {
            self.goto(idx);
            return Ok(());
        }

        self.check_type(value.ty(), Type::Bool)
    }

    fn jump_cond(&mut self, idx: usize, cond: bool) -> Result<(), Error> {
        let value = self.stack.pop();
        self.check_type(value.ty(), Type::Bool)?;

        if value.bool() == cond {
            self.stack.push(cond.into());
            self.goto(idx);
        }

        Ok(())
    }

    fn init_class(&mut self, class: Handle<Class>, arg_count: usize) -> Result<(), Error> {
        let init_fn = class.methods.get("init").cloned();
        let object = Object::new(class);
        let handle = self.gc.alloc(object)?;

        if let Some(f) = init_fn {
            let f = self.gc.alloc(f)?;
            self.stack.push(handle.into());
            self.stack.push(f.into());
            self.call(arg_count)?;
        } else {
            self.stack.push(handle.into());
        }

        self.gc_tick();

        Ok(())
    }

    fn call_extern(&mut self, idx: usize) -> Result<(), Error> {
        let name = self.context.consts[idx].str();
        let handler = self
            .linker
            .resolve(name.as_ref())
            .ok_or_else(|| FatalErrorKind::InvalidExternFn(name.to_string()).at(self.span))?;
        let args = mem::take(&mut self.frame.locals);
        let mut atom = Atom::new(&mut self.gc).with_span(self.span);

        if let Some(receiver) = self.frame.receiver {
            atom = atom.with_receiver(receiver);
        }

        let return_value = (handler)(atom, args)?;

        self.frame.returned = true;
        self.stack.push(return_value);
        self.gc_tick();

        Ok(())
    }

    fn call_fn(&mut self, (fn_idx, arg_count): (u32, u32)) -> Result<(), Error> {
        let f = self.context.load_fn(&mut self.gc, fn_idx as usize)?;
        self.fn_call(f, arg_count as usize)
    }

    fn call(&mut self, arg_count: usize) -> Result<(), Error> {
        let callee = self.stack.pop();

        match callee.ty() {
            Type::Fn => self.fn_call(callee.func(), arg_count),
            Type::Class => self.init_class(callee.class(), arg_count),
            ty => Err(ErrorKind::NotCallable(ty).at(self.span).into()),
        }
    }

    fn fn_call(&mut self, f: Handle<Fn>, arg_count: usize) -> Result<(), Error> {
        if arg_count != f.arg_count {
            return Err(ErrorKind::ArgCountMismatch { func: f, arg_count }
                .at(self.span)
                .into());
        }

        let mut frame = Frame::new(self.span, f);

        if frame.function.method {
            frame = frame.with_receiver(self.stack.pop());
        }

        self.call_stack.push(mem::replace(&mut self.frame, frame));
        self.frame.locals = self.stack.split_to_vec(arg_count);
        self.gc_tick();

        Ok(())
    }

    fn tail_call(&mut self, arg_count: usize) -> Result<(), Error> {
        self.frame.offset = 0;
        self.frame.returned = false;
        self.frame.locals = self.stack.split_to_vec(arg_count);
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
        let values = self.stack.split_to_vec(size);
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
            self.stack.push(Value::new_smallint(i));
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
        self.context.vars.insert(idx, self.stack.pop());
    }

    fn load(&mut self, idx: usize) {
        self.stack
            .push(self.context.vars.get(&idx).copied().unwrap_or_default());
    }

    fn load_const(&mut self, idx: usize) {
        self.stack.push(self.context.consts[idx]);
    }

    fn discard(&mut self) {
        self.stack.pop();
    }

    fn ret(&mut self) {
        self.frame.returned = true;
        self.frame.offset = self.frame.function.body.len();
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
                Op::LoadFn => self.load_fn(code.code())?,
                Op::LoadClass => self.load_class(code.code())?,
                Op::Store => self.store(code.code()),
                Op::Load => self.load(code.code()),
                Op::LoadArg => self.load_arg(code.code()),
                Op::LoadSelf => self.load_self()?,
                Op::Discard => self.discard(),
                Op::Return => self.ret(),
                Op::ReturnArg => self.return_arg(code.code()),
                Op::Call => self.call(code.code())?,
                Op::CallFn => self.call_fn(code.code2())?,
                Op::CallExtern => self.call_extern(code.code())?,
                Op::TailCall => self.tail_call(code.code())?,
                Op::Jump => self.goto(code.code()),
                Op::JumpIfFalse => self.jump_if_false(code.code())?,
                Op::PushJumpIfTrue => self.jump_cond(code.code(), true)?,
                Op::PushJumpIfFalse => self.jump_cond(code.code(), false)?,
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
                    .map(|s| Call::new(s.call_site, s.function))
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
