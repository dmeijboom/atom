use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};

use strum::IntoEnumIterator;

use crate::compiler;
use crate::compiler::ir::{Code, Label};
use crate::runtime::types::{
    make_array, unwrap_or_clone_inner, AtomArray, AtomNil, AtomRef, AtomRefMut, AtomString, Class,
    Closure, Fn, FnKind, Input, Interface, Method, Object, Receiver, Symbol, Value, ValueType,
};
use crate::runtime::{stdlib, AtomApi, Convert, ErrorKind, Result, RuntimeError};
use crate::vm::global_label::GlobalLabel;
use crate::vm::module::Global;
use crate::vm::Module;

use super::call_stack::{CallStack, StackFrame, Target};
use super::module::ModuleId;
use super::module_cache::ModuleCache;
use super::stack::Stack;

#[derive(PartialEq, PartialOrd)]
enum NumberType {
    Int,
    Uint,
    Float,
    Unknown,
}

macro_rules! write_array_ptr {
    ($array:expr, $index:expr, $value:expr) => {
        ($array[$index].as_mut_ptr() as *mut Value).write($value)
    };
}

macro_rules! impl_op {
    ($vm:expr, int_only: $opname:ident) => {{
        let right = $vm.stack.pop();
        let left = $vm.stack.pop();

        $vm.stack.push(match left {
            Value::Int(val) => {
                let right: i64 = right.convert()?;
                Value::Int(val.$opname(&right))
            }
            Value::Uint(val) => {
                let right: u64 = right.convert()?;
                Value::Uint(val.$opname(&right))
            }
            _ => {
                return Err(RuntimeError::new(
                    ErrorKind::TypeError,
                    format!(
                        "expected '{}', found '{}' in: {}",
                        right.get_type().name(),
                        left.get_type().name(),
                        stringify!($opname)
                    ),
                ));
            }
        });

        Ok(())
    }};

    ($vm:expr, compare: $opname:ident) => {{
        let right = $vm.stack.pop();
        let left = $vm.stack.pop();

        $vm.stack.push(match left {
            Value::Int(val) => {
                let right: i64 = right.convert()?;
                Value::Bool(val.$opname(&right))
            }
            Value::Uint(val) => {
                let right: u64 = right.convert()?;
                Value::Bool(val.$opname(&right))
            }
            Value::Float(val) => {
                let right: f64 = right.convert()?;
                Value::Bool(val.$opname(&right))
            }
            _ => {
                return Err(RuntimeError::new(
                    ErrorKind::TypeError,
                    format!(
                        "expected '{}', found '{}' in: {}",
                        right.get_type().name(),
                        left.get_type().name(),
                        stringify!($opname)
                    ),
                ));
            }
        });

        Ok(())
    }};

    ($vm:expr, $opname:ident) => {{
        let right = $vm.stack.pop();
        let left = $vm.stack.pop();

        $vm.stack.push(match left {
            Value::Int(val) => {
                let right: i64 = right.convert()?;
                Value::Int(val.$opname(right))
            }
            Value::Uint(val) => {
                let right: u64 = right.convert()?;
                Value::Uint(val.$opname(right))
            }
            Value::Float(val) => {
                let right: f64 = right.convert()?;
                Value::Float(val.$opname(right))
            }
            _ => {
                return Err(RuntimeError::new(
                    ErrorKind::TypeError,
                    format!(
                        "expected '{}', found '{}' in: {}",
                        right.get_type().name(),
                        left.get_type().name(),
                        stringify!($opname)
                    ),
                ));
            }
        });

        Ok(())
    }};
}

enum Flow {
    Continue,
    Return(bool),
}

pub struct Machine {
    stack: Stack,
    call_stack: CallStack,
    module_cache: ModuleCache,
    try_stack: Vec<GlobalLabel>,
    type_classes: Vec<AtomRef<Class>>,
}

impl Machine {
    pub fn new() -> Result<Self> {
        let mut vm = Self {
            stack: Stack::new(),
            try_stack: vec![],
            call_stack: CallStack::new(),
            module_cache: ModuleCache::new(),
            type_classes: vec![],
        };

        vm.module_cache.add_external_hook(stdlib::hook);

        Ok(vm)
    }

    pub fn get_module(&self, name: &str) -> Option<&Module> {
        self.module_cache.get_module(name).ok()
    }

    pub fn register_module(&mut self, module: compiler::Module) -> Result<()> {
        let is_core = module.name == "std.core";

        self.module_cache.register(module)?;

        if is_core {
            // Fill the type classes cache
            for value_type in ValueType::iter() {
                if value_type == ValueType::Void {
                    continue;
                }

                self.type_classes
                    .push(self.module_cache.get_class("std.core", value_type.name())?);
            }
        }

        Ok(())
    }

    fn get_class<'c>(&'c self, value: &'c Value) -> Result<&'c AtomRef<Class>> {
        match value {
            Value::Object(object) => Ok(&object.class),
            _ => {
                // We need to get `index - 1` as the Void type is omitted in the type classes cache
                self.type_classes
                    .get(value.get_type() as usize - 1)
                    .ok_or_else(|| {
                        RuntimeError::new(
                            ErrorKind::FatalError,
                            format!("no class found for type: {}", value.get_type().name()),
                        )
                    })
            }
        }
    }

    fn validate_class(&self, class: &Class, interface: &Interface) -> bool {
        interface
            .functions
            .iter()
            .all(|name| class.methods.contains_key(name))
    }

    fn eval_tail_call(&mut self, arg_count: usize) -> Result<()> {
        // Reset locals, we can do this quite easily as the first n-locals are the arguments
        let frame = self.call_stack.current_mut();

        frame.store_return_value = true;
        frame.return_addr.push(frame.position);
        frame.position = 0;
        frame.locals.truncate(arg_count);

        for i in 0..arg_count {
            // We need to push the arguments in reverse order
            frame.locals[arg_count - (i + 1)] = self.stack.pop();
        }

        Ok(())
    }

    fn eval_call(&mut self, arg_count: usize, store_return_value: bool) -> Result<()> {
        let value = self.stack.pop();

        match value {
            Value::Fn(func) => self.eval_func(Target::Fn(func), store_return_value, arg_count),
            Value::Closure(closure) => {
                self.eval_func(Target::Closure(closure), store_return_value, arg_count)
            }
            Value::Method(method) => self.eval_method_call(method, arg_count, store_return_value),
            _ => Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!("type '{}' is not callable", value.get_type().name()),
            )),
        }
    }

    fn eval_new_instance(&mut self, keywords: [usize; 2], field_count: usize) -> Result<()> {
        let class: AtomRef<Class> = self.stack.pop().convert()?;
        let keywords = keywords[0]..keywords[1];

        if keywords.len() != field_count {
            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!(
                    "unable to create instance of '{}' with non-keyword arguments",
                    class.as_ref()
                ),
            ));
        }

        if keywords.len() != class.fields.len() {
            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!(
                    "unable to create instance of '{}' with incorrect number of fields",
                    class.as_ref()
                ),
            ));
        }

        let function = &self.call_stack.current().function;
        let fields = make_array(keywords.len(), |array| unsafe {
            for segment_id in keywords.rev() {
                let name = function.ir.get_data(segment_id);
                let (field_id, _) = class.get_field(name).ok_or_else(|| {
                    RuntimeError::new(
                        ErrorKind::FatalError,
                        format!("invalid field {}.{}", class.as_ref(), name),
                    )
                })?;

                write_array_ptr!(array, field_id, self.stack.pop());
            }

            Ok(())
        })?;

        self.stack
            .push(Value::Object(AtomRefMut::new(AtomRef::new(Object::new(
                class,
                AtomRefMut::new(fields),
            )))));

        Ok(())
    }

    fn eval_method_call(
        &mut self,
        method: AtomRef<Method>,
        arg_count: usize,
        store_return_value: bool,
    ) -> Result<()> {
        match &method.receiver {
            Receiver::Unbound if !method.is_static => Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!(
                    "unable to call non-static '{}(..)' with unbound receiver",
                    method.as_ref()
                ),
            )),
            Receiver::Bound(_) if method.is_static => Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!(
                    "unable to call static '{}(..)' on instance",
                    method.as_ref()
                ),
            )),
            _ => {
                self.eval_func(Target::Method(method), store_return_value, arg_count)?;

                Ok(())
            }
        }
    }

    fn eval_func(
        &mut self,
        target: Target,
        store_return_value: bool,
        arg_count: usize,
    ) -> Result<()> {
        let (func, values) = match &target {
            Target::Fn(func) => (func, None),
            Target::Method(method) => (&method.func, None),
            Target::Closure(closure) => (&closure.func, Some(closure.values.clone())),
        };

        match func.kind {
            FnKind::Native => {
                if arg_count != func.args.len() {
                    return Err(RuntimeError::new(
                        ErrorKind::FatalError,
                        format!(
                            "invalid argument count for target: {}(...) (expected {}, not {})",
                            target,
                            func.args.len(),
                            arg_count,
                        ),
                    ));
                }

                let locals_size = func.ir.get_locals_size().unwrap_or(arg_count);

                match self.call_stack.recycle() {
                    Some(mut frame) => {
                        frame.reset(target, store_return_value);
                        self.stack.pop_many(arg_count, &mut frame.locals)?;

                        if let Some(values) = values {
                            frame.locals = vec![values, frame.locals].concat();
                        }

                        self.call_stack.push(frame);
                    }
                    None => {
                        let mut locals = Vec::with_capacity(locals_size);
                        self.stack.pop_many(arg_count, &mut locals)?;

                        let locals = if let Some(values) = values {
                            vec![values, locals].concat()
                        } else {
                            locals
                        };

                        self.call_stack
                            .push(StackFrame::new(target, store_return_value, locals));
                    }
                }

                Ok(())
            }
            FnKind::External(closure) => {
                self.call_stack
                    .push(StackFrame::new(target, store_return_value, vec![]));

                let mut values = Vec::with_capacity(arg_count);
                self.stack.pop_many(arg_count, &mut values)?;

                let result = closure(Input::new(self, values));

                self.call_stack.pop();

                match result {
                    Ok(return_value) if store_return_value => {
                        self.stack.push(return_value);

                        Ok(())
                    }
                    Ok(_) => Ok(()),
                    Err(err) => Err(err),
                }
            }
        }
    }

    fn eval_const<T: Into<Value>>(&mut self, data: T) {
        self.stack.push(data.into());
    }

    fn eval_const_byte(&mut self, byte: u8) {
        self.stack.push(Value::Byte(byte));
    }

    fn eval_const_string(&mut self, segment_id: usize) {
        let s = self.call_stack.current().function.ir.get_data(segment_id);

        self.stack.push(Value::String(AtomString::clone(s)));
    }

    fn eval_symbol(&mut self, segment_id: usize) {
        let s = self.call_stack.current().function.ir.get_data(segment_id);

        self.stack.push(if s == "nil" {
            Value::Nil(AtomNil)
        } else {
            Value::Symbol(Symbol::new(AtomString::clone(s)))
        });
    }

    fn eval_current(&mut self) -> Result<Flow> {
        let frame = self.call_stack.current_mut();
        let code = match frame.function.ir.get(frame.position) {
            None => return Ok(Flow::Return(false)),
            Some(code) => code.clone(),
        };

        frame.position += 1;

        match code {
            Code::ConstInt(val) => self.eval_const(Value::Int(val)),
            Code::ConstUint(val) => self.eval_const(Value::Uint(val)),
            Code::ConstBool(val) => self.eval_const(val),
            Code::ConstSymbol(segment_id) => self.eval_symbol(segment_id),
            Code::ConstFloat(val) => self.eval_const(val),
            Code::ConstChar(val) => self.eval_const(val),
            Code::ConstByte(val) => self.eval_const_byte(val),
            Code::ConstString(segment_id) => self.eval_const_string(segment_id),
            Code::MakeRange => self.eval_make_range()?,
            Code::MakeTuple(len) => self.eval_make_tuple(len)?,
            Code::GetType => self.eval_get_type()?,
            Code::MakeArray(len) => self.eval_make_array(len)?,
            Code::MakeTemplate(len) => self.eval_make_template(len)?,
            Code::MakeRef => self.eval_make_ref(),
            Code::MakeClosure(fn_id) => self.eval_make_closure(fn_id)?,
            Code::Discard => self.stack.delete()?,
            Code::Return => return Ok(Flow::Return(true)),
            Code::Deref => self.eval_deref()?,
            Code::LogicalAnd => self.eval_logical_and()?,
            Code::ArithmeticAdd => self.eval_arithmetic_add()?,
            Code::ArithmeticSub => self.eval_arithmetic_sub()?,
            Code::ArithmeticMul => self.eval_arithmetic_mul()?,
            Code::ArithmeticDiv => self.eval_arithmetic_div()?,
            Code::ArithmeticExp => self.eval_arithmetic_exp()?,
            Code::ArithmeticMod => self.eval_arithmetic_mod()?,
            Code::ArithmeticBitOr => self.eval_arithmetic_bit_or()?,
            Code::ArithmeticBitAnd => self.eval_arithmetic_bit_and()?,
            Code::ArithmeticBitXor => self.eval_arithmetic_bit_xor()?,
            Code::ArithmeticBitShiftLeft => self.eval_arithmetic_bit_shift_left()?,
            Code::ArithmeticBitShiftRight => self.eval_arithmetic_bit_shift_right()?,
            Code::ComparisonEq => self.eval_comparison_eq()?,
            Code::ComparisonNeq => self.eval_comparison_neq()?,
            Code::ComparisonGt => self.eval_comparison_gt()?,
            Code::ComparisonGte => self.eval_comparison_gte()?,
            Code::ComparisonLt => self.eval_comparison_lt()?,
            Code::ComparisonLte => self.eval_comparison_lte()?,
            Code::AssertIsType => self.eval_assert_is_type()?,
            Code::Cast(segment_id) => self.eval_cast(segment_id)?,
            Code::Call(arg_count) => self.eval_call(arg_count, true)?,
            Code::CallVoid(arg_count) => self.eval_call(arg_count, false)?,
            Code::TailCall(arg_count) => self.eval_tail_call(arg_count)?,
            Code::Store(id) => self.eval_store(id, false)?,
            Code::StoreMut(id) => self.eval_store(id, true)?,
            Code::StoreTryOk => self.eval_store_try_ok()?,
            Code::LoadIndex => self.eval_load_index(false)?,
            Code::MakeSlice => self.eval_make_slice()?,
            Code::MakeInstance((keywords, field_count)) => {
                self.eval_new_instance(keywords, field_count)?
            }
            Code::StoreIndex => self.eval_store_index()?,
            Code::LoadMember(segment_id) => self.eval_load_member(segment_id)?,
            Code::StoreMember(segment_id) => self.eval_store_member(segment_id)?,
            Code::Load(id) => self.eval_load(id)?,
            Code::LoadReceiver => self.eval_load_receiver()?,
            Code::LoadGlobal(id) => self.eval_load_global(id)?,
            Code::LoadFn(id) => self.eval_load_fn(id)?,
            Code::LoadClass(id) => self.eval_load_class(id)?,
            Code::LoadInterface(id) => self.eval_load_interface(id)?,
            Code::LoadTarget => self.eval_load_target()?,
            Code::SetLabel(_) => {}
            Code::Branch((true_label, false_label)) => {
                let label = self.eval_branch(true_label, false_label)?;

                self.jump_to_label(label)?;
            }
            Code::Jump(label) => self.jump_to_label(label)?,
            Code::JumpIfTrue(label) => {
                if self.eval_jump_if_true()? {
                    self.jump_to_label(label)?;
                }
            }
            Code::JumpOnError(label) => {
                self.try_stack
                    .push(GlobalLabel::new(self.call_stack.current_id(), label));
            }
        };

        Ok(Flow::Continue)
    }

    fn eval_make_range(&mut self) -> Result<()> {
        let to: i64 = self
            .stack
            .pop()
            .convert()
            .map_err(|e: RuntimeError| e.with_context("unable to construct Range"))?;
        let from: i64 = self
            .stack
            .pop()
            .convert()
            .map_err(|e: RuntimeError| e.with_context("unable to construct Range"))?;
        let fields: AtomArray<Value> = AtomRef::from([Value::Int(from), Value::Int(to)]);
        let object = Object::new(
            self.find_class("std.core", "Range")?,
            AtomRefMut::new(fields),
        );

        self.stack
            .push(Value::Object(AtomRefMut::new(AtomRef::new(object))));

        Ok(())
    }

    fn eval_make_tuple(&mut self, len: usize) -> Result<()> {
        let values = make_array(len, |array| unsafe {
            for i in 0..len {
                write_array_ptr!(array, len - (i + 1), self.stack.pop());
            }

            Ok(())
        })?;

        self.stack.push(Value::Tuple(values));

        Ok(())
    }

    fn eval_get_type(&mut self) -> Result<()> {
        let value = self.stack.pop();
        let class = AtomRef::clone(self.get_class(&value)?);

        self.stack.push(Value::Class(class));

        Ok(())
    }

    fn eval_make_array(&mut self, len: usize) -> Result<()> {
        let mut values = Vec::with_capacity(len);

        self.stack.pop_many(len, &mut values)?;
        self.stack
            .push(Value::Array(AtomRefMut::new(AtomRef::new(values))));

        Ok(())
    }

    fn eval_make_template(&mut self, len: usize) -> Result<()> {
        let mut values = Vec::with_capacity(len);
        self.stack.pop_many(len, &mut values)?;

        let strings = values
            .into_iter()
            .map(|value| value.to_string())
            .collect::<Vec<_>>();

        let mut buffer = Vec::with_capacity(strings.iter().map(|s| s.len()).sum());

        for string in strings {
            buffer.append(&mut string.into_bytes());
        }

        self.stack
            .push(Value::String(AtomString::from(AtomRef::from(buffer))));

        Ok(())
    }

    fn eval_make_ref(&mut self) {
        let value = self.stack.pop();

        self.stack.push(Value::Ref(AtomRef::new(value)));
    }

    fn eval_deref(&mut self) -> Result<()> {
        let value = self.stack.pop();

        if let Value::Ref(value) = value {
            self.stack.push(unwrap_or_clone_inner(value));

            return Ok(());
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!("unable to dereference: {}", value.get_type().name()),
        ))
    }

    fn eval_logical_and(&mut self) -> Result<()> {
        let right = self.stack.pop();
        let left = self.stack.pop();

        self.stack.push(match left {
            Value::Bool(val) => Value::Bool(val && right.convert()?),
            _ => {
                return Err(RuntimeError::new(
                    ErrorKind::TypeError,
                    format!(
                        "expected '{}', found: {}",
                        right.get_type().name(),
                        left.get_type().name()
                    ),
                ))
            }
        });

        Ok(())
    }

    fn eval_arithmetic_bit_or(&mut self) -> Result<()> {
        impl_op!(self, int_only: bitor)
    }

    fn eval_arithmetic_bit_and(&mut self) -> Result<()> {
        impl_op!(self, int_only: bitand)
    }

    fn eval_arithmetic_bit_xor(&mut self) -> Result<()> {
        impl_op!(self, int_only: bitxor)
    }

    fn eval_arithmetic_bit_shift_left(&mut self) -> Result<()> {
        impl_op!(self, int_only: shl)
    }

    fn eval_arithmetic_bit_shift_right(&mut self) -> Result<()> {
        impl_op!(self, int_only: shr)
    }

    fn eval_arithmetic_add(&mut self) -> Result<()> {
        impl_op!(self, add)
    }

    fn eval_arithmetic_sub(&mut self) -> Result<()> {
        impl_op!(self, sub)
    }

    fn eval_arithmetic_mul(&mut self) -> Result<()> {
        impl_op!(self, mul)
    }

    fn eval_arithmetic_div(&mut self) -> Result<()> {
        impl_op!(self, div)
    }

    fn eval_arithmetic_exp(&mut self) -> Result<()> {
        let right = self.stack.pop();
        let left = self.stack.pop();

        self.stack.push(match left {
            Value::Int(val) => {
                let right_val: u64 = right.convert()?;

                Value::Int(val.pow(right_val as u32))
            }
            Value::Uint(val) => {
                let right_val: u64 = right.convert()?;

                Value::Uint(val.pow(right_val as u32))
            }
            Value::Float(val) => Value::Float(val.powf(right.convert()?)),
            _ => {
                return Err(RuntimeError::new(
                    ErrorKind::TypeError,
                    format!(
                        "invalid types: {} and {} in exponent",
                        left.get_type().name(),
                        right.get_type().name()
                    ),
                ))
            }
        });

        Ok(())
    }

    fn eval_arithmetic_mod(&mut self) -> Result<()> {
        let right = self.stack.pop();
        let left = self.stack.pop();

        self.stack.push(match left {
            Value::Int(val) => {
                let right_val: i64 = right.convert()?;

                Value::Int(val.rem(right_val))
            }
            Value::Uint(val) => {
                let right_val: u64 = right.convert()?;

                Value::Uint(val.rem(right_val))
            }
            _ => {
                return Err(RuntimeError::new(
                    ErrorKind::TypeError,
                    format!(
                        "invalid types: {} and {} in modulus",
                        left.get_type().name(),
                        right.get_type().name()
                    ),
                ))
            }
        });

        Ok(())
    }

    fn eval_is_eq(&mut self) -> Result<bool> {
        let right = self.stack.pop();
        let left = self.stack.pop();

        let left_num_id = match &left {
            Value::Int(_) => NumberType::Int,
            Value::Uint(_) => NumberType::Uint,
            Value::Float(_) => NumberType::Float,
            _ => NumberType::Unknown,
        };
        let right_num_id = match &right {
            Value::Int(_) => NumberType::Int,
            Value::Uint(_) => NumberType::Uint,
            Value::Float(_) => NumberType::Float,
            _ => NumberType::Unknown,
        };
        let upper_bound = if left_num_id > right_num_id {
            left_num_id
        } else {
            right_num_id
        };

        Ok(match upper_bound {
            NumberType::Int => {
                let left: i64 = left.convert()?;
                let right: i64 = right.convert()?;

                left == right
            }
            NumberType::Uint => {
                let left: u64 = left.convert()?;
                let right: u64 = right.convert()?;

                left == right
            }
            NumberType::Float => {
                let left: f64 = left.convert()?;
                let right: f64 = right.convert()?;

                left == right
            }
            NumberType::Unknown => left.eq(&right),
        })
    }

    fn eval_comparison_eq(&mut self) -> Result<()> {
        let result = self.eval_is_eq()?;
        self.stack.push(Value::Bool(result));

        Ok(())
    }

    fn eval_comparison_neq(&mut self) -> Result<()> {
        let result = self.eval_is_eq()?;
        self.stack.push(Value::Bool(!result));

        Ok(())
    }

    fn eval_comparison_gt(&mut self) -> Result<()> {
        impl_op!(self, compare: gt)
    }

    fn eval_comparison_gte(&mut self) -> Result<()> {
        impl_op!(self, compare: ge)
    }

    fn eval_comparison_lt(&mut self) -> Result<()> {
        impl_op!(self, compare: lt)
    }

    fn eval_comparison_lte(&mut self) -> Result<()> {
        impl_op!(self, compare: le)
    }

    fn eval_assert_is_type(&mut self) -> Result<()> {
        let right = self.stack.pop();
        let left = self.stack.pop();
        let left_class = self.get_class(&left)?;

        if let Value::Class(right_class) = right {
            let equals = Value::Bool(left_class.as_ref() == right_class.as_ref());

            self.stack.push(equals);

            return Ok(());
        }

        if let Value::Interface(interface) = right {
            let equals = Value::Bool(self.validate_class(&left_class, &interface));

            self.stack.push(equals);

            return Ok(());
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!("unable to assert type with: {}", right,),
        ))
    }

    fn eval_cast(&mut self, segment_id: usize) -> Result<()> {
        let type_name = self.call_stack.current().function.ir.get_data(segment_id);
        let value = self.stack.pop();

        if value.get_type().name() == type_name {
            self.stack.push(value);

            return Ok(());
        }

        self.stack.push(match value {
            Value::Int(val) if type_name == "Float" => Value::Float(val as f64),
            Value::Int(val) if type_name == "Byte" => Value::Byte(val as u8),
            Value::Int(val) if type_name == "Uint" => Value::Uint(val as u64),
            Value::Uint(val) if type_name == "Int" => Value::Int(val as i64),
            Value::Float(val) if type_name == "Int" => {
                if val.is_sign_positive() {
                    Value::Int(val as i64)
                } else {
                    Value::Uint(val as u64)
                }
            }
            Value::Char(val) if type_name == "Byte" => Value::Byte(val as u8),
            Value::Byte(val) if type_name == "Int" => Value::Int(val as i64),
            Value::Byte(val) if type_name == "Uint" => Value::Uint(val as u64),
            Value::Byte(val) if type_name == "Char" => Value::Char(val as char),
            Value::Bool(val) if type_name == "Int" => Value::Int(val as i64),
            Value::Bool(val) if type_name == "Uint" => Value::Uint(val as u64),
            _ => {
                return Err(RuntimeError::new(
                    ErrorKind::FatalError,
                    format!(
                        "unable to cast '{}' to invalid type: {}",
                        value.get_type().name(),
                        type_name
                    ),
                ));
            }
        });

        Ok(())
    }

    fn eval_store(&mut self, id: usize, _is_mutable: bool) -> Result<()> {
        let value = self.stack.pop();
        let frame = self.call_stack.current_mut();

        if frame.locals.len() == id {
            frame.locals.push(value);
        } else {
            // Insert padding if required
            while frame.locals.len() <= id {
                frame.locals.push(Value::Void);
            }

            frame.locals[id] = value;
        }

        Ok(())
    }

    fn eval_store_try_ok(&mut self) -> Result<()> {
        self.try_stack.pop();

        let return_value = self.stack.pop();
        let array: AtomArray<Value> =
            AtomRef::new([Value::Symbol(Symbol::from("ok")), return_value]);

        self.stack.push(Value::Tuple(array));

        Ok(())
    }

    fn eval_load_index(&mut self, push_back: bool) -> Result<()> {
        let index = self.stack.pop();
        let value = self.stack.pop();

        let index_type = index.get_type();
        let index: usize = index.convert().map_err(|e| {
            e.with_message(format!(
                "unable to use '{}' as array index",
                index_type.name()
            ))
        })?;

        let elem = if let Value::Array(array) = &value {
            array.get(index)
        } else if let Value::Tuple(tuple) = &value {
            tuple.get(index)
        } else {
            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!("unable to index type: {}", value.get_type().name()),
            ));
        };

        if let Some(item) = elem {
            let item = item.clone();

            if push_back {
                self.stack.push(value);
            }

            self.stack.push(item);

            return Ok(());
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!("index out of bounds: {}", index,),
        ))
    }

    fn eval_make_slice(&mut self) -> Result<()> {
        let to = self.stack.pop().convert()?;
        let from = self.stack.pop().convert()?;
        let value = self.stack.pop();

        match value {
            Value::Array(array) => {
                let data = array[from..to].to_vec();

                self.stack
                    .push(Value::Array(AtomRefMut::new(AtomRef::new(data))));

                Ok(())
            }
            _ => Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!("unable to create a slice of type: {}", value),
            )),
        }
    }

    fn eval_store_index(&mut self) -> Result<()> {
        let value = self.stack.pop();
        let index = self.stack.pop();
        let data = self.stack.pop();

        match data {
            Value::Array(mut array) => {
                let index_type = index.get_type();
                let index: usize = index.convert().map_err(|e| {
                    e.with_message(format!(
                        "unable to use '{}' as array index",
                        index_type.name()
                    ))
                })?;

                if let Some(entry) = array.as_mut().get_mut(index) {
                    *entry = value;

                    return Ok(());
                }

                Err(RuntimeError::new(
                    ErrorKind::FatalError,
                    format!("index out of bounds: {}", index),
                ))
            }
            _ => Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!("unable to index type: {}", data.get_type().name()),
            )),
        }
    }

    fn eval_load_member(&mut self, segment_id: usize) -> Result<()> {
        let frame = self.call_stack.current();
        let member = frame.function.ir.get_data(segment_id);
        let receiver = self.stack.pop();

        if let Value::Class(class) = &receiver {
            if let Some(func) = class.static_methods.get(member.as_str()) {
                self.stack.push(Value::Method(AtomRef::new(Method {
                    receiver: Receiver::Unbound,
                    func: AtomRef::clone(func),
                    is_static: true,
                })));

                return Ok(());
            }
        }

        let class = self.get_class(&receiver)?;

        if let Some((id, field)) = class.get_field(member) {
            if let Value::Object(object) = &receiver {
                if !field.public && frame.function.origin.module_id != class.origin.module_id {
                    return Err(RuntimeError::new(
                        ErrorKind::FatalError,
                        format!(
                            "unable to access private field: {}.{}",
                            class.as_ref(),
                            member,
                        ),
                    ));
                }

                let field = object.fields.get(id).cloned().ok_or_else(|| {
                    RuntimeError::new(
                        ErrorKind::FatalError,
                        format!(
                            "unable to get unknown field '{}' of class: {}",
                            member,
                            class.as_ref()
                        ),
                    )
                })?;

                self.stack.push(field);

                return Ok(());
            }

            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!("unable to lookup field on: {}", receiver.get_type().name(),),
            ));
        }

        if let Some(func) = class.methods.get(member.as_str()).map(AtomRef::clone) {
            if !func.public && frame.function.origin.module_id != class.origin.module_id {
                return Err(RuntimeError::new(
                    ErrorKind::FatalError,
                    format!(
                        "unable to access private method: {}.{}(..)",
                        class.as_ref(),
                        member,
                    ),
                ));
            }

            self.stack.push(Value::Method(AtomRef::new(Method {
                receiver: Receiver::Bound(receiver),
                func,
                is_static: false,
            })));

            return Ok(());
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!(
                "no such field or method '{}' for: {}",
                member,
                class.as_ref(),
            ),
        ))
    }

    fn eval_store_member(&mut self, segment_id: usize) -> Result<()> {
        let frame = self.call_stack.current();
        let member = frame.function.ir.get_data(segment_id);
        let value = self.stack.pop();
        let object = self.stack.pop();
        let class = self.get_class(&object)?;

        if let Some((id, field)) = class.get_field(member) {
            if !field.public && frame.function.origin.module_id != class.origin.module_id {
                return Err(RuntimeError::new(
                    ErrorKind::FatalError,
                    format!(
                        "unable to access private field '{}' of class: {}",
                        member,
                        class.as_ref()
                    ),
                ));
            }

            if !field.mutable {
                return Err(RuntimeError::new(
                    ErrorKind::FatalError,
                    format!(
                        "unable to assign to immutable field '{}' of class: {}",
                        member,
                        class.as_ref()
                    ),
                ));
            }

            if let Value::Object(mut object) = object {
                object.as_mut().fields.as_mut()[id] = value;

                return Ok(());
            }

            return Err(RuntimeError::new(
                ErrorKind::FatalError,
                format!(
                    "unable to store member on invalid type '{}' expected Object",
                    object.get_type().name(),
                ),
            ));
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!("no such field '{}' for class: {}", member, class.as_ref()),
        ))
    }

    fn eval_load(&mut self, id: usize) -> Result<()> {
        if !self.call_stack.is_empty() {
            let frame = self.call_stack.current();

            if let Some(value) = frame.locals.get(id) {
                self.stack.push(value.clone());

                return Ok(());
            }
        }

        Err(RuntimeError::new(
            ErrorKind::UserError,
            format!("undefined local with id: {}", id),
        ))
    }

    fn eval_load_receiver(&mut self) -> Result<()> {
        let receiver = self.call_stack.current().get_receiver().ok_or_else(|| {
            RuntimeError::new(
                ErrorKind::FatalError,
                "unable to load 'this' outside of a method".to_string(),
            )
        })?;

        self.stack.push(receiver.clone());

        Ok(())
    }

    fn eval_load_global(&mut self, id: usize) -> Result<()> {
        let module_id = self.call_stack.current().function.origin.module_id;
        let current_module = self.module_cache.get_module_by_id(module_id)?;
        let value = match &current_module.globals[id] {
            Global::Fn(func) => func.upgrade().map(Value::Fn).ok_or_else(|| {
                RuntimeError::new(ErrorKind::FatalError, "function was dropped".to_string())
            }),
            Global::Class(class) => class.upgrade().map(Value::Class).ok_or_else(|| {
                RuntimeError::new(ErrorKind::FatalError, "class was dropped".to_string())
            }),
            Global::Interface(interface) => {
                interface.upgrade().map(Value::Interface).ok_or_else(|| {
                    RuntimeError::new(ErrorKind::FatalError, "interface was dropped".to_string())
                })
            }
        }?;

        self.stack.push(value);

        Ok(())
    }

    fn eval_load_fn(&mut self, id: usize) -> Result<()> {
        let module_id = self.call_stack.current().function.origin.module_id;
        let current_module = self.module_cache.get_module_by_id(module_id)?;

        self.stack
            .push(Value::Fn(AtomRef::clone(&current_module.functions[id])));

        Ok(())
    }

    fn eval_make_closure(&mut self, fn_id: usize) -> Result<()> {
        let module_id = self.call_stack.current().function.origin.module_id;
        let current_module = self.module_cache.get_module_by_id(module_id)?;
        let frame = self.call_stack.current();

        self.stack.push(Value::Closure(AtomRef::new(Closure {
            func: AtomRef::clone(&current_module.functions[fn_id]),
            values: frame.locals.clone(),
        })));

        Ok(())
    }

    fn eval_load_class(&mut self, id: usize) -> Result<()> {
        let module_id = self.call_stack.current().function.origin.module_id;
        let current_module = self.module_cache.get_module_by_id(module_id)?;
        let value = current_module
            .classes
            .get_index(id)
            .map(|(_, val)| AtomRef::clone(val))
            .ok_or_else(|| {
                RuntimeError::new(
                    ErrorKind::FatalError,
                    format!("class with ID '{}' not found", id),
                )
            })?;

        self.stack.push(Value::Class(value));

        Ok(())
    }

    fn eval_load_interface(&mut self, id: usize) -> Result<()> {
        let frame = self.call_stack.current();
        let current_module = self
            .module_cache
            .get_module_by_id(frame.function.origin.module_id)?;
        let value = current_module
            .interfaces
            .get(id)
            .map(AtomRef::clone)
            .ok_or_else(|| {
                RuntimeError::new(
                    ErrorKind::FatalError,
                    format!("interface with ID '{}' not found", id),
                )
            })?;

        self.stack.push(Value::Interface(value));

        Ok(())
    }

    fn eval_load_target(&mut self) -> Result<()> {
        if let Some(context) = self.call_stack.last() {
            self.stack.push(match &context.target {
                Target::Fn(func) => Value::Fn(AtomRef::clone(func)),
                Target::Method(method) => Value::Method(AtomRef::clone(method)),
                Target::Closure(closure) => Value::Closure(AtomRef::clone(closure)),
            });

            return Ok(());
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            "unable to load target outside of call".to_string(),
        ))
    }

    fn eval_branch(&mut self, true_label: Label, false_label: Label) -> Result<Label> {
        let value = self.stack.pop().convert()?;

        Ok(if value { true_label } else { false_label })
    }

    fn eval_jump_if_true(&mut self) -> Result<bool> {
        let value = self.stack.pop().convert()?;

        if value {
            self.stack.push(Value::Bool(value));
        }

        Ok(value)
    }

    fn find_label(&self, search: &str) -> Result<usize> {
        for (i, code) in self.call_stack.current().function.ir.iter().enumerate() {
            if let Code::SetLabel(label) = code {
                if label == search {
                    return Ok(i);
                }
            }
        }

        Err(RuntimeError::new(
            ErrorKind::FatalError,
            format!("unable to find label: {}", search,),
        ))
    }

    fn jump_to_label(&mut self, label: Label) -> Result<()> {
        match label {
            Label::Name(name) => {
                let index = self.find_label(&name)?;

                self.call_stack.current_mut().position = index;
            }
            Label::Index(index) => self.call_stack.current_mut().position = index,
        }

        Ok(())
    }

    fn _eval(&mut self, module_id: ModuleId) -> Result<()> {
        while !self.call_stack.is_empty() {
            // Evaluates the instruction and optionally returns a label to jump to
            match self.eval_current() {
                Ok(flow) => match flow {
                    Flow::Continue => continue,
                    Flow::Return(is_stored) => {
                        let frame = self.call_stack.current();

                        // Make sure the return value was pushed to the stack only if it's used
                        if frame.store_return_value != is_stored {
                            if frame.store_return_value {
                                self.stack.push(Value::Void);
                            } else {
                                self.stack.pop();
                            }
                        }

                        // Cleanup after the call has finished
                        let frame = self.call_stack.current_mut();

                        if let Some(addr) = frame.return_addr.pop() {
                            frame.position = addr;

                            continue;
                        }

                        self.call_stack.pop();
                    }
                },
                Err(err) => {
                    // If there is an error that is try-able, let's handle it
                    if matches!(err.kind, ErrorKind::UserError | ErrorKind::IOError) {
                        if let Some(global_label) = self.try_stack.pop() {
                            let array: AtomArray<Value> = AtomRef::new([
                                Value::Symbol(Symbol::from("err")),
                                Value::String(AtomString::new(format!("{}", err))),
                            ]);

                            self.stack.push(Value::Tuple(array));

                            // Pop stack frames until we reach the one were looking for
                            while self.call_stack.current_id() != global_label.frame {
                                self.call_stack.pop();
                            }

                            self.jump_to_label(global_label.label)?;

                            continue;
                        }
                    }

                    // Otherwise, simply fail
                    let module = self.module_cache.get_module_by_id(module_id)?;

                    if err.location.is_none() {
                        let mut e = err.with_module(&module.name);
                        let frame = self.call_stack.current();

                        if let Some(location) = frame.function.ir.get_location(frame.position - 1) {
                            e = e.with_location(location.clone());
                        }

                        if let Some(filename) = &module.filename {
                            return Err(e.with_filename(filename.clone()));
                        }

                        return Err(e);
                    };

                    return Err(err);
                }
            };
        }

        Ok(())
    }

    pub fn eval(&mut self, module: &str, entrypoint: AtomRef<Fn>) -> Result<Option<Value>> {
        let module_id = self.module_cache.get_module(module)?.id;
        let locals = match entrypoint.ir.get_locals_size() {
            Some(size) => Vec::with_capacity(size),
            None => vec![],
        };

        self.call_stack
            .push(StackFrame::new(Target::Fn(entrypoint), true, locals));

        self._eval(module_id)
            .map_err(|e| e.with_stack_trace(self.call_stack.rewind()))?;

        Ok(self.stack.try_pop())
    }
}

impl AtomApi for Machine {
    fn find_class(&self, module_name: &str, class_name: &str) -> Result<AtomRef<Class>> {
        self.module_cache.get_class(module_name, class_name)
    }

    fn get_receiver(&self) -> Option<&Value> {
        if self.call_stack.is_empty() {
            return None;
        }

        self.call_stack.current().get_receiver()
    }
}
