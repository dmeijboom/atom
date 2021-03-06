use anyhow::{anyhow, Result};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{
    AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
};
use inkwell::values::{BasicValue, BasicValueEnum, CallableValue, FunctionValue, PointerValue};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel};

use crate::backend::{module, Block, Fn, InstrKind, Terminator, Type};

macro_rules! pop_binary {
    ($stack:expr, int) => {{
        let (lhs, rhs) = pop_binary!($stack);
        (lhs.into_int_value(), rhs.into_int_value())
    }};

    ($stack:expr, float) => {{
        let (lhs, rhs) = pop_binary!($stack);
        (lhs.into_float_value(), rhs.into_float_value())
    }};

    ($stack:expr) => {{
        let rhs = $stack.pop().unwrap();
        let lhs = $stack.pop().unwrap();

        (lhs, rhs)
    }};
}

macro_rules! label {
    ($instr:expr, $name:ident) => {
        &format!("{}{}", stringify!($name), $instr.id)
    };
}

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    llvm_module: Module<'ctx>,
    stack: Vec<BasicValueEnum<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        Self {
            context,
            llvm_module: context.create_module(name),
            stack: vec![],
        }
    }

    fn make_type(&self, ty: &Type) -> AnyTypeEnum<'ctx> {
        match ty {
            Type::Float32 => self.context.f32_type().as_any_type_enum(),
            Type::Float64 => self.context.f64_type().as_any_type_enum(),
            Type::Int8 => self.context.i8_type().as_any_type_enum(),
            Type::Int16 => self.context.i16_type().as_any_type_enum(),
            Type::Int32 => self.context.i32_type().as_any_type_enum(),
            Type::Int64 => self.context.i64_type().as_any_type_enum(),
            Type::Int1 => self.context.custom_width_int_type(1).as_any_type_enum(),
            Type::Ptr(inner) => match self.make_type(inner) {
                AnyTypeEnum::ArrayType(ty) => ty.ptr_type(AddressSpace::Generic).as_any_type_enum(),
                AnyTypeEnum::FloatType(ty) => ty.ptr_type(AddressSpace::Generic).as_any_type_enum(),
                AnyTypeEnum::FunctionType(ty) => {
                    ty.ptr_type(AddressSpace::Generic).as_any_type_enum()
                }
                AnyTypeEnum::IntType(ty) => ty.ptr_type(AddressSpace::Generic).as_any_type_enum(),
                AnyTypeEnum::PointerType(ty) => {
                    ty.ptr_type(AddressSpace::Generic).as_any_type_enum()
                }
                AnyTypeEnum::StructType(ty) => {
                    ty.ptr_type(AddressSpace::Generic).as_any_type_enum()
                }
                AnyTypeEnum::VectorType(ty) => {
                    ty.ptr_type(AddressSpace::Generic).as_any_type_enum()
                }
                AnyTypeEnum::VoidType(_) => unreachable!(),
            },
            Type::Fn(return_type, args) => self
                .fn_type(return_type, args.as_slice())
                .as_any_type_enum(),
            Type::Void => self.context.void_type().as_any_type_enum(),
        }
    }

    fn make_basic_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match self.make_type(ty) {
            AnyTypeEnum::ArrayType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::FloatType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::IntType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::PointerType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::StructType(ty) => ty.as_basic_type_enum(),
            AnyTypeEnum::VectorType(ty) => ty.as_basic_type_enum(),
            _ => unreachable!("invalid basic type"),
        }
    }

    fn make_basic_metadata_type(&self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        match self.make_basic_type(ty) {
            BasicTypeEnum::ArrayType(ty) => BasicMetadataTypeEnum::ArrayType(ty),
            BasicTypeEnum::FloatType(ty) => BasicMetadataTypeEnum::FloatType(ty),
            BasicTypeEnum::IntType(ty) => BasicMetadataTypeEnum::IntType(ty),
            BasicTypeEnum::PointerType(ty) => BasicMetadataTypeEnum::PointerType(ty),
            BasicTypeEnum::StructType(ty) => BasicMetadataTypeEnum::StructType(ty),
            BasicTypeEnum::VectorType(ty) => BasicMetadataTypeEnum::VectorType(ty),
        }
    }

    fn fn_type(&self, ty: &Type, params: &[Type]) -> FunctionType<'ctx> {
        let params = params
            .iter()
            .map(|ty| self.make_basic_metadata_type(ty))
            .collect::<Vec<_>>();

        match self.make_type(ty) {
            AnyTypeEnum::ArrayType(ty) => ty.fn_type(&params, false),
            AnyTypeEnum::FloatType(ty) => ty.fn_type(&params, false),
            AnyTypeEnum::IntType(ty) => ty.fn_type(&params, false),
            AnyTypeEnum::PointerType(ty) => ty.fn_type(&params, false),
            AnyTypeEnum::StructType(ty) => ty.fn_type(&params, false),
            AnyTypeEnum::VectorType(ty) => ty.fn_type(&params, false),
            AnyTypeEnum::VoidType(ty) => ty.fn_type(&params, false),
            _ => unimplemented!(),
        }
    }

    fn generate_block(
        &mut self,
        llvm_func: FunctionValue<'ctx>,
        locals: &[PointerValue<'ctx>],
        llvm_block: BasicBlock<'ctx>,
        block: &Block,
    ) -> Builder<'ctx> {
        let builder = self.context.create_builder();
        builder.position_at_end(llvm_block);

        for instr in block.iter() {
            let value = match &instr.kind {
                InstrKind::ConstInt(val) => self
                    .context
                    .i32_type()
                    .const_int(*val as u64, false)
                    .as_basic_value_enum(),
                InstrKind::ConstUint(val) => self
                    .context
                    .i32_type()
                    .const_int(*val, true)
                    .as_basic_value_enum(),
                InstrKind::ConstFloat(val) => self
                    .context
                    .f32_type()
                    .const_float(*val as f64)
                    .as_basic_value_enum(),
                InstrKind::ConstBool(val) => self
                    .context
                    .custom_width_int_type(1)
                    .const_int(if *val { 1 } else { 0 }, false)
                    .as_basic_value_enum(),
                InstrKind::IntAdd => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_add(lhs, rhs, label!(instr, int_add))
                        .as_basic_value_enum()
                }
                InstrKind::IntSub => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_sub(lhs, rhs, label!(instr, int_sub))
                        .as_basic_value_enum()
                }
                InstrKind::IntMul => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_mul(lhs, rhs, label!(instr, int_mul))
                        .as_basic_value_enum()
                }
                InstrKind::IntSDiv => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_signed_div(lhs, rhs, label!(instr, int_sdiv))
                        .as_basic_value_enum()
                }
                InstrKind::IntUDiv => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_unsigned_div(lhs, rhs, label!(instr, int_udiv))
                        .as_basic_value_enum()
                }
                InstrKind::IntShl => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_left_shift(lhs, rhs, label!(instr, int_shl))
                        .as_basic_value_enum()
                }
                InstrKind::IntSShr => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_right_shift(lhs, rhs, true, label!(instr, int_sshr))
                        .as_basic_value_enum()
                }
                InstrKind::IntUShr => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_right_shift(lhs, rhs, false, label!(instr, int_ushr))
                        .as_basic_value_enum()
                }
                InstrKind::IntSGte => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::SGE, lhs, rhs, label!(instr, int_sgte))
                        .as_basic_value_enum()
                }
                InstrKind::IntSGt => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, label!(instr, int_sgt))
                        .as_basic_value_enum()
                }
                InstrKind::IntUGte => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::UGE, lhs, rhs, label!(instr, int_ugte))
                        .as_basic_value_enum()
                }
                InstrKind::IntUGt => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::UGT, lhs, rhs, label!(instr, int_ugt))
                        .as_basic_value_enum()
                }
                InstrKind::IntSLte => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::SLE, lhs, rhs, label!(instr, int_slte))
                        .as_basic_value_enum()
                }
                InstrKind::IntSLt => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, label!(instr, int_slt))
                        .as_basic_value_enum()
                }
                InstrKind::IntULte => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::ULE, lhs, rhs, label!(instr, int_ulte))
                        .as_basic_value_enum()
                }
                InstrKind::IntULt => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::ULT, lhs, rhs, label!(instr, int_ult))
                        .as_basic_value_enum()
                }
                InstrKind::IntEq => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, label!(instr, int_eq))
                        .as_basic_value_enum()
                }
                InstrKind::IntNeq => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::NE, lhs, rhs, label!(instr, int_neq))
                        .as_basic_value_enum()
                }
                InstrKind::FloatAdd => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_add(lhs, rhs, label!(instr, float_add))
                        .as_basic_value_enum()
                }
                InstrKind::FloatSub => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_sub(lhs, rhs, label!(instr, float_sub))
                        .as_basic_value_enum()
                }
                InstrKind::FloatMul => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_mul(lhs, rhs, label!(instr, float_mul))
                        .as_basic_value_enum()
                }
                InstrKind::FloatDiv => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_div(lhs, rhs, label!(instr, float_div))
                        .as_basic_value_enum()
                }
                InstrKind::FloatLte => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(
                            FloatPredicate::OLE,
                            lhs,
                            rhs,
                            label!(instr, float_lte),
                        )
                        .as_basic_value_enum()
                }
                InstrKind::FloatLt => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::OLE, lhs, rhs, label!(instr, float_lt))
                        .as_basic_value_enum()
                }
                InstrKind::FloatGte => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(
                            FloatPredicate::OGE,
                            lhs,
                            rhs,
                            label!(instr, float_gte),
                        )
                        .as_basic_value_enum()
                }
                InstrKind::FloatGt => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::OGT, lhs, rhs, label!(instr, float_gt))
                        .as_basic_value_enum()
                }
                InstrKind::FloatEq => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::OEQ, lhs, rhs, label!(instr, float_eq))
                        .as_basic_value_enum()
                }
                InstrKind::FloatNeq => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(
                            FloatPredicate::ONE,
                            lhs,
                            rhs,
                            label!(instr, float_neq),
                        )
                        .as_basic_value_enum()
                }
                InstrKind::LoadFn(name) => {
                    let fn_ptr = self.llvm_module.get_function(name).unwrap();

                    fn_ptr
                        .as_global_value()
                        .as_pointer_value()
                        .as_basic_value_enum()
                }
                InstrKind::Load(idx) => builder.build_load(locals[*idx], label!(instr, load)),
                InstrKind::Store(idx) => {
                    let value = self.stack.pop().unwrap();
                    builder.build_store(locals[*idx], value);

                    continue;
                }
                InstrKind::Call(_arg_count) => {
                    let fn_ptr = self.stack.pop().unwrap().into_pointer_value();
                    let callable = CallableValue::try_from(fn_ptr).unwrap();
                    let value = builder.build_call(callable, &[], label!(instr, call));

                    if let Some(basic_value) = value.try_as_basic_value().left() {
                        basic_value.as_basic_value_enum()
                    } else {
                        continue;
                    }
                }
                InstrKind::And => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_and(lhs, rhs, label!(instr, and))
                        .as_basic_value_enum()
                }
                InstrKind::Or => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_or(lhs, rhs, label!(instr, or))
                        .as_basic_value_enum()
                }
                InstrKind::Branch(then, alt) => {
                    let cond = self.stack.pop().unwrap().into_int_value();
                    let then_block = self
                        .context
                        .append_basic_block(llvm_func, label!(instr, then));
                    let alt_block = self
                        .context
                        .append_basic_block(llvm_func, label!(instr, alt));
                    let cont_block = self
                        .context
                        .append_basic_block(llvm_func, label!(instr, cont));

                    builder.build_conditional_branch(cond, then_block, alt_block);

                    let then_builder = self.generate_block(llvm_func, locals, then_block, then);

                    if !then.is_terminated() {
                        then_builder.build_unconditional_branch(cont_block);
                    }

                    let alt_builder = self.generate_block(llvm_func, locals, alt_block, alt);

                    if !alt.is_terminated() {
                        alt_builder.build_unconditional_branch(cont_block);
                    }

                    builder.position_at_end(cont_block);

                    continue;
                }
            };

            self.stack.push(value);
        }

        if let Some(Terminator::Return) = block.term {
            if llvm_func.get_type().get_return_type().is_none() {
                builder.build_return(None);
            } else {
                builder.build_return(self.stack.pop().as_ref().map(|v| v as &dyn BasicValue));
            }
        };

        builder
    }

    fn generate_fn(&mut self, func: &Fn) {
        let llvm_func = self.llvm_module.get_function(&func.name).unwrap();
        let block = self.context.append_basic_block(llvm_func, "entry");
        let builder = self.context.create_builder();

        builder.position_at_end(block);

        let mut locals = vec![];

        for (idx, ty) in func.locals.iter().enumerate() {
            locals.push(builder.build_alloca(self.make_basic_type(ty), &format!("local{}", idx)));
        }

        self.generate_block(llvm_func, &locals, block, &func.body);
    }

    pub fn generate(mut self, module: &module::Module, optimize: bool) -> Result<MemoryBuffer> {
        // Register functions early to prevent circular dependencies
        for func in module.funcs.iter() {
            let return_type = self.fn_type(&func.return_type, &[]);
            self.llvm_module.add_function(&func.name, return_type, None);
        }

        for func in module.funcs.iter() {
            self.generate_fn(func);
        }

        self.llvm_module.print_to_stderr();
        self.llvm_module
            .verify()
            .map_err(|e| anyhow!("failed to verify module: {}", e))?;

        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| anyhow!("failed to initialize target: {}", e))?;

        let triple = TargetMachine::get_default_triple();
        let target = triple
            .as_str()
            .to_string_lossy()
            .split('-')
            .next()
            .and_then(Target::from_name)
            .unwrap();
        let machine = target
            .create_target_machine(
                &triple,
                &TargetMachine::get_host_cpu_name().to_string(),
                &TargetMachine::get_host_cpu_features().to_string(),
                if optimize {
                    OptimizationLevel::Aggressive
                } else {
                    OptimizationLevel::None
                },
                RelocMode::Static,
                CodeModel::Small,
            )
            .unwrap();

        machine
            .write_to_memory_buffer(&self.llvm_module, FileType::Object)
            .map_err(|e| anyhow!("failed to write to memory buffer: {}", e))
    }
}
