use anyhow::{anyhow, Result};
use inkwell::basic_block::BasicBlock;
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{FloatPredicate, IntPredicate, OptimizationLevel};

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
            Type::Void => self.context.void_type().as_any_type_enum(),
            _ => unimplemented!(),
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

    fn fn_type(&self, ty: &Type, params: &[&Type]) -> FunctionType<'ctx> {
        let params = params
            .iter()
            .map(|ty| self.make_basic_type(ty))
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

    fn generate_body(
        &mut self,
        func: FunctionValue<'ctx>,
        locals: &[PointerValue<'ctx>],
        mut block: BasicBlock<'ctx>,
        body: &Block,
    ) {
        let builder = self.context.create_builder();
        builder.position_at_end(block);

        for instr in body.iter() {
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
                        .build_int_add(lhs, rhs, "int_add")
                        .as_basic_value_enum()
                }
                InstrKind::IntSub => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_sub(lhs, rhs, "int_sub")
                        .as_basic_value_enum()
                }
                InstrKind::IntMul => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_mul(lhs, rhs, "int_mul")
                        .as_basic_value_enum()
                }
                InstrKind::IntSDiv => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_signed_div(lhs, rhs, "int_sdiv")
                        .as_basic_value_enum()
                }
                InstrKind::IntUDiv => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_unsigned_div(lhs, rhs, "int_udiv")
                        .as_basic_value_enum()
                }
                InstrKind::IntShl => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_left_shift(lhs, rhs, "int_shl")
                        .as_basic_value_enum()
                }
                InstrKind::IntSShr => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_right_shift(lhs, rhs, true, "int_sshr")
                        .as_basic_value_enum()
                }
                InstrKind::IntUShr => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_right_shift(lhs, rhs, false, "int_ushr")
                        .as_basic_value_enum()
                }
                InstrKind::IntSGte => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::SGE, lhs, rhs, "int_sgte")
                        .as_basic_value_enum()
                }
                InstrKind::IntSGt => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::SGT, lhs, rhs, "int_sgt")
                        .as_basic_value_enum()
                }
                InstrKind::IntUGte => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::UGE, lhs, rhs, "int_ugte")
                        .as_basic_value_enum()
                }
                InstrKind::IntUGt => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::UGT, lhs, rhs, "int_ugt")
                        .as_basic_value_enum()
                }
                InstrKind::IntSLte => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::SLE, lhs, rhs, "int_slte")
                        .as_basic_value_enum()
                }
                InstrKind::IntSLt => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::SLT, lhs, rhs, "int_slt")
                        .as_basic_value_enum()
                }
                InstrKind::IntULte => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::ULE, lhs, rhs, "int_ulte")
                        .as_basic_value_enum()
                }
                InstrKind::IntULt => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::ULT, lhs, rhs, "int_ult")
                        .as_basic_value_enum()
                }
                InstrKind::IntEq => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::EQ, lhs, rhs, "int_eq")
                        .as_basic_value_enum()
                }
                InstrKind::IntNeq => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder
                        .build_int_compare(IntPredicate::NE, lhs, rhs, "int_neq")
                        .as_basic_value_enum()
                }
                InstrKind::FloatAdd => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_add(lhs, rhs, "float_add")
                        .as_basic_value_enum()
                }
                InstrKind::FloatSub => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_sub(lhs, rhs, "float_sub")
                        .as_basic_value_enum()
                }
                InstrKind::FloatMul => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_mul(lhs, rhs, "float_mul")
                        .as_basic_value_enum()
                }
                InstrKind::FloatDiv => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_div(lhs, rhs, "float_div")
                        .as_basic_value_enum()
                }
                InstrKind::FloatLte => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::OLE, lhs, rhs, "float_lte")
                        .as_basic_value_enum()
                }
                InstrKind::FloatLt => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::OLE, lhs, rhs, "float_lt")
                        .as_basic_value_enum()
                }
                InstrKind::FloatGte => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::OGE, lhs, rhs, "float_gte")
                        .as_basic_value_enum()
                }
                InstrKind::FloatGt => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::OGT, lhs, rhs, "float_gt")
                        .as_basic_value_enum()
                }
                InstrKind::FloatEq => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "float_eq")
                        .as_basic_value_enum()
                }
                InstrKind::FloatNeq => {
                    let (lhs, rhs) = pop_binary!(self.stack, float);
                    builder
                        .build_float_compare(FloatPredicate::ONE, lhs, rhs, "float_neq")
                        .as_basic_value_enum()
                }
                InstrKind::Load(idx) => builder.build_load(locals[*idx], &format!("load{}", idx)),
                InstrKind::Store(idx) => {
                    let value = self.stack.pop().unwrap();
                    builder.build_store(locals[*idx], value);

                    continue;
                }
                InstrKind::And => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder.build_and(lhs, rhs, "and").as_basic_value_enum()
                }
                InstrKind::Or => {
                    let (lhs, rhs) = pop_binary!(self.stack, int);
                    builder.build_or(lhs, rhs, "or").as_basic_value_enum()
                }
                InstrKind::Branch(then, alt) => {
                    let cond = self.stack.pop().unwrap().into_int_value();
                    let then_block = self.context.append_basic_block(func, "then");
                    let alt_block = self.context.append_basic_block(func, "alt");
                    let cont = if then.is_terminated() && alt.is_terminated() {
                        None
                    } else {
                        Some(self.context.append_basic_block(func, "cont"))
                    };

                    builder.build_conditional_branch(cond, then_block, alt_block);

                    self.generate_body(func, locals, then_block, then);

                    if then.term.is_none() {
                        if let Some(cont_block) = cont {
                            builder.position_at_end(then_block);
                            builder.build_unconditional_branch(cont_block);
                        }
                    }

                    self.generate_body(func, locals, alt_block, alt);

                    if alt.term.is_none() {
                        if let Some(cont_block) = cont {
                            builder.position_at_end(alt_block);
                            builder.build_unconditional_branch(cont_block);
                        }
                    }

                    if let Some(cont_block) = cont {
                        block = cont_block;
                    }

                    builder.position_at_end(block);

                    continue;
                }
            };

            self.stack.push(value);
        }

        if let Some(Terminator::Return) = body.term {
            if func.get_type().get_return_type().is_none() {
                builder.build_return(None);
                return;
            }

            builder.build_return(self.stack.pop().as_ref().map(|v| v as &dyn BasicValue));
        };
    }

    fn generate_fn(&mut self, func: &Fn) {
        let return_type = self.fn_type(&func.return_type, &[]);
        let llvm_func = self.llvm_module.add_function(&func.name, return_type, None);
        let block = self.context.append_basic_block(llvm_func, "entry");
        let builder = self.context.create_builder();

        builder.position_at_end(block);

        let mut locals = vec![];

        for (idx, ty) in func.locals.iter().enumerate() {
            locals.push(builder.build_alloca(self.make_basic_type(ty), &format!("local{}", idx)));
        }

        self.generate_body(llvm_func, &locals, block, &func.body);
    }

    pub fn generate(mut self, module: &module::Module, optimize: bool) -> Result<MemoryBuffer> {
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
