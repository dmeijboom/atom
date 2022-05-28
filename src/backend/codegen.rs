use anyhow::{anyhow, Result};
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{AnyType, AnyTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::OptimizationLevel;

use crate::backend::{module, Fn, InstrKind, Terminator, Type};
use crate::frontend::syntax::LiteralKind;

macro_rules! pop_binary {
    ($stack:ident as int) => {{
        let (lhs, rhs) = pop_binary!($stack);
        (lhs.into_int_value(), rhs.into_int_value())
    }};

    ($stack:ident as float) => {{
        let (lhs, rhs) = pop_binary!($stack);
        (lhs.into_float_value(), rhs.into_float_value())
    }};

    ($stack:ident) => {{
        let rhs = $stack.pop().unwrap();
        let lhs = $stack.pop().unwrap();

        (lhs, rhs)
    }};
}

pub struct CodeGen<'ctx> {
    module: module::Module,
    context: &'ctx Context,
    llvm_module: Module<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module: module::Module) -> Self {
        Self {
            llvm_module: context.create_module(&module.name),
            context,
            module,
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
        let params = &[];

        match self.make_type(ty) {
            AnyTypeEnum::ArrayType(ty) => ty.fn_type(params, false),
            AnyTypeEnum::FloatType(ty) => ty.fn_type(params, false),
            AnyTypeEnum::IntType(ty) => ty.fn_type(params, false),
            AnyTypeEnum::PointerType(ty) => ty.fn_type(params, false),
            AnyTypeEnum::StructType(ty) => ty.fn_type(params, false),
            AnyTypeEnum::VectorType(ty) => ty.fn_type(params, false),
            AnyTypeEnum::VoidType(ty) => ty.fn_type(params, false),
            _ => unimplemented!(),
        }
    }

    fn generate_fn(&self, func: &Fn) {
        let return_type = self.fn_type(&func.return_type, &[]);
        let llvm_func = self.llvm_module.add_function(&func.name, return_type, None);
        let block = self.context.append_basic_block(llvm_func, "entry");
        let builder = self.context.create_builder();

        builder.position_at_end(block);

        let mut stack: Vec<BasicValueEnum> = vec![];
        let mut locals = vec![];

        for (idx, ty) in func.locals.iter().enumerate() {
            locals.push(builder.build_alloca(self.make_basic_type(ty), &format!("local{}", idx)));
        }

        for block in func.body.iter() {
            for instr in block.body.iter() {
                let value = match &instr.kind {
                    InstrKind::Const(literal) => match literal {
                        LiteralKind::Int(val) => BasicValueEnum::IntValue(
                            self.context.i32_type().const_int(*val as u64, *val < 0),
                        ),
                        LiteralKind::Float(val) => {
                            BasicValueEnum::FloatValue(self.context.f32_type().const_float(*val))
                        }
                        _ => unimplemented!(),
                    },
                    InstrKind::IntAdd => {
                        let (lhs, rhs) = pop_binary!(stack as int);
                        BasicValueEnum::IntValue(builder.build_int_add(lhs, rhs, "int_add"))
                    }
                    InstrKind::IntSub => {
                        let (lhs, rhs) = pop_binary!(stack as int);
                        BasicValueEnum::IntValue(builder.build_int_sub(lhs, rhs, "int_sub"))
                    }
                    InstrKind::IntMul => {
                        let (lhs, rhs) = pop_binary!(stack as int);
                        BasicValueEnum::IntValue(builder.build_int_mul(lhs, rhs, "int_mul"))
                    }
                    InstrKind::IntSDiv => {
                        let (lhs, rhs) = pop_binary!(stack as int);
                        BasicValueEnum::IntValue(builder.build_int_signed_div(lhs, rhs, "int_sdiv"))
                    }
                    InstrKind::IntUDiv => {
                        let (lhs, rhs) = pop_binary!(stack as int);
                        BasicValueEnum::IntValue(
                            builder.build_int_unsigned_div(lhs, rhs, "int_udiv"),
                        )
                    }
                    InstrKind::FloatAdd => {
                        let (lhs, rhs) = pop_binary!(stack as float);
                        BasicValueEnum::FloatValue(builder.build_float_add(lhs, rhs, "float_add"))
                    }
                    InstrKind::FloatSub => {
                        let (lhs, rhs) = pop_binary!(stack as float);
                        BasicValueEnum::FloatValue(builder.build_float_sub(lhs, rhs, "float_sub"))
                    }
                    InstrKind::FloatMul => {
                        let (lhs, rhs) = pop_binary!(stack as float);
                        BasicValueEnum::FloatValue(builder.build_float_mul(lhs, rhs, "float_mul"))
                    }
                    InstrKind::FloatDiv => {
                        let (lhs, rhs) = pop_binary!(stack as float);
                        BasicValueEnum::FloatValue(builder.build_float_div(lhs, rhs, "float_div"))
                    }
                    InstrKind::Load(idx) => {
                        builder.build_load(locals[*idx], &format!("load{}", idx))
                    }
                    InstrKind::Store(idx) => {
                        let value = stack.pop().unwrap();
                        builder.build_store(locals[*idx], value);

                        continue;
                    }
                };

                stack.push(value);
            }

            match block.term {
                Some(Terminator::Return) => {
                    builder.build_return(stack.pop().as_ref().map(|v| v as &dyn BasicValue));
                }
                None => {}
            }
        }
    }

    pub fn generate(self) -> Result<MemoryBuffer> {
        for func in self.module.funcs.iter() {
            self.generate_fn(func);
        }

        self.llvm_module
            .verify()
            .map_err(|e| anyhow!("failed to verify module: {}", e))?;
        self.llvm_module.print_to_stderr();

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
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        machine
            .write_to_memory_buffer(&self.llvm_module, FileType::Object)
            .map_err(|e| anyhow!("failed to write to memory buffer: {}", e))
    }
}
