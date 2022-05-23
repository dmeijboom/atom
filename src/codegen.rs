use anyhow::{anyhow, Result};
use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::FunctionType;
use inkwell::values::{BasicValue, BasicValueEnum};
use inkwell::OptimizationLevel;

use crate::module::{self, Fn, InstrKind, Terminator, Type};
use crate::syntax::LiteralKind;

pub struct CodeGenerator<'ctx> {
    module: module::Module,
    context: &'ctx Context,
    llvm_module: Module<'ctx>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module: module::Module) -> Self {
        Self {
            llvm_module: context.create_module(&module.name),
            context,
            module,
        }
    }

    fn fn_type(&self, ty: &Type, params: &[&Type]) -> FunctionType<'ctx> {
        let params = &[];

        match ty {
            Type::Basic(basic_type) => match basic_type {
                module::BasicType::Float => self.context.f32_type().fn_type(params, false),
                module::BasicType::Float64 => self.context.f64_type().fn_type(params, false),
                module::BasicType::Int8 => self.context.i8_type().fn_type(params, false),
                module::BasicType::Int16 => self.context.i16_type().fn_type(params, false),
                module::BasicType::Int => self.context.i32_type().fn_type(params, false),
                module::BasicType::Int64 => self.context.i64_type().fn_type(params, false),
                module::BasicType::Void => self.context.void_type().fn_type(params, false),
                _ => unimplemented!(),
            },
        }
    }

    fn generate_fn(&self, func: &Fn) {
        let return_type = self.fn_type(&func.return_type, &[]);
        let llvm_func = self.llvm_module.add_function(&func.name, return_type, None);
        let block = self.context.append_basic_block(llvm_func, "entry");
        let builder = self.context.create_builder();

        builder.position_at_end(block);

        let mut stack = vec![];

        for block in func.body.iter() {
            for instr in block.body.iter() {
                stack.push(match &instr.kind {
                    InstrKind::Const(literal) => match literal {
                        LiteralKind::Int(val) => BasicValueEnum::IntValue(
                            self.context.i32_type().const_int(*val as u64, *val < 0),
                        ),
                        LiteralKind::Float(val) => {
                            BasicValueEnum::FloatValue(self.context.f32_type().const_float(*val))
                        }
                        _ => unimplemented!(),
                    },
                });
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
            self.generate_fn(&func);
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
            .and_then(|t| Target::from_name(t))
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

        Ok(machine
            .write_to_memory_buffer(&self.llvm_module, FileType::Object)
            .map_err(|e| anyhow!("failed to write to memory buffer: {}", e))?)
    }
}
