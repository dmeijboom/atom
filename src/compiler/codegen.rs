use std::mem;

use crate::ast::LogicalOp;
use crate::compiler::mir::{
    AssignLeftHand, Block, Const, DeclKind, ForLoopMeta, Function, Mir, Scope, ScopeContext, Stmt,
    StmtKind, TemplateComponent, Terminator, Value, ValueKind,
};
use crate::compiler::optimizers::Optimizer;
use crate::compiler::slugs::Slugs;
use crate::compiler::ir::{Code, Label, IR};

use super::module::Module;
use super::result::{CompileError, Result};

pub struct CodeGenerator<'c> {
    ir: IR,
    slugs: Slugs,
    mir: &'c Mir,
    module: &'c mut Module,
    optimizers: Vec<Optimizer>,
}

impl<'c> CodeGenerator<'c> {
    pub fn new(module: &'c mut Module, mir: &'c Mir, optimizers: Vec<Optimizer>) -> Self {
        Self {
            ir: IR::new(),
            mir,
            module,
            optimizers,
            slugs: Slugs::new(),
        }
    }

    fn get_loop_meta(&self, scope: &'c Scope) -> &ForLoopMeta {
        if let ScopeContext::ForLoop(meta) = &scope.context {
            return meta;
        }

        if let Some(parent_id) = scope.parent {
            if let Some(parent) = self.mir.scopes.get(parent_id) {
                return self.get_loop_meta(parent);
            }
        }

        unreachable!("invalid scope (missing loop metadata)")
    }

    fn collect_instr(&mut self) -> IR {
        let mut ir = IR::new();
        mem::swap(&mut ir, &mut self.ir);

        for optimizer in self.optimizers.iter() {
            optimizer(self.module, &mut ir);
        }

        ir
    }

    fn compile_values(&mut self, scope: &'c Scope, values: &[Value]) -> Result<()> {
        for value in values {
            self.compile_value(scope, value)?;
        }

        Ok(())
    }

    fn compile_value(&mut self, scope: &'c Scope, value: &Value) -> Result<()> {
        let location = Some(&value.loc);

        match &value.kind {
            ValueKind::Const(val) => {
                self.ir.add(
                    match val {
                        Const::Int128(val) => Code::ConstInt128(*val),
                        Const::Int64(val) => Code::ConstInt64(*val),
                        Const::Uint64(val) => Code::ConstUint64(*val),
                        Const::Int32(val) => Code::ConstInt32(*val),
                        Const::Byte(val) => Code::ConstByte(*val),
                        Const::Float(val) => Code::ConstFloat(*val),
                        Const::Bool(val) => Code::ConstBool(*val),
                        Const::Char(val) => Code::ConstChar(*val),
                        Const::Symbol(val) => Code::ConstSymbol(val.clone()),
                        Const::String(val) => Code::ConstString(val.clone()),
                    },
                    location,
                );
            }
            ValueKind::Receiver => self.ir.add(Code::LoadReceiver, location),
            ValueKind::Local(id) => self.ir.add(Code::Load(*id), location),
            ValueKind::Name(name) => {
                let code = if let Some(id) = self.module.imports.get_index_of(name) {
                    Code::LoadGlobal(id)
                } else if let Some(id) = self.module.functions.get_index_of(name) {
                    Code::LoadFn(id)
                } else if let Some(id) = self.module.classes.get_index_of(name) {
                    Code::LoadClass(id)
                } else if let Some(id) = self.module.interfaces.get_index_of(name) {
                    Code::LoadInterface(id)
                } else {
                    return Err(CompileError::new(format!("invalid name: {}", name))
                        .with_location(value.loc.clone()));
                };

                self.ir.add(code, location);
            }
            ValueKind::Cast(cast) => {
                self.compile_value(scope, &cast.value)?;
                self.ir.add(Code::Cast(cast.dest.clone()), location);
            }
            ValueKind::Call(call) => {
                self.compile_values(scope, &call.args)?;

                let function_target = if let ValueKind::Name(function_name) = &call.callee.kind {
                    Some(function_name)
                } else {
                    None
                };

                if function_target.map(|s| s.as_str()) == self.mir.get_function_target(scope) {
                    self.ir.add(Code::LoadTarget, location);
                } else {
                    self.compile_value(scope, &call.callee)?;
                }

                let code = if call.keywords.is_empty() {
                    Code::Call(call.args.len())
                } else {
                    Code::CallKeywords((call.keywords.clone(), call.args.len()))
                };

                self.ir.add(code, location);
            }
            ValueKind::Unwrap(unwrap) => {
                self.compile_value(scope, unwrap)?;
                self.ir.add(Code::Unwrap, location);
            }
            ValueKind::Array(values) => {
                self.compile_values(scope, values)?;
                self.ir.add(Code::MakeArray(values.len()), location);
            }
            ValueKind::Tuple(values) => {
                self.compile_values(scope, values)?;
                self.ir.add(Code::MakeTuple(values.len()), location);
            }
            ValueKind::Closure(_) => unreachable!(),
            ValueKind::Member(member) => {
                self.compile_value(scope, &member.object)?;
                self.ir
                    .add(Code::LoadMember(member.member.clone()), location);
            }
            ValueKind::Comparison(comparison) => {
                self.compile_value(scope, &comparison.left)?;
                self.compile_value(scope, &comparison.right)?;
                self.ir.add(comparison.op.into(), location);
            }
            ValueKind::Arithmetic(arithmetic) => {
                self.compile_value(scope, &arithmetic.left)?;
                self.compile_value(scope, &arithmetic.right)?;
                self.ir.add(arithmetic.op.into(), location);
            }
            ValueKind::Logical(logical) => {
                self.compile_value(scope, &logical.left)?;

                match logical.op {
                    LogicalOp::And => {
                        self.compile_value(scope, &logical.right)?;
                        self.ir.add(Code::LogicalAnd, location);
                    }
                    LogicalOp::Or => {
                        let label = self.slugs.get("or");

                        self.ir
                            .add(Code::JumpIfTrue(Label::new(label.clone())), location);
                        self.compile_value(scope, &logical.right)?;
                        self.ir.add(Code::SetLabel(label), location);
                    }
                }
            }
            ValueKind::MakeRef(value) => {
                self.compile_value(scope, value)?;
                self.ir.add(Code::MakeRef, location);
            }
            ValueKind::Deref(value) => {
                self.compile_value(scope, value)?;
                self.ir.add(Code::Deref, location);
            }
            ValueKind::Index(index) => {
                self.compile_value(scope, &index.object)?;
                self.compile_value(scope, &index.index)?;
                self.ir.add(Code::LoadIndex, location);
            }
            ValueKind::Slice(slice) => {
                self.compile_value(scope, &slice.object)?;
                self.compile_value(scope, &slice.begin)?;
                self.compile_value(scope, &slice.end)?;
                self.ir.add(Code::MakeSlice, location);
            }
            ValueKind::Range(range) => {
                self.compile_value(scope, &range.begin)?;
                self.compile_value(scope, &range.end)?;
                self.ir.add(Code::MakeRange, location);
            }
            ValueKind::Template(template) => {
                for component in template.iter() {
                    match component {
                        TemplateComponent::String(val) => {
                            self.ir.add(Code::ConstString(val.to_string()), location)
                        }
                        TemplateComponent::Value(value) => self.compile_value(scope, value)?,
                    }
                }

                self.ir.add(Code::MakeTemplate(template.len()), location);
            }
            ValueKind::TypeAssert(assert) => {
                self.compile_value(scope, &assert.left)?;
                self.compile_value(scope, &assert.right)?;
                self.ir.add(Code::AssertIsType, location);
            }
        }

        Ok(())
    }

    fn compile_stmt(&mut self, scope: &'c Scope, stmt: &Stmt) -> Result<()> {
        let location = Some(&stmt.loc);

        match &stmt.kind {
            StmtKind::Assign(assign) => match &assign.left {
                AssignLeftHand::Local(id) => {
                    self.compile_value(scope, &assign.right)?;
                    self.ir
                        .add(self.mir.get_local(scope, *id).store_instr(), location);
                }
                AssignLeftHand::Index(index) => {
                    self.compile_value(scope, &index.object)?;
                    self.compile_value(scope, &index.index)?;
                    self.compile_value(scope, &assign.right)?;

                    self.ir.add(Code::StoreIndex, location);
                }
                AssignLeftHand::Member(member) => {
                    self.compile_value(scope, &member.object)?;
                    self.compile_value(scope, &assign.right)?;

                    self.ir
                        .add(Code::StoreMember(member.member.clone()), location);
                }
            },
            StmtKind::Cond(cond) => {
                let if_label = self.slugs.get("if");
                let else_label = self.slugs.get("else");
                let cont_label = self.slugs.get("if_else_cont");

                self.compile_value(scope, &cond.condition)?;

                self.ir.add(
                    Code::Branch((
                        Label::new(if_label.clone()),
                        Label::new(if cond.alt.is_none() {
                            cont_label.clone()
                        } else {
                            else_label.clone()
                        }),
                    )),
                    location,
                );

                self.ir.add(Code::SetLabel(if_label), location);
                self.compile_block(&cond.block)?;

                if let Some(block) = &cond.alt {
                    self.ir
                        .add(Code::Jump(Label::new(cont_label.clone())), location);
                    self.ir.add(Code::SetLabel(else_label), location);
                    self.compile_block(block)?;
                    self.ir
                        .add(Code::Jump(Label::new(cont_label.clone())), location);
                }

                self.ir.add(Code::SetLabel(cont_label), location);
            }
            StmtKind::Loop(block) => {
                let loop_label = self.slugs.get("loop_body");

                self.ir.add(Code::SetLabel(loop_label.clone()), location);
                self.compile_block(block)?;
                self.ir.add(Code::Jump(Label::new(loop_label)), location);

                let cont_label = self
                    .mir
                    .scopes
                    .get(block.scope_id)
                    .and_then(|scope| {
                        if let ScopeContext::ForLoop(meta) = &scope.context {
                            Some(meta.continue_label.clone())
                        } else {
                            None
                        }
                    })
                    .ok_or_else(|| unreachable!("invalid scope for loop"))?;

                self.ir.add(Code::SetLabel(cont_label), location);
            }
            StmtKind::Eval(value) => {
                self.compile_value(scope, value)?;
                self.ir.add(Code::Discard, location);
            }
            StmtKind::Return(value) => {
                self.compile_value(scope, value)?;
                self.ir.add(Code::Return, location);
            }
        }

        Ok(())
    }

    fn compile_block(&mut self, block: &Block) -> Result<()> {
        let scope = unsafe { self.mir.scopes.get_unchecked(block.scope_id) };

        for stmt in block.statements.iter() {
            self.compile_stmt(scope, stmt)?;
        }

        if let Some(Terminator::Break) = &block.terminator {
            let meta = self.get_loop_meta(scope);
            let cont_label = meta.continue_label.clone();

            self.ir
                .add(Code::Jump(Label::new(cont_label)), Some(&block.loc));
        }

        Ok(())
    }

    fn compile_function(&mut self, function: &Function) -> Result<IR> {
        self.compile_block(&function.block)?;

        Ok(self.collect_instr())
    }

    pub fn compile(mut self) -> Result<()> {
        for decl in self.mir.program.iter() {
            match &decl.kind {
                DeclKind::Function(function) => {
                    let body = self.compile_function(function)?;

                    if let Some(func) = self.module.functions.get_mut(&function.name) {
                        func.body = body;
                    } else {
                        return Err(CompileError::new(format!(
                            "missing function definition: {}",
                            function.name
                        )));
                    }
                }
                DeclKind::Class(class) => {
                    for function in class.methods.iter() {
                        let body = self.compile_function(function)?;

                        if let Some(method) = self
                            .module
                            .classes
                            .get_mut(&class.name)
                            .and_then(|class| class.methods.get_mut(&function.name))
                        {
                            method.body = body;
                        } else {
                            return Err(CompileError::new(format!(
                                "missing method definition: {}.{}",
                                class.name, function.name
                            )));
                        }
                    }
                }
                _ => continue,
            }
        }

        Ok(())
    }
}
