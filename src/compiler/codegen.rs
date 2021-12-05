use std::mem;

use enumflags2::BitFlags;

use crate::compiler::ir::{Code, Label, IR};
use crate::compiler::mir::{
    AssignLeftHand, Block, Const, DeclKind, ForLoopMeta, Function, Mir, Scope, ScopeContext, Stmt,
    StmtKind, TemplateComponent, Terminator, Value, ValueKind,
};
use crate::compiler::optimizers::Optimizer;
use crate::compiler::slugs::Slugs;
use crate::compiler::{module, FunctionAttr};
use crate::syntax::LogicalOp;

use super::error::{CompileError, Result};
use super::module::Module;

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

    fn collect_instr(&mut self, optimize: bool) -> IR {
        let mut ir = IR::new();
        mem::swap(&mut ir, &mut self.ir);

        if optimize {
            for optimizer in self.optimizers.iter() {
                optimizer(self.module, &mut ir);
            }
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
                let code = match val {
                    Const::Int(val) => Code::ConstInt(*val),
                    Const::Uint(val) => Code::ConstUint(*val),
                    Const::Byte(val) => Code::ConstByte(*val),
                    Const::Float(val) => Code::ConstFloat(*val),
                    Const::Bool(val) => Code::ConstBool(*val),
                    Const::Char(val) => Code::ConstChar(*val),
                    Const::Symbol(val) => {
                        let segment_id = self.ir.add_data(val);
                        Code::ConstSymbol(segment_id)
                    }
                    Const::String(val) => {
                        let segment_id = self.ir.add_data(val);
                        Code::ConstString(segment_id)
                    }
                };

                self.ir.add(code, location);
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

                let segment_id = self.ir.add_data(&cast.dest);

                self.ir.add(Code::Cast(segment_id), location);
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

                self.ir.add(Code::Call(call.args.len()), location);
            }
            ValueKind::New(new) => {
                self.compile_values(scope, &new.args)?;
                self.compile_value(scope, &new.callee)?;

                let mut segment_ids = vec![];

                for keyword in new.keywords.iter() {
                    segment_ids.push(self.ir.add_data(keyword));
                }

                self.ir.add(
                    Code::MakeInstance((
                        if segment_ids.is_empty() {
                            [0, 0]
                        } else {
                            [segment_ids[0], segment_ids[segment_ids.len() - 1] + 1]
                        },
                        new.args.len(),
                    )),
                    location,
                );
            }
            ValueKind::Try(value) => {
                let label = self.slugs.get("try_end");

                self.ir
                    .add(Code::JumpOnError(Label::Name(label.clone())), location);
                self.compile_value(scope, value)?;
                self.ir.add(Code::StoreTryOk, location);
                self.ir.add(Code::SetLabel(label), location);
            }
            ValueKind::Array(values) => {
                self.compile_values(scope, values)?;
                self.ir.add(Code::MakeArray(values.len()), location);
            }
            ValueKind::Tuple(values) => {
                self.compile_values(scope, values)?;
                self.ir.add(Code::MakeTuple(values.len()), location);
            }
            ValueKind::TypeOf(type_of) => {
                self.compile_value(scope, type_of)?;
                self.ir.add(Code::GetType, location);
            }
            ValueKind::Closure(closure) => {
                let parent_ir = self.collect_instr(false);

                self.compile_block(&closure.block)?;

                let mut body = self.collect_instr(true);

                if let Some(locals_size) = body.get_locals_size() {
                    if closure.args.len() > locals_size {
                        body.set_locals_size(closure.args.len());
                    }
                }

                self.module.functions.insert(
                    closure.name.clone(),
                    module::Function {
                        name: closure.name.clone(),
                        body,
                        attr: BitFlags::from_flag(FunctionAttr::Closure),
                        args: closure.args.clone(),
                        location: closure.block.loc.clone(),
                    },
                );

                self.ir.append(parent_ir);
                self.ir.add(
                    Code::MakeClosure(self.module.functions.get_index_of(&closure.name).unwrap()),
                    location,
                );
            }
            ValueKind::Member(member) => {
                self.compile_value(scope, &member.object)?;

                let segment_id = self.ir.add_data(&member.member);

                self.ir.add(Code::LoadMember(segment_id), location);
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
                            .add(Code::JumpIfTrue(Label::Name(label.clone())), location);
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
                            let segment_id = self.ir.add_data(val);

                            self.ir.add(Code::ConstString(segment_id), location)
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

                    let segment_id = self.ir.add_data(&member.member);

                    self.ir.add(Code::StoreMember(segment_id), location);
                }
            },
            StmtKind::Cond(cond) => {
                let if_label = self.slugs.get("if");
                let else_label = self.slugs.get("else");
                let cont_label = self.slugs.get("if_else_cont");

                self.compile_value(scope, &cond.condition)?;

                self.ir.add(
                    Code::Branch((
                        Label::Name(if_label.clone()),
                        Label::Name(if cond.alt.is_none() {
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
                        .add(Code::Jump(Label::Name(cont_label.clone())), location);
                    self.ir.add(Code::SetLabel(else_label), location);
                    self.compile_block(block)?;
                    self.ir
                        .add(Code::Jump(Label::Name(cont_label.clone())), location);
                }

                self.ir.add(Code::SetLabel(cont_label), location);
            }
            StmtKind::Loop(block) => {
                let loop_label = self.slugs.get("loop_body");

                self.ir.add(Code::SetLabel(loop_label.clone()), location);
                self.compile_block(block)?;
                self.ir.add(Code::Jump(Label::Name(loop_label)), location);

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
                    .expect("invalid scope for loop");

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
        let scope = &self.mir.scopes[block.scope_id];

        for stmt in block.statements.iter() {
            self.compile_stmt(scope, stmt)?;
        }

        if let Some(Terminator::Break) = &block.terminator {
            let meta = self.get_loop_meta(scope);
            let cont_label = meta.continue_label.clone();

            self.ir
                .add(Code::Jump(Label::Name(cont_label)), Some(&block.loc));
        }

        Ok(())
    }

    fn compile_function(&mut self, function: &Function) -> Result<IR> {
        self.compile_block(&function.block)?;

        let mut ir = self.collect_instr(true);

        if let Some(locals_size) = ir.get_locals_size() {
            if function.args.len() > locals_size {
                ir.set_locals_size(function.args.len());
            }
        }

        Ok(ir)
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
