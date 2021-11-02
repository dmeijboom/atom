use crate::compiler::mir::{
    Assign, AssignLeftHand, Block, Const, DeclKind, Function, Index, Mir, Scope, Stmt, StmtKind,
    Value, ValueKind,
};
use crate::compiler::result::{CompileError, Result};
use crate::compiler::types::{Id, PrimitiveType};
use crate::compiler::{mir, Class, Func, Interface, Module, Type};

fn is_same_type(types: &[Type]) -> bool {
    if types.len() <= 1 {
        return true;
    }

    let first = types.first();

    types.iter().skip(1).all(|t| first == Some(t))
}

fn parse_type(types: &mut Vec<Type>) -> Type {
    if !types.is_empty() && is_same_type(types) {
        return types.remove(0);
    }

    Type::Unknown
}

macro_rules! unwrap_type {
    ($container:ident, $type:expr) => {
        if let Type::$container(inner_type) = $type {
            *inner_type
        } else {
            Type::Unknown
        }
    };
}

/// FrontendCompiler implements a basic and (optional)type-checking compiler
pub struct FrontendCompiler<'c> {
    mir: &'c Mir,
    module: &'c mut Module,
    return_types: Vec<Type>,
}

impl<'c> FrontendCompiler<'c> {
    pub fn new(module: &'c mut Module, mir: &'c Mir) -> Self {
        Self {
            module,
            mir,
            return_types: vec![],
        }
    }

    //fn collect_return_type(&mut self) -> Option<Type> {
    //    if self.return_types.is_empty() {
    //        return None;
    //    }

    //    let mut return_types = self.return_types.drain(..).collect::<Vec<_>>();

    //    Some(if !return_types.is_empty() && is_same_type(&return_types) {
    //        return_types.remove(0)
    //    } else {
    //        Type::Unknown
    //    })
    //}

    fn compile_single_type(&mut self, scope: &'c Scope, values: &[Value]) -> Result<Type> {
        let mut types = vec![];

        for value in values {
            types.push(self.compile_type(scope, value)?);
        }

        Ok(parse_type(&mut types))
    }

    fn compile_index_type(&mut self, scope: &'c Scope, index: &Index) -> Result<Type> {
        let object = self.compile_type(scope, &index.object)?;
        let index_type = self.compile_type(scope, &index.index)?;

        if object.is_typed() && !matches!(object, Type::Array(_)) {
            return Err(CompileError::new(format!("unable to index: {}", object))
                .with_location(index.object.loc.clone()));
        }

        if index_type.is_typed() && !matches!(index_type, Type::Primitive(PrimitiveType::Int)) {
            return Err(
                CompileError::new(format!("unable to use '{}' as index", index_type))
                    .with_location(index.object.loc.clone()),
            );
        }

        Ok(unwrap_type!(Array, object))
    }

    fn register_function(&mut self, function: &mir::Function, public: bool) {
        if public {
            self.module.exports.insert(
                function.name.clone(),
                Type::Fn(Id::new(self.module.name.clone(), function.name.clone())),
            );
        }

        self.module.funcs.insert(
            function.name.clone(),
            Func {
                name: function.name.clone(),
                is_extern: function.is_extern,
                args: function.args.clone(),
                ..Func::default()
            },
        );
    }

    fn register_class(&mut self, class: &mir::Class, public: bool) {
        if public {
            self.module.exports.insert(
                class.name.clone(),
                Type::Class(Id::new(self.module.name.clone(), class.name.clone())),
            );
        }

        let mut output = Class {
            name: class.name.clone(),
            fields: class
                .fields
                .iter()
                .cloned()
                .map(|field| (field.name.clone(), field))
                .collect(),
            ..Class::default()
        };

        for function in class.methods.iter() {
            output.methods.insert(
                function.name.clone(),
                Func {
                    name: function.name.clone(),
                    is_extern: function.is_extern,
                    args: function.args.clone(),
                    ..Func::default()
                },
            );
        }

        self.module.classes.insert(class.name.clone(), output);
    }

    fn register_interface(&mut self, interface: &mir::Interface, public: bool) {
        if public {
            self.module.exports.insert(
                interface.name.clone(),
                Type::Interface(Id::new(self.module.name.clone(), interface.name.clone())),
            );
        }

        self.module.interfaces.insert(
            interface.name.clone(),
            Interface {
                name: interface.name.clone(),
                functions: interface.functions.clone(),
            },
        );
    }

    fn compile_type(&mut self, scope: &'c Scope, value: &Value) -> Result<Type> {
        Ok(match &value.kind {
            ValueKind::Const(val) => Type::Primitive(match val {
                Const::Int128(_) | Const::Int64(_) | Const::Uint64(_) | Const::Int32(_) => {
                    PrimitiveType::Int
                }
                Const::Byte(_) => PrimitiveType::Byte,
                Const::Float(_) => PrimitiveType::Float,
                Const::Bool(_) => PrimitiveType::Bool,
                Const::Char(_) => PrimitiveType::Char,
                Const::Symbol(_) => PrimitiveType::Symbol,
                Const::String(_) => PrimitiveType::String,
            }),
            ValueKind::Receiver => Type::Unknown,
            ValueKind::Local(id) => self.mir.get_local(scope, *id).known_type.clone(),
            ValueKind::Name(name) => {
                if let Some((_, _, import)) = self.module.imports.get_full(name) {
                    import.known_type.clone()
                } else if let Some(func) = self.module.funcs.get(name) {
                    Type::Fn(Id::new(self.module.name.clone(), func.name.clone()))
                } else if let Some(class) = self.module.classes.get(name) {
                    Type::Class(Id::new(self.module.name.clone(), class.name.clone()))
                } else if let Some(interface) = self.module.interfaces.get(name) {
                    Type::Interface(Id::new(self.module.name.clone(), interface.name.clone()))
                } else {
                    return Err(CompileError::new(format!("no such name: {}", name)));
                }
            }
            // @TODO: can we predict the resulting type?
            ValueKind::Cast(cast) => {
                self.compile_type(scope, &cast.value)?;

                Type::Unknown
            }
            // @TODO: we could parse the `return_type` field of the function
            ValueKind::Call(call) => {
                self.compile_type(scope, &call.callee)?;

                for arg in call.args.iter() {
                    self.compile_type(scope, arg)?;
                }

                Type::Unknown
            }
            ValueKind::Unwrap(value) => {
                let value_type = self.compile_type(scope, value)?;

                if value_type.is_typed() && !matches!(value_type, Type::Option(_)) {
                    return Err(
                        CompileError::new(format!("unable to unwrap: {}", value_type))
                            .with_location(value.loc.clone()),
                    );
                }

                unwrap_type!(Option, value_type)
            }
            ValueKind::Array(values) => {
                let item = self.compile_single_type(scope, values)?;

                Type::Array(Box::new(item))
            }
            ValueKind::Tuple(values) => {
                let mut items = vec![];

                for value in values.iter() {
                    items.push(self.compile_type(scope, value)?);
                }

                Type::Tuple(items)
            }
            // @TODO: return the proper type here
            ValueKind::Closure(_) => Type::Unknown,
            ValueKind::Member(member) => {
                self.compile_type(scope, &member.object)?;

                // @TODO: can we parse the type of the field?
                Type::Unknown
            }
            ValueKind::Comparison(comparison) => {
                let left = self.compile_type(scope, &comparison.left)?;
                let right = self.compile_type(scope, &comparison.right)?;

                if left.is_typed() && right.is_typed() && left != right {
                    return Err(CompileError::new(format!(
                        "unable to compare '{}' and: {}",
                        left, right
                    ))
                    .with_location(comparison.left.loc.clone()));
                }

                Type::Primitive(PrimitiveType::Bool)
            }
            ValueKind::Arithmetic(arithmetic) => {
                let left = self.compile_type(scope, &arithmetic.left)?;
                self.compile_type(scope, &arithmetic.right)?;

                // @TODO: do we know for sure the resulting type is always the left-hand side type?
                left
            }
            ValueKind::Logical(logical) => {
                let left = self.compile_type(scope, &logical.left)?;

                if left.is_typed() && !matches!(left, Type::Primitive(PrimitiveType::Bool)) {
                    return Err(CompileError::new(format!(
                        "unable to use '{}' as condition",
                        left
                    ))
                    .with_location(logical.left.loc.clone()));
                }

                let right = self.compile_type(scope, &logical.right)?;

                if right.is_typed() && !matches!(right, Type::Primitive(PrimitiveType::Bool)) {
                    return Err(CompileError::new(format!(
                        "unable to use '{}' as condition",
                        right
                    ))
                    .with_location(logical.right.loc.clone()));
                }

                Type::Primitive(PrimitiveType::Bool)
            }
            ValueKind::MakeRef(make_ref) => {
                let inner_type = self.compile_type(scope, make_ref)?;

                Type::Ref(Box::new(inner_type))
            }
            ValueKind::Deref(deref) => {
                let ref_type = self.compile_type(scope, deref)?;

                if ref_type.is_typed() && !matches!(ref_type, Type::Ref(_)) {
                    return Err(CompileError::new(format!("unable to deref: {}", ref_type))
                        .with_location(deref.loc.clone()));
                }

                unwrap_type!(Ref, ref_type)
            }
            ValueKind::Index(index) => self.compile_index_type(scope, index)?,
            ValueKind::Slice(slice) => {
                let array = self.compile_type(scope, &slice.object)?;

                if array.is_typed() && !matches!(array, Type::Array(_)) {
                    return Err(CompileError::new(format!("unable to slice: {}", array))
                        .with_location(slice.object.loc.clone()));
                }

                let begin = self.compile_type(scope, &slice.begin)?;

                if begin.is_typed() && !matches!(begin, Type::Primitive(PrimitiveType::Int)) {
                    return Err(
                        CompileError::new(format!("unable to use '{}' to slice", begin))
                            .with_location(slice.object.loc.clone()),
                    );
                }

                let end = self.compile_type(scope, &slice.end)?;

                if end.is_typed() && !matches!(end, Type::Primitive(PrimitiveType::Int)) {
                    return Err(
                        CompileError::new(format!("unable to use '{}' to slice", end))
                            .with_location(slice.object.loc.clone()),
                    );
                }

                array
            }
            ValueKind::Range(range) => {
                self.compile_type(scope, &range.begin)?;
                self.compile_type(scope, &range.end)?;

                Type::Unknown
            }
            ValueKind::Template(_) => Type::Primitive(PrimitiveType::String),
            ValueKind::TypeAssert(type_assert) => {
                self.compile_type(scope, &type_assert.left)?;

                Type::Primitive(PrimitiveType::Bool)
            }
        })
    }

    fn check_assign(&mut self, scope: &'c Scope, assign: &Assign) -> Result<()> {
        self.compile_type(scope, &assign.right)?;

        match &assign.left {
            AssignLeftHand::Local(id) => {
                // @TODO: implement optional check
                self.mir.get_local(scope, *id);
            }
            AssignLeftHand::Index(index) => {
                self.compile_index_type(scope, index)?;
            }
            AssignLeftHand::Member(member) => {
                // @TODO: should we validate something here?
                self.compile_type(scope, &member.object)?;
            }
        }

        Ok(())
    }

    fn walk_stmt(&mut self, scope: &'c Scope, stmt: &Stmt) -> Result<()> {
        match &stmt.kind {
            StmtKind::Assign(assign) => self.check_assign(scope, assign)?,
            StmtKind::Cond(cond) => {
                let cond_type = self.compile_type(scope, &cond.condition)?;

                if cond_type.is_typed()
                    && !matches!(cond_type, Type::Primitive(PrimitiveType::Bool))
                {
                    return Err(CompileError::new(format!(
                        "unable to use '{}' as condition",
                        cond_type
                    ))
                    .with_location(stmt.loc.clone()));
                }

                self.walk_block(&cond.block)?;

                if let Some(block) = &cond.alt {
                    self.walk_block(block)?;
                }
            }
            StmtKind::Loop(block) => self.walk_block(block)?,
            StmtKind::Eval(value) => {
                self.compile_type(scope, value)?;
            }
            StmtKind::Return(value) => {
                let value = self.compile_type(scope, value)?;

                self.return_types.push(value);
            }
        }

        Ok(())
    }

    fn walk_block(&mut self, block: &Block) -> Result<()> {
        let scope = unsafe { self.mir.scopes.get_unchecked(block.scope_id) };

        for stmt in block.statements.iter() {
            self.walk_stmt(scope, stmt)?;
        }

        Ok(())
    }

    fn walk_function(&mut self, function: &Function) -> Result<()> {
        self.walk_block(&function.block)
    }

    pub fn compile(mut self) -> Result<()> {
        // Register global types early
        for decl in self.mir.program.iter() {
            match &decl.kind {
                DeclKind::Function(function) => self.register_function(function, decl.public),
                DeclKind::Class(class) => self.register_class(class, decl.public),
                DeclKind::Interface(interface) => self.register_interface(interface, decl.public),
            }
        }

        for decl in self.mir.program.iter() {
            match &decl.kind {
                DeclKind::Class(class) => {
                    for method in class.methods.iter() {
                        self.walk_function(method)?;
                    }
                }
                DeclKind::Function(function) => self.walk_function(function)?,
                _ => continue,
            }
        }

        Ok(())
    }
}
