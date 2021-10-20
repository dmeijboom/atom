use std::collections::HashMap;
use std::ops::Range;
use std::path::Path;
use std::rc::Rc;
use std::sync::RwLock;

use indexmap::map::IndexMap;
use wyhash2::WyHash;

use atom_ir::{Code, Label, Location, IR};

use crate::ast::{
    ArithmeticExpr, ArithmeticOp, AssignOp, ClassDeclStmt, ClosureExpr, Expr, ExternFnDeclStmt,
    FnDeclStmt, InterfaceDeclStmt, Literal, LogicalOp, Pos, Stmt, TemplateComponent, Variable,
};
use crate::compiler::module::Import;
use crate::compiler::types::MapType;
use crate::parser;
use crate::std::core::DEFAULT_IMPORTS;

use super::filesystem::{FileSystem, FileSystemCache};
use super::module::{Class, Field, Func, FuncArg, Interface, Module};
use super::optimizers::{call_void, load_local_twice_add, pre_compute_labels, Optimizer};
use super::optimizers::{remove_core_validations, remove_type_cast};
use super::result::{CompileError, Result};
use super::scope::{ForLoopMeta, Local, Scope, ScopeContext, ScopeGraph};
use super::type_checker::TypeChecker;
use super::types::{self, Type};

pub fn parse_line_numbers_offset(source: &str) -> Vec<usize> {
    let mut i = 0;
    let mut line_numbers_offset = vec![];

    while i < source.len() {
        if let Some(offset) = source[i + 1..].find('\n') {
            line_numbers_offset.push(offset + i);

            i += offset + 1;

            continue;
        }

        break;
    }

    line_numbers_offset
}

fn validate_unique(names: &[(&str, &str)]) -> Result<()> {
    for (i, (_, name)) in names.iter().enumerate() {
        let other = names
            .iter()
            .enumerate()
            .find(|(other_index, (_, other_name))| *other_index != i && name == other_name);

        if let Some((_, (typename, name))) = other {
            return Err(CompileError::new(format!(
                "unable to redefine {}: {}",
                typename, name
            )));
        }
    }

    Ok(())
}

const STD_SOURCES: [(&str, &str); 6] = [
    ("std.core", include_str!("../std/atom/std/core.atom")),
    ("std.map", include_str!("../std/atom/std/map.atom")),
    ("std.io", include_str!("../std/atom/std/io.atom")),
    (
        "std.encoding.utf8",
        include_str!("../std/atom/std/encoding/utf8.atom"),
    ),
    (
        "std.encoding.binary",
        include_str!("../std/atom/std/encoding/binary.atom"),
    ),
    (
        "std.encoding.json",
        include_str!("../std/atom/std/encoding/json.atom"),
    ),
];

pub struct Compiler {
    pos: Pos,
    fs: FileSystem,
    optimize: bool,
    module: Module,
    tree: Vec<Stmt>,
    scope: ScopeGraph,
    type_checker: TypeChecker,
    labels: HashMap<String, bool, WyHash>,
    modules: Rc<RwLock<HashMap<String, Module>>>,
    optimizers: Vec<Optimizer>,
    // Unfortunately the parser doesn't expose line or column information so we're using a map of
    // newline positions to calculate the line number and column instead
    line_numbers_offset: Vec<usize>,
}

impl Compiler {
    pub fn new(tree: Vec<Stmt>, line_numbers_offset: Vec<usize>, optimize: bool) -> Self {
        let mut cache = FileSystemCache::new();

        for (module_name, source) in STD_SOURCES {
            cache.add_module(module_name.to_string(), source);
        }

        let fs = FileSystem::new(cache);

        Self {
            tree,
            fs,
            optimize,
            pos: 0..0,
            labels: HashMap::with_hasher(WyHash::default()),
            module: Module::new(),
            optimizers: if optimize {
                vec![
                    remove_type_cast::optimize,
                    call_void::optimize,
                    load_local_twice_add::optimize,
                    remove_core_validations::optimize,
                    pre_compute_labels::optimize,
                ]
            } else {
                vec![]
            },
            type_checker: TypeChecker::new(),
            scope: ScopeGraph::new(),
            line_numbers_offset,
            modules: Rc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn add_lookup_path(&mut self, path: impl AsRef<Path>) {
        self.fs.add_path(path.as_ref().to_path_buf());
    }

    #[inline(always)]
    fn get_location_by_offset(&self, offset: &Range<usize>) -> Location {
        let index = self
            .line_numbers_offset
            .iter()
            .position(|start| offset.start < *start);

        if let Some(index) = index {
            let length = self.line_numbers_offset.len();

            if length > 0 && index > 0 {
                let start = self.line_numbers_offset[index - 1];

                return Location::new(offset.clone(), index + 1, offset.start - start + 1);
            }
        }

        Location::new(offset.clone(), 1, offset.start + 1)
    }

    fn get_location(&self) -> Location {
        self.get_location_by_offset(&self.pos)
    }

    #[inline(always)]
    fn enter_scope(&mut self, context: ScopeContext) {
        let new_scope = Scope::new_child(context);

        self.scope.push(new_scope);
    }

    #[inline(always)]
    fn exit_scope(&mut self) {
        self.scope.pop();
    }

    #[inline(always)]
    fn fork(&self, module_name: String, tree: Vec<Stmt>, line_numbers_offset: Vec<usize>) -> Self {
        Self {
            fs: self.fs.clone(),
            pos: 0..0,
            optimize: self.optimize,
            module: Module::with_name(module_name),
            tree,
            scope: ScopeGraph::new(),
            labels: HashMap::with_hasher(WyHash::default()),
            line_numbers_offset,
            type_checker: TypeChecker::new(),
            optimizers: self.optimizers.clone(),
            modules: Rc::clone(&self.modules),
        }
    }

    #[inline(always)]
    fn compile_store(&self, local: &Local) -> Code {
        if local.mutable {
            Code::StoreMut(local.id)
        } else {
            Code::Store(local.id)
        }
    }

    fn make_label(&mut self, prefix: &str) -> String {
        let mut i: i64 = 0;

        loop {
            let label = if i == 0 {
                prefix.to_string()
            } else {
                format!("{}{}", prefix, i)
            };

            if !self.labels.contains_key(&label) {
                self.labels.insert(label.clone(), true);

                return label;
            }

            i += 1;
        }
    }

    fn compile_expr(&mut self, ir: &mut IR, expr: &Expr) -> Result<Type> {
        self.pos = expr.pos();

        let location = self.get_location();
        let location = Some(&location);

        Ok(match &expr {
            Expr::Literal(literal_expr) => {
                let (code, known_type) = match &literal_expr.literal {
                    Literal::Int128(val) => (Code::ConstInt128(*val), types::INT.clone()),
                    Literal::Int64(val) => (Code::ConstInt64(*val), types::INT.clone()),
                    Literal::Uint64(val) => (Code::ConstUint64(*val), types::INT.clone()),
                    Literal::Int32(val) => (Code::ConstInt32(*val), types::INT.clone()),
                    Literal::Byte(val) => (Code::ConstByte(*val), types::BYTE.clone()),
                    Literal::Float(val) => (Code::ConstFloat(*val), types::FLOAT.clone()),
                    Literal::Bool(val) => (Code::ConstBool(*val), types::BOOL.clone()),
                    Literal::Char(val) => (Code::ConstChar(*val), types::CHAR.clone()),
                    Literal::Symbol(name) => {
                        (Code::ConstSymbol(name.clone()), types::SYMBOL.clone())
                    }
                    Literal::String(val) => (Code::ConstString(val.clone()), types::STRING.clone()),
                };

                ir.add(code, location);

                known_type
            }
            Expr::Template(template_expr) => {
                for component in template_expr.components.iter() {
                    match component {
                        TemplateComponent::String(s) => {
                            ir.add(Code::ConstString(s.clone()), location);
                        }
                        TemplateComponent::Expr(expr) => {
                            self.compile_expr(ir, expr)?;
                        }
                    }
                }

                ir.add(Code::MakeTemplate(template_expr.components.len()), location);

                types::STRING.clone()
            }
            Expr::Range(range_expr) => {
                self.compile_expr(ir, &range_expr.from)?;
                self.compile_expr(ir, &range_expr.to)?;

                ir.add(Code::MakeRange, location);

                // @TODO: return an actual type
                Type::Unknown
            }
            Expr::Ident(ident) => {
                let (code, known_type) = self.compile_name(&ident.name)?;

                ir.add(code, location);

                known_type
            }
            Expr::Cast(cast_expr) => {
                self.compile_expr(ir, &cast_expr.expr)?;
                ir.add(Code::Cast(cast_expr.type_name.clone()), location);

                Type::Unknown
            }
            Expr::Call(call_expr) => {
                let mut names = vec![];

                for arg in call_expr.keyword_args.iter() {
                    names.push(arg.name.clone());
                    self.compile_expr(ir, &arg.value)?;
                }

                for arg in call_expr.args.iter() {
                    self.compile_expr(ir, arg)?;
                }

                let instruction = if names.is_empty() {
                    Code::Call(call_expr.args.len())
                } else {
                    // Make sure each keyword argument is unique
                    if !names.is_empty() {
                        let mut unique_keys = vec![];

                        for key in names.iter() {
                            if unique_keys.contains(key) {
                                return Err(CompileError::new(format!(
                                    "duplicate keyword argument: {}",
                                    key
                                )));
                            }

                            unique_keys.push(key.to_string());
                        }
                    }

                    Code::CallKeywords((names, call_expr.keyword_args.len() + call_expr.args.len()))
                };

                let find_target_match = || {
                    if let Some((target, is_method)) = self.scope.get_function_target() {
                        if !is_method {
                            if let Expr::Ident(ident_expr) = &call_expr.callee {
                                return (
                                    ident_expr.name == target,
                                    self.module.get_fn_by_name(&target).is_none(),
                                );
                            }
                        }
                    }

                    (false, false)
                };

                let (is_target, force_optimize) = find_target_match();

                // If the target refers to a closure it can't be referenced by it's name (at least not globally).
                // So we have to use the 'LoadTarget' instruction instead even without optimizations enabled.
                // Otherwise it won't work
                if (self.optimize || force_optimize) && is_target {
                    ir.add(Code::LoadTarget, location);
                } else {
                    self.compile_expr(ir, &call_expr.callee)?;
                }

                ir.add(instruction, location);

                Type::Unknown
            }
            Expr::Unwrap(unwrap_expr) => {
                let known_type = self.compile_expr(ir, &unwrap_expr.expr)?;
                ir.add(Code::Unwrap, location);

                self.type_checker.unwrap_option(known_type)?
            }
            Expr::Not(not_expr) => {
                let condition = self.compile_expr(ir, &not_expr.expr)?;
                ir.add(Code::Not, location);

                self.type_checker.condition(condition)?
            }
            Expr::Index(index_expr) => {
                let slice_params = if let Expr::Range(range) = &index_expr.index {
                    Some((&range.from, &range.to))
                } else {
                    None
                };

                // Contrary to 'normal' index operations the slice operation is safe
                if slice_params.is_none() && !self.scope.in_unsafe_block() {
                    return Err(CompileError::new(
                        "unable to perform index operation outside of an 'unsafe' block"
                            .to_string(),
                    ));
                }

                let array_type = self.compile_expr(ir, &index_expr.object)?;

                match slice_params {
                    Some((from, to)) => {
                        self.compile_expr(ir, from)?;
                        self.compile_expr(ir, to)?;
                        ir.add(Code::MakeSlice, location);

                        array_type
                    }
                    None => {
                        self.compile_expr(ir, &index_expr.index)?;
                        ir.add(Code::LoadIndex, location);

                        self.type_checker.index(array_type)?
                    }
                }
            }
            Expr::Tuple(tuple_expr) => {
                let mut item_types = vec![];

                for item in tuple_expr.items.iter() {
                    item_types.push(self.compile_expr(ir, item)?);
                }

                ir.add(Code::MakeTuple(tuple_expr.items.len()), location);

                Type::Tuple(item_types)
            }
            Expr::Array(array_expr) => {
                for item in array_expr.items.iter() {
                    self.compile_expr(ir, item)?;
                }

                ir.add(Code::MakeArray(array_expr.items.len()), location);

                Type::Array(Box::new(Type::Unknown))
            }
            Expr::Map(map_expr) => {
                let map_type = Type::Map(Box::new(MapType::new(Type::Unknown, Type::Unknown)));
                let local = self
                    .scope
                    .set_local("__map__".to_string(), false, map_type.clone())?;
                let new_map_id = self.module.imports.get_index_of("newMap").unwrap();

                ir.add(Code::LoadGlobal(new_map_id), location);
                ir.add(Code::Call(0), location);
                ir.add(Code::Store(local.id), location);

                for key_val in map_expr.key_values.iter() {
                    self.compile_expr(ir, &key_val.key)?;
                    self.compile_expr(ir, &key_val.value)?;
                    ir.add(Code::Load(local.id), location);
                    ir.add(Code::LoadMember("set".to_string()), location);
                    ir.add(Code::CallVoid(2), location);
                }

                ir.add(Code::Load(local.id), location);

                map_type
            }
            Expr::Closure(closure_expr) => {
                ir.add(self.compile_closure(closure_expr, None)?, location);

                Type::Unknown
            }
            Expr::MemberCond(_) => unreachable!("not supported yet"),
            Expr::Member(member_expr) => {
                self.compile_expr(ir, &member_expr.object)?;

                ir.add(Code::LoadMember(member_expr.member.to_string()), location);

                Type::Unknown
            }
            Expr::Arithmetic(arithmetic_expr) => {
                self.compile_expr(ir, &arithmetic_expr.left)?;
                self.compile_expr(ir, &arithmetic_expr.right)?;
                ir.add(arithmetic_expr.op.into(), location);

                Type::Unknown
            }
            Expr::TypeAssert(type_assert_expr) => {
                self.compile_expr(ir, &type_assert_expr.left)?;
                self.compile_expr(ir, &type_assert_expr.right)?;
                ir.add(Code::AssertIsType, location);

                types::BOOL.clone()
            }
            Expr::Comparison(comparison_expr) => {
                self.compile_expr(ir, &comparison_expr.left)?;
                self.compile_expr(ir, &comparison_expr.right)?;
                ir.add(comparison_expr.op.into(), location);

                types::BOOL.clone()
            }
            Expr::Logical(logical_expr) => {
                match logical_expr.op {
                    LogicalOp::And => {
                        self.compile_expr(ir, &logical_expr.left)?;
                        self.compile_expr(ir, &logical_expr.right)?;
                        ir.add(Code::LogicalAnd, location);
                    }
                    LogicalOp::Or => {
                        self.compile_expr(ir, &logical_expr.left)?;

                        let label = self.make_label("or");

                        ir.add(Code::JumpIfTrue(Label::new(label.clone())), location);
                        self.compile_expr(ir, &logical_expr.right)?;
                        ir.add(Code::SetLabel(label), location);
                    }
                };

                types::BOOL.clone()
            }
            Expr::MakeRef(make_ref_expr) => {
                let inner_type = self.compile_expr(ir, &make_ref_expr.expr)?;
                ir.add(Code::MakeRef, location);

                Type::Ref(Box::new(inner_type))
            }
            Expr::Deref(deref_expr) => {
                let ref_type = self.compile_expr(ir, &deref_expr.expr)?;

                ir.add(Code::Deref, location);

                self.type_checker.deref(ref_type)?
            }
        })
    }

    fn compile_name(&mut self, name: &str) -> Result<(Code, Type)> {
        if name == "this" && self.scope.in_function_block() {
            Ok((Code::LoadReceiver, Type::Unknown))
        } else if let Some(local) = self.scope.get_local(name, true) {
            Ok((Code::Load(local.id), local.known_type))
        } else if let Some((id, _, import)) = self.module.imports.get_full(name) {
            Ok((Code::LoadGlobal(id), import.known_type.clone()))
        } else if let Some(id) = self.module.funcs.iter().position(|func| func.name == name) {
            Ok((Code::LoadFn(id), Type::Fn(name.to_string())))
        } else if let Some(id) = self.module.classes.get_index_of(name) {
            Ok((Code::LoadClass(id), Type::Class(name.to_string())))
        } else if let Some(id) = self.module.interfaces.get_index_of(name) {
            Ok((Code::LoadInterface(id), Type::Interface(name.to_string())))
        } else {
            let index = self.tree.iter().position(|stmt| match stmt {
                Stmt::FnDecl(fn_decl_stmt) if fn_decl_stmt.name == name => true,
                Stmt::ExternFnDecl(extern_fn_decl) if extern_fn_decl.name == name => true,
                Stmt::ClassDecl(class_decl) if class_decl.name == name => true,
                Stmt::InterfaceDecl(interface_decl) if interface_decl.name == name => true,
                _ => false,
            });

            if let Some(index) = index {
                let stmt = self.tree.remove(index);

                self.compile_top_level_stmt(stmt)?;

                return self.compile_name(name);
            }

            Err(CompileError::new(format!("no such name: {}", name)))
        }
    }

    fn compile_closure(
        &mut self,
        closure_expr: &ClosureExpr,
        target: Option<String>,
    ) -> Result<Code> {
        let mut args = vec![];

        self.enter_scope(ScopeContext::Function((target.unwrap_or_default(), false)));

        // As the vm will copy all the locals of the parent function we need to pretend like we set the
        // locals of the parent function
        if let Some(parent) = self.scope.get_by_id(self.scope.len() - 2) {
            self.scope.current_mut().local_id = parent.local_id;
        }

        for arg in closure_expr.args.iter() {
            self.scope
                .set_local(arg.name.clone(), arg.mutable, Type::Unknown)?;

            args.push(FuncArg {
                mutable: arg.mutable,
                name: arg.name.clone(),
            });
        }

        let body = self._compile_stmt_list(&closure_expr.body)?;

        self.exit_scope();

        let id = self.module.funcs.len();

        self.module.funcs.push(Func {
            is_void: !body.iter().any(|code| code == &Code::Return),
            body,
            args,
            location: self.get_location(),
            name: "<closure>".to_string(),
            is_closure: true,
            is_extern: false,
        });

        Ok(Code::MakeClosure(id))
    }

    fn compile_assign_local(&mut self, ir: &mut IR, name: &str, value: &Expr) -> Result<Type> {
        if let Some(local) = self.scope.get_local(name, true) {
            if !local.mutable {
                return Err(CompileError::new(format!("name is not mutable: {}", name)));
            }

            self.compile_name_value(ir, name, value)?;
            ir.add(self.compile_store(&local), Some(&self.get_location()));

            return Ok(local.known_type);
        }

        Err(CompileError::new(format!(
            "unable to assign value to unknown name: {}",
            name
        )))
    }

    #[inline(always)]
    fn compile_assign_member(
        &mut self,
        ir: &mut IR,
        object: &Expr,
        member: &str,
        value: &Expr,
    ) -> Result<()> {
        self.compile_expr(ir, value)?;
        self.compile_expr(ir, object)?;
        ir.add(
            Code::StoreMember(member.to_string()),
            Some(&self.get_location()),
        );

        Ok(())
    }

    #[inline(always)]
    fn compile_assign_index(
        &mut self,
        ir: &mut IR,
        object: &Expr,
        index: &Expr,
        value: &Expr,
    ) -> Result<Type> {
        let index_type = self.compile_expr(ir, object)?;
        self.compile_expr(ir, index)?;
        self.compile_expr(ir, value)?;
        ir.add(Code::StoreIndex, Some(&self.get_location()));

        self.type_checker.index(index_type)
    }

    fn compile_assign(&mut self, ir: &mut IR, left: &Expr, right: &Expr) -> Result<Type> {
        match left {
            Expr::Index(index_expr) => {
                self.compile_assign_index(ir, &index_expr.object, &index_expr.index, right)
            }
            Expr::Ident(ident_expr) => self.compile_assign_local(ir, &ident_expr.name, right),
            Expr::Member(member_expr) => {
                self.compile_assign_member(ir, &member_expr.object, &member_expr.member, right)?;

                Ok(Type::Unknown)
            }
            _ => Err(CompileError::new(
                "invalid left-hand side in assignment".to_string(),
            )),
        }
    }

    #[inline(always)]
    fn declare_local(&mut self, name: &str, mutable: bool, known_type: Type) -> Result<Local> {
        if self.scope.get_local(name, false).is_some() {
            Err(CompileError::new(format!("name already defined: {}", name)))
        } else {
            Ok(self
                .scope
                .set_local(name.to_string(), mutable, known_type)?)
        }
    }

    fn compile_name_value(&mut self, ir: &mut IR, name: &str, value: &Expr) -> Result<Type> {
        if let Expr::Closure(closure_expr) = value {
            ir.add(
                self.compile_closure(closure_expr, Some(name.to_string()))?,
                Some(&self.get_location()),
            );

            Ok(Type::Closure(name.to_string()))
        } else {
            self.compile_expr(ir, value)
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt, ir: &mut IR) -> Result<()> {
        self.pos = stmt.pos();

        let location = self.get_location();
        let location = Some(&location);

        match stmt {
            Stmt::If(if_stmt) => {
                let if_label = self.make_label("if");
                let else_label = self.make_label("else");
                let cont_label = self.make_label("if_else_cont");

                let condition = self.compile_expr(ir, &if_stmt.cond)?;
                self.type_checker.condition(condition)?;

                ir.add(
                    Code::Branch((
                        Label::new(if_label.clone()),
                        Label::new(if if_stmt.alt.is_none() {
                            cont_label.clone()
                        } else {
                            else_label.clone()
                        }),
                    )),
                    location,
                );
                ir.add(Code::SetLabel(if_label), location);
                ir.append(self.compile_stmt_list(ScopeContext::IfElse, &if_stmt.body)?);

                if let Some(alt) = &if_stmt.alt {
                    ir.add(Code::Jump(Label::new(cont_label.clone())), location);
                    ir.add(Code::SetLabel(else_label), location);

                    self.enter_scope(ScopeContext::IfElse);
                    self.compile_stmt(alt, ir)?;
                    self.exit_scope();

                    ir.add(Code::Jump(Label::new(cont_label.clone())), location);
                }

                ir.add(Code::SetLabel(cont_label), location);
            }
            Stmt::Else(else_stmt) => ir.append(self._compile_stmt_list(&else_stmt.body)?),
            Stmt::Expr(expr_stmt) => {
                self.compile_expr(ir, &expr_stmt.expr)?;
                ir.add(Code::Discard, location);
            }
            Stmt::Let(let_stmt) => match &let_stmt.var {
                Variable::Name(name) => {
                    let local_type = self.compile_name_value(ir, name, &let_stmt.value)?;
                    let local = self.declare_local(name, let_stmt.mutable, local_type)?;

                    ir.add(self.compile_store(&local), location);
                }
                Variable::Tuple(names) | Variable::Array(names) => {
                    self.compile_expr(ir, &let_stmt.value)?;

                    for (i, name) in names.iter().enumerate() {
                        let local = self.declare_local(name, let_stmt.mutable, Type::Unknown)?;

                        ir.add(Code::ConstUint64(i as u64), location);
                        // We need to keep the current value until the last item was processed
                        ir.add(
                            if i == names.len() - 1 {
                                Code::LoadIndex
                            } else {
                                Code::TeeIndex
                            },
                            location,
                        );
                        ir.add(self.compile_store(&local), location);
                    }
                }
            },
            Stmt::LetDecl(let_decl_stmt) => {
                self.declare_local(&let_decl_stmt.name, true, Type::Unknown)?;
            }
            Stmt::Assign(assign_stmt) => {
                if let Some(op) = &assign_stmt.op {
                    self.compile_assign(
                        ir,
                        &assign_stmt.left,
                        &Expr::Arithmetic(
                            ArithmeticExpr {
                                left: assign_stmt.left.clone(),
                                right: assign_stmt.right.clone(),
                                op: match op {
                                    AssignOp::Mul => ArithmeticOp::Mul,
                                    AssignOp::Div => ArithmeticOp::Div,
                                    AssignOp::Add => ArithmeticOp::Add,
                                    AssignOp::Sub => ArithmeticOp::Sub,
                                },
                                pos: assign_stmt.pos.clone(),
                            }
                            .into(),
                        ),
                    )?;
                } else {
                    self.compile_assign(ir, &assign_stmt.left, &assign_stmt.right)?;
                }
            }
            Stmt::For(for_stmt) => {
                let for_label = self.make_label("for");
                let body_label = self.make_label("for_body");
                let cont_label = self.make_label("for_cont");

                if let Some(expr) = &for_stmt.expr {
                    let iter = self.scope.set_local(
                        "__iter__".to_string(),
                        false,
                        Type::Interface("Iterable".to_string()),
                    )?;

                    self.compile_expr(ir, expr)?;

                    self.enter_scope(ScopeContext::ForLoop(ForLoopMeta {
                        continue_label: cont_label.clone(),
                    }));

                    let local = self.scope.set_local(
                        match &for_stmt.alias {
                            Some(Variable::Name(name)) => name.clone(),
                            _ => "__item__".to_string(),
                        },
                        false,
                        Type::Unknown,
                    )?;

                    // Step 1. Get the iterator from the object
                    ir.add(self.compile_name("Iterable")?.0, location);
                    ir.add(Code::Validate, location);
                    ir.add(Code::LoadMember("iter".to_string()), location);
                    ir.add(Code::Call(0), location);
                    ir.add(Code::Store(iter.id), location);
                    // Step 2. Now in the loop, get the next value from the iterator
                    ir.add(Code::SetLabel(for_label.clone()), location);
                    ir.add(Code::Load(iter.id), location);
                    ir.add(Code::LoadMember("next".to_string()), location);
                    ir.add(Code::Call(0), location);
                    ir.add(Code::Store(local.id), location);
                    // Step 3. Check if it has a value and either continue or stop
                    ir.add(Code::Load(local.id), location);
                    ir.add(Code::LoadMember("isSome".to_string()), location);
                    ir.add(Code::Call(0), location);
                    ir.add(
                        Code::Branch((
                            Label::new(body_label.clone()),
                            Label::new(cont_label.clone()),
                        )),
                        location,
                    );
                    // Step 4. Evaluate the body and so on..
                    ir.add(Code::SetLabel(body_label), location);

                    // Only store the current item when requested
                    if let Some(var) = &for_stmt.alias {
                        ir.add(Code::Load(local.id), location);
                        ir.add(Code::Unwrap, location);

                        match var {
                            // If it's a name we can re-use the local slot
                            Variable::Name(_) => {
                                ir.add(Code::Store(local.id), location);
                            }
                            Variable::Tuple(names) | Variable::Array(names) => {
                                for (i, name) in names.iter().enumerate() {
                                    let local =
                                        self.scope.set_local(name.clone(), false, Type::Unknown)?;

                                    ir.add(Code::ConstUint64(i as u64), location);
                                    ir.add(
                                        if i == names.len() - 1 {
                                            Code::LoadIndex
                                        } else {
                                            Code::TeeIndex
                                        },
                                        location,
                                    );
                                    ir.add(Code::Store(local.id), location);
                                }
                            }
                        };
                    }

                    ir.append(self._compile_stmt_list(&for_stmt.body)?);
                } else {
                    self.enter_scope(ScopeContext::ForLoop(ForLoopMeta {
                        continue_label: cont_label.clone(),
                    }));

                    ir.add(Code::SetLabel(for_label.clone()), location);
                    ir.add(Code::SetLabel(body_label), location);
                    ir.append(self.compile_stmt_list(
                        ScopeContext::ForLoop(ForLoopMeta {
                            continue_label: cont_label.clone(),
                        }),
                        &for_stmt.body,
                    )?);
                }

                ir.add(Code::Jump(Label::new(for_label)), location);
                ir.add(Code::SetLabel(cont_label), location);

                self.exit_scope();
            }
            Stmt::Break(break_stmt) => {
                if break_stmt.label.is_some() {
                    unreachable!();
                }

                if let Some(meta) = self.scope.get_for_loop() {
                    ir.add(Code::Jump(Label::new(meta.continue_label)), location);
                } else {
                    return Err(CompileError::new(
                        "unable to break outside of a loop".to_string(),
                    ));
                }
            }
            Stmt::Raise(raise_stmt) => {
                self.compile_expr(ir, &raise_stmt.expr)?;
                ir.add(Code::Raise, location);
            }
            Stmt::Return(return_stmt) => {
                self.compile_expr(ir, &return_stmt.expr)?;
                ir.add(Code::Return, location);
            }
            Stmt::Unsafe(unsafe_stmt) => {
                ir.append(self.compile_stmt_list(ScopeContext::Unsafe, &unsafe_stmt.body)?);
            }
            // ignore top level statements
            Stmt::FnDecl(_)
            | Stmt::ExternFnDecl(_)
            | Stmt::ClassDecl(_)
            | Stmt::MixinDecl(_)
            | Stmt::InterfaceDecl(_)
            | Stmt::Module(_)
            | Stmt::Import(_) => {}
        }

        Ok(())
    }

    fn _compile_stmt_list(&mut self, tree: &[Stmt]) -> Result<IR> {
        let mut ir = IR::new();

        for stmt in tree.iter() {
            self.compile_stmt(stmt, &mut ir)?;
        }

        for optimizer in self.optimizers.iter() {
            optimizer(&self.module, &mut ir);
        }

        Ok(ir)
    }

    fn compile_stmt_list(&mut self, context: ScopeContext, tree: &[Stmt]) -> Result<IR> {
        self.enter_scope(context);

        let instructions = self._compile_stmt_list(tree)?;

        self.exit_scope();

        Ok(instructions)
    }

    fn has_no_side_effects(&self, code: &Code) -> bool {
        matches!(
            code,
            Code::Return
                | Code::ArithmeticAdd
                | Code::ArithmeticSub
                | Code::ArithmeticMul
                | Code::ArithmeticDiv
                | Code::ArithmeticExp
                | Code::ArithmeticBitAnd
                | Code::ArithmeticBitOr
                | Code::ArithmeticBitXor
                | Code::ArithmeticBitShiftLeft
                | Code::ArithmeticBitShiftRight
                | Code::ComparisonEq
                | Code::ComparisonNeq
                | Code::ComparisonGt
                | Code::ComparisonGte
                | Code::ComparisonLt
                | Code::ComparisonLte
                | Code::ConstString(_)
                | Code::ConstBool(_)
                | Code::ConstSymbol(_)
                | Code::ConstByte(_)
                | Code::ConstChar(_)
                | Code::ConstFloat(_)
                | Code::ConstInt128(_)
                | Code::ConstUint128(_)
                | Code::ConstInt64(_)
                | Code::ConstUint64(_)
                | Code::ConstInt32(_)
                | Code::ConstUint32(_)
                | Code::ConstInt16(_)
                | Code::ConstUint16(_)
                | Code::ConstInt8(_)
                | Code::ConstUint8(_)
                | Code::Discard
        )
    }

    fn compile_extern_fn(&mut self, extern_fn_decl: &ExternFnDeclStmt) -> Func {
        if extern_fn_decl.public {
            self.module.exports.insert(
                extern_fn_decl.name.clone(),
                Type::Fn(extern_fn_decl.name.clone()),
            );
        }

        Func {
            name: extern_fn_decl.name.clone(),
            body: IR::new(),
            is_void: false,
            is_extern: true,
            is_closure: false,
            // @TODO: this can be implemented when varargs are working
            args: vec![],
            location: self.get_location_by_offset(&extern_fn_decl.pos),
        }
    }

    fn compile_fn(&mut self, fn_decl: &FnDeclStmt, is_method: bool) -> Result<Func> {
        if fn_decl.public {
            self.module
                .exports
                .insert(fn_decl.name.clone(), Type::Fn(fn_decl.name.clone()));
        }

        self.enter_scope(ScopeContext::Function((fn_decl.name.clone(), is_method)));

        for arg in fn_decl.args.iter() {
            self.scope
                .set_local(arg.name.clone(), arg.mutable, Type::Unknown)?;
        }

        let mut body = self._compile_stmt_list(&fn_decl.body)?;

        self.exit_scope();

        // Let's check if we can optimize tail recursion call
        if self.optimize {
            loop {
                let index = || {
                    for (i, code) in body.iter().enumerate() {
                        if code == &Code::LoadTarget {
                            let next_code = body.get(i + 1);

                            if let Some(Code::Call(arg_count)) = next_code {
                                if !body
                                    .iter()
                                    .skip(i + 2)
                                    .any(|code| !self.has_no_side_effects(code))
                                {
                                    return Some((i, *arg_count));
                                }
                            }
                        }
                    }

                    None
                };

                match index() {
                    // No more tail recursions calls were found
                    None => break,

                    // Replace the two instructions with a tail-recursion-call one
                    Some((index, arg_count)) => {
                        body[index] = Code::TailCall(arg_count);
                        body.remove(index + 1);
                    }
                }
            }
        }

        Ok(Func {
            location: self.get_location_by_offset(&fn_decl.pos),
            name: fn_decl.name.clone(),
            is_void: !body.iter().any(|code| code == &Code::Return),
            is_extern: false,
            is_closure: false,
            body,
            args: fn_decl
                .args
                .iter()
                .map(|arg| FuncArg {
                    mutable: arg.mutable,
                    name: arg.name.clone(),
                })
                .collect::<Vec<_>>(),
        })
    }

    fn compile_class(&mut self, class_decl: ClassDeclStmt) -> Result<()> {
        if class_decl.public {
            self.module.exports.insert(
                class_decl.name.clone(),
                Type::Class(class_decl.name.clone()),
            );
        }

        // Register class early so that it can be referenced in one of it's methods
        self.module.classes.insert(
            class_decl.name.clone(),
            Class {
                name: class_decl.name.clone(),
                methods: HashMap::new(),
                fields: IndexMap::new(),
            },
        );

        self.enter_scope(ScopeContext::Class(class_decl.name.clone()));

        let mut fields = IndexMap::new();

        for field in class_decl.fields.iter() {
            fields.insert(
                field.name.clone(),
                Field {
                    mutable: field.mutable,
                    public: field.public,
                },
            );
        }

        let mut methods = HashMap::new();

        for extern_fn_decl in class_decl.extern_funcs.iter() {
            methods.insert(
                extern_fn_decl.name.clone(),
                self.compile_extern_fn(extern_fn_decl),
            );
        }

        for fn_decl in class_decl.funcs.iter() {
            methods.insert(fn_decl.name.clone(), self.compile_fn(fn_decl, true)?);
        }

        self.exit_scope();

        // Update the already-registered class with it's fields and methods
        if let Some(class) = self.module.classes.get_mut(&class_decl.name) {
            class.fields = fields;
            class.methods = methods;
        }

        Ok(())
    }

    fn compile_interface(&mut self, interface_decl: &InterfaceDeclStmt) -> Result<Interface> {
        if interface_decl.public {
            self.module.exports.insert(
                interface_decl.name.clone(),
                Type::Interface(interface_decl.name.clone()),
            );
        }

        Ok(Interface {
            name: interface_decl.name.clone(),
            functions: interface_decl
                .functions
                .iter()
                .map(|func| func.name.clone())
                .collect(),
        })
    }

    fn setup_prelude(&mut self) -> Result<()> {
        // Core module shouldn't include the prelude as that would create an infinite loop
        if self.module.name == "std.core"
            || self.module.name == "std.map"
            || self.module.name == "std.encoding.binary"
        {
            return Ok(());
        }

        for name in DEFAULT_IMPORTS {
            self.import_name(name)?;
        }

        Ok(())
    }

    fn parse_and_compile(&self, name: String) -> Result<Module> {
        let file = self
            .fs
            .read_file(&name)
            .map_err(|e| CompileError::new(format!("failed to read '{}': {}", name, e)))?;
        let tree = parser::parse(file.source())
            .map_err(|e| CompileError::new(format!("failed to parse module '{}': {}", name, e)))?;

        let filename = file
            .name()
            .and_then(|name| name.to_str().map(|filename| filename.to_string()));
        let line_numbers_offset = parse_line_numbers_offset(file.source());
        let mut module = self
            .fork(name, tree, line_numbers_offset)
            .compile()
            .map_err(|e| {
                if let Some(filename) = filename.clone() {
                    e.with_filename(filename)
                } else {
                    e
                }
            })?;

        module.filename = filename;

        Ok(module)
    }

    fn import_name(&mut self, name: &str) -> Result<()> {
        let mut components = name.split('.').collect::<Vec<_>>();

        if components.len() < 2 {
            return Err(CompileError::new(format!("invalid import path: {}", name,)));
        }

        let name = components.pop().unwrap();
        let module_name = components.join(".");
        let module_exist = { self.modules.read().unwrap().contains_key(&module_name) };

        if !module_exist {
            let module = self.parse_and_compile(module_name.to_string())?;

            {
                self.modules
                    .write()
                    .unwrap()
                    .insert(module_name.clone(), module);
            }
        }

        let guard = self.modules.read().unwrap();
        let module = guard.get(&module_name).unwrap();

        if let Some(global) = module.exports.get(name) {
            if self.module.imports.contains_key(name) {
                return Err(CompileError::new(format!(
                    "unable to redefine global: {}",
                    name
                )));
            }

            self.module.imports.insert(
                name.to_string(),
                Import::new(global.clone(), module.name.clone()),
            );

            return Ok(());
        } else if let Some(mixin) = module.mixins.get(name) {
            // As mixins are being used at compile time we need to import them instead of delegating that to the vm
            self.module.mixins.insert(mixin.name.clone(), mixin.clone());

            return Ok(());
        }

        Err(CompileError::new(format!(
            "failed to import unknown name '{}' from: {}",
            name, module.name
        )))
    }

    fn compile_top_level_stmt(&mut self, stmt: Stmt) -> Result<()> {
        match stmt {
            Stmt::FnDecl(fn_decl) => {
                let func = self.compile_fn(&fn_decl, false)?;

                self.module.funcs.push(func);
            }
            Stmt::ExternFnDecl(extern_fn_decl) => {
                let func = self.compile_extern_fn(&extern_fn_decl);

                self.module.funcs.push(func);
            }
            Stmt::ClassDecl(class_decl) => {
                self.compile_class(class_decl)?;
            }
            Stmt::InterfaceDecl(interface_decl) => {
                let interface = self.compile_interface(&interface_decl)?;

                self.module
                    .interfaces
                    .insert(interface_decl.name, interface);
            }
            _ => unreachable!(),
        }

        Ok(())
    }

    fn imports_pass(&mut self) -> Result<()> {
        loop {
            let index = self
                .tree
                .iter()
                .position(|stmt| matches!(stmt, Stmt::Import(_)));

            if let Some(index) = index {
                if let Stmt::Import(import_stmt) = self.tree.remove(index) {
                    self.pos = import_stmt.pos;
                    self.import_name(&import_stmt.name)?;

                    continue;
                }
            }

            break;
        }

        Ok(())
    }

    fn name_validation_pass(&self) -> Result<()> {
        let mut names: Vec<(&str, &str)> = vec![];

        for stmt in self.tree.iter() {
            match stmt {
                Stmt::FnDecl(fn_decl_stmt) => names.push(("function", &fn_decl_stmt.name)),
                Stmt::ExternFnDecl(extern_fn_decl) => {
                    names.push(("external function", &extern_fn_decl.name))
                }
                Stmt::Import(import_stmt) => names.push(("import", &import_stmt.name)),
                Stmt::ClassDecl(class_decl_stmt) => {
                    let mut class_names: Vec<(&str, &str)> = vec![];

                    for field in class_decl_stmt.fields.iter() {
                        class_names.push(("field", &field.name));
                    }

                    for func in class_decl_stmt.funcs.iter() {
                        class_names.push(("function", &func.name));
                    }

                    for external_func in class_decl_stmt.extern_funcs.iter() {
                        class_names.push(("external function", &external_func.name));
                    }

                    validate_unique(&class_names).map_err(|e| {
                        CompileError::new(format!(
                            "{} for class {}",
                            e.message, class_decl_stmt.name
                        ))
                    })?;

                    names.push(("class", &class_decl_stmt.name))
                }
                Stmt::InterfaceDecl(interface_decl_stmt) => {
                    names.push(("interface", &interface_decl_stmt.name))
                }
                _ => {}
            }
        }

        validate_unique(&names)
    }

    fn mixins_pass(&mut self) -> Result<()> {
        // Import all local mixins
        loop {
            let index = self
                .tree
                .iter()
                .position(|stmt| matches!(stmt, Stmt::MixinDecl(_)));

            if let Some(index) = index {
                if let Stmt::MixinDecl(mixin_decl_stmt) = self.tree.remove(index) {
                    self.module
                        .mixins
                        .insert(mixin_decl_stmt.name.clone(), mixin_decl_stmt);

                    continue;
                }
            }

            break;
        }

        // Expand AST for classes that extends mixins
        for stmt in self.tree.iter_mut() {
            if let Stmt::ClassDecl(class_decl_stmt) = stmt {
                for name in class_decl_stmt.extends.iter() {
                    let mixin = self
                        .module
                        .mixins
                        .get(name)
                        .ok_or_else(|| CompileError::new(format!("no such mixin: {}", name)))?;

                    class_decl_stmt.funcs.append(&mut mixin.funcs.clone());
                    class_decl_stmt
                        .extern_funcs
                        .append(&mut mixin.extern_funcs.clone());
                }
            }
        }

        Ok(())
    }

    fn module_name_pass(&mut self) -> Result<()> {
        loop {
            let index = self
                .tree
                .iter()
                .position(|stmt| matches!(stmt, Stmt::Module(_)));

            if let Some(index) = index {
                if index != 0 {
                    return Err(CompileError::new(
                        "module statement must be the first statement in a file and can only exist once".to_string(),
                    ));
                }

                if let Stmt::Module(module_stmt) = self.tree.remove(index) {
                    self.module.name = module_stmt.name;
                }

                continue;
            }

            break;
        }

        Ok(())
    }

    #[inline(always)]
    fn _compile(&mut self) -> Result<()> {
        self.module_name_pass()?;
        self.setup_prelude()?;
        self.mixins_pass()?;
        self.name_validation_pass()?;
        self.imports_pass()?;

        while !self.tree.is_empty() {
            let stmt = self.tree.remove(0);

            self.compile_top_level_stmt(stmt)?;
        }

        Ok(())
    }

    pub fn compile(mut self) -> Result<Module> {
        self._compile()
            .map_err(|e| e.with_location(self.get_location()))?;

        Ok(self.module)
    }

    pub fn compile_all(self) -> Result<Vec<Module>> {
        let modules = Rc::clone(&self.modules);
        let module = self.compile()?;
        let mut modules = Rc::try_unwrap(modules)
            .map_err(|_| {
                CompileError::new("unable to call compile_all more than once".to_string())
            })?
            .into_inner()
            .unwrap();

        // Sort modules based on their dependencies (@TODO: better error reporting)
        let mut output = vec![];
        let mut resolved = vec![];

        while !modules.is_empty() {
            let name = modules
                .iter()
                .find(|(_, module)| {
                    !module
                        .imports
                        .iter()
                        .any(|(_, import)| !resolved.contains(&import.origin))
                })
                .map(|(name, _)| name)
                .ok_or_else(|| {
                    CompileError::new(format!(
                        "circular dependency not allowed: {}",
                        modules
                            .iter()
                            .map(|(name, _)| name.clone())
                            .collect::<Vec<_>>()
                            .join(", ")
                    ))
                })?
                .clone();

            let module = modules.remove(&name).unwrap();

            resolved.push(name);
            output.push(module);
        }

        output.push(module);

        Ok(output)
    }
}
