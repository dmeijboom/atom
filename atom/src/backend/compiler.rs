use std::{
    borrow::Cow,
    collections::HashMap,
    hash::{Hash, Hasher},
};

use bytes::{Bytes, BytesMut};
use lazy_static::lazy_static;

use crate::{
    collections::{IntMap, OrderedSet},
    frontend::{
        ast::{self, BinaryOp, Path, UnaryOp},
        Block, IRClass, IRFn, IRNode, IRValue, Loop, NodeKind, Program, Span, Spanned,
        VariableKind,
    },
    runtime::{Context, Fn},
};

use super::{Bytecode, Const, Op, Serializable};

lazy_static! {
    static ref BINARY_OPS: HashMap<ast::BinaryOp, Op> = {
        [
            (ast::BinaryOp::Add, Op::Add),
            (ast::BinaryOp::Sub, Op::Sub),
            (ast::BinaryOp::Mul, Op::Mul),
            (ast::BinaryOp::Rem, Op::Rem),
            (ast::BinaryOp::Div, Op::Div),
            (ast::BinaryOp::Eq, Op::Eq),
            (ast::BinaryOp::Ne, Op::Ne),
            (ast::BinaryOp::Gt, Op::Gt),
            (ast::BinaryOp::Gte, Op::Gte),
            (ast::BinaryOp::Lt, Op::Lt),
            (ast::BinaryOp::Lte, Op::Lte),
            (ast::BinaryOp::TypeAssert, Op::TypeAssert),
            (ast::BinaryOp::BitOr, Op::BitwiseOr),
            (ast::BinaryOp::BitAnd, Op::BitwiseAnd),
            (ast::BinaryOp::ShiftLeft, Op::ShiftLeft),
            (ast::BinaryOp::ShiftRight, Op::ShiftRight),
            (ast::BinaryOp::Xor, Op::BitwiseXor),
        ]
        .iter()
        .cloned()
        .collect()
    };
}

pub struct GlobalContext {
    pub atoms: OrderedSet<String>,
}

impl Default for GlobalContext {
    fn default() -> Self {
        Self {
            atoms: OrderedSet::new([
                "false".to_string(),
                "true".to_string(),
                "nil".to_string(),
                "module".to_string(),
            ]),
        }
    }
}

#[derive(Debug, Default)]
pub struct Package {
    pub body: Bytes,
    pub imports: Vec<Path>,
    pub consts: Vec<Const>,
    pub classes: Vec<Class>,
    pub functions: Vec<Fn>,
    pub offsets: IntMap<usize, Span>,
}

// #[derive(Debug, thiserror::Error)]
// pub enum ErrorKind {
//     #[error("failed to write bytecode(s): {0}")]
//     FailedWriteBytecode(#[from] io::Error),
// }

// pub type CompileError = SpannedError<ErrorKind>;

#[derive(Debug, Default)]
pub struct Class {
    pub name: Cow<'static, str>,
    pub public: bool,
    pub methods: HashMap<Cow<'static, str>, Fn>,
}

impl Hash for Class {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

enum Marker {
    Begin(usize),
    End(usize),
}

pub struct Compiler<'a> {
    functions: Vec<Fn>,
    classes: Vec<Class>,
    block: Vec<BytesMut>,
    imports: Vec<Path>,
    consts: OrderedSet<Const>,
    offsets: IntMap<usize, Span>,
    loops: IntMap<usize, Vec<Marker>>,
    ctx: &'a mut GlobalContext,
}

impl<'a> Compiler<'a> {
    pub fn new(ctx: &'a mut GlobalContext) -> Self {
        Self {
            ctx,
            block: vec![],
            classes: vec![],
            imports: vec![],
            functions: vec![],
            loops: IntMap::default(),
            offsets: IntMap::default(),
            consts: OrderedSet::default(),
        }
    }

    fn body(&mut self) -> &mut BytesMut {
        self.block.last_mut().expect("missing block")
    }

    fn offset(&self) -> usize {
        self.block.last().expect("missing block").len()
    }

    fn set_offset(&mut self, offset: usize, new_offset: usize) {
        let orig = Bytecode::deserialize(&mut &self.body()[offset..offset + super::BYTECODE_SIZE]);
        Bytecode::with_code(orig.op, (new_offset / super::BYTECODE_SIZE) as u32)
            .serialize(&mut &mut self.body()[offset..]);
    }

    fn exit_loop(&mut self, loop_id: usize, begin: usize, end: usize) {
        let mut markers = self.loops.remove(&loop_id).unwrap_or_default();

        while let Some(marker) = markers.pop() {
            match marker {
                Marker::Begin(idx) => self.set_offset(idx, begin),
                Marker::End(idx) => self.set_offset(idx, end),
            }
        }
    }

    fn accept(&mut self, op: Op) -> Option<Spanned<Bytecode>> {
        if self.body().is_empty() {
            return None;
        }

        let offset = self.body().len() - super::BYTECODE_SIZE;
        let span = self.offsets.get(&offset).copied().unwrap_or_default();
        let bc = Bytecode::deserialize(&mut &self.body()[offset..]);

        if bc.op == op {
            self.body().truncate(offset);
            return Some(bc.at(span));
        }

        None
    }

    fn push(&mut self, bytecode: Spanned<Bytecode>) -> usize {
        let offset = self.body().len();
        bytecode.inner.serialize(&mut self.body());
        self.offsets.insert(offset, bytecode.span);
        offset
    }

    fn optim_tail_call(&mut self) {
        let discard = self.accept(Op::Discard);

        match self.accept(Op::CallFn) {
            Some(bc) => {
                let (idx, argc) = bc.code2();
                self.push(Bytecode::with_code(Op::LoadFn, idx as u32).at(bc.span));
                self.push(Bytecode::with_code(Op::TailCall, argc as u32).at(bc.span));
            }
            None => {
                if let Some(bc) = discard {
                    self.push(bc);
                }
            }
        }
    }

    fn enter_block(&mut self) {
        self.block.push(BytesMut::default());
    }

    fn exit_block(&mut self) -> BytesMut {
        self.optim_tail_call();
        self.block.pop().unwrap_or_default()
    }

    fn call(&mut self, span: Span, callee: IRNode, args: Vec<IRNode>) {
        let arg_count = args.len();

        for arg in args {
            self.node(arg);
        }

        if let NodeKind::BuiltinRef(id) = callee.kind {
            self.push(Bytecode::with_code2(Op::CallBuiltin, id as u16, arg_count as u16).at(span));
            return;
        }

        self.node(callee);

        match self.accept(Op::LoadFn) {
            Some(bc) => self.push(
                Bytecode::with_code2(Op::CallFn, bc.code as u16, arg_count as u16).at(bc.span),
            ),
            None => self.push(Bytecode::with_code(Op::Call, arg_count as u32).at(span)),
        };
    }

    fn constant(&mut self, span: Span, value: IRValue) {
        let constant = match value {
            IRValue::Int(i) if i > 0 && u32::try_from(i).is_ok() => {
                self.push(Bytecode::with_code(Op::LoadConstInt, i as u32).at(span));
                return;
            }
            IRValue::Int(i) => Const::Int(i),
            IRValue::BigInt(i) => Const::BigInt(i),
            IRValue::Float(f) => Const::Float(f),
            IRValue::String(s) => Const::Str(s),
            IRValue::Atom(id) => {
                self.push(Bytecode::with_code(Op::LoadAtom, id).at(span));
                return;
            }
        };

        let id = self.consts.insert(constant);
        self.push(Bytecode::with_code(Op::LoadConst, id).at(span));
    }

    fn member(&mut self, span: Span, object: IRNode, name: String) {
        self.node(object);

        let code = self.ctx.atoms.insert(name);
        self.push(Bytecode::with_code(Op::LoadMember, code).at(span));
    }

    fn unary(&mut self, span: Span, op: UnaryOp, node: IRNode) {
        self.node(node);

        match op {
            UnaryOp::Not => self.push(Bytecode::new(Op::UnaryNot).at(span)),
        };
    }

    fn load_variable(&mut self, span: Span, kind: VariableKind, id: usize) {
        self.push(
            Bytecode::with_code(
                match kind {
                    VariableKind::Local => Op::LoadLocal,
                    VariableKind::Global => Op::Load,
                },
                id as u32,
            )
            .at(span),
        );
    }

    fn store_variable(&mut self, span: Span, kind: VariableKind, id: usize, value: IRNode) {
        self.node(value);
        self.push(
            Bytecode::with_code(
                match kind {
                    VariableKind::Local => Op::StoreLocal,
                    VariableKind::Global => Op::Store,
                },
                id as u32,
            )
            .at(span),
        );
    }

    fn for_loop(&mut self, span: Span, for_loop: Loop) {
        let begin = self.offset();

        if let Some(node) = for_loop.pre {
            self.node(node);
        }

        self.node(for_loop.cond);
        let offset = self.push(Bytecode::new(Op::JumpIfFalse).at(span));
        self.block(for_loop.body);
        let end = self.offset();

        if let Some(node) = for_loop.post {
            self.node(node);
        }

        self.push(Bytecode::with_code(Op::Jump, (begin / super::BYTECODE_SIZE) as u32).at(span));
        self.set_offset(offset, self.offset());
        self.exit_loop(for_loop.id, end, self.offset());
    }

    fn logical(&mut self, span: Span, rhs: IRNode, cond: bool) {
        let offset = self.push(match cond {
            true => Bytecode::new(Op::PushJumpIfTrue).at(span),
            false => Bytecode::new(Op::PushJumpIfFalse).at(span),
        });

        self.node(rhs);
        self.set_offset(offset, self.offset());
    }

    fn binary(&mut self, span: Span, lhs: IRNode, op: BinaryOp, rhs: IRNode) {
        self.node(lhs);

        match BINARY_OPS.get(&op).copied() {
            Some(op) => {
                self.node(rhs);
                self.push(Bytecode::new(op).at(span));
            }
            None => match op {
                ast::BinaryOp::Or => self.logical(span, rhs, true),
                ast::BinaryOp::And => self.logical(span, rhs, false),
                _ => unreachable!(),
            },
        };
    }

    fn array(&mut self, span: Span, items: Vec<IRNode>) {
        let item_count = items.len();

        for item in items {
            self.node(item);
        }

        self.push(Bytecode::with_code(Op::MakeArray, item_count as u32).at(span));
    }

    fn slice(&mut self, span: Span, begin: Option<IRNode>, end: Option<IRNode>) {
        let mut code = 0;

        if let Some(begin) = begin {
            code += 1;
            self.node(begin);
        }

        if let Some(end) = end {
            code += 2;
            self.node(end);
        }

        self.push(Bytecode::with_code(Op::MakeSlice, code).at(span));
    }

    fn comp_member(&mut self, span: Span, object: IRNode, index: IRNode) {
        self.node(object);

        match index.kind {
            NodeKind::Range(begin, end) => self.slice(span, begin.map(|n| *n), end.map(|n| *n)),
            _ => {
                self.node(index);
                self.push(Bytecode::new(Op::LoadElement).at(span));
            }
        }
    }

    fn ret(&mut self, span: Span, node: IRNode) {
        self.node(node);

        match self.accept(Op::LoadLocal) {
            Some(bc) => self.push(Bytecode::with_code(Op::ReturnLocal, bc.code).at(bc.span)),
            _ => self.push(Bytecode::new(Op::Return).at(span)),
        };
    }

    fn condition(&mut self, span: Span, cases: Vec<(IRNode, Block)>, default: Option<Block>) {
        let mut offsets = vec![];

        for (cond, block) in cases {
            self.node(cond);
            let transition = self.push(Bytecode::new(Op::JumpIfFalse).at(span));
            self.block(block);
            offsets.push(self.push(Bytecode::new(Op::Jump).at(span)));
            self.set_offset(transition, self.offset());
        }

        if let Some(block) = default {
            self.block(block);
        }

        for offset in offsets {
            self.set_offset(offset, self.offset());
        }
    }

    fn store(&mut self, span: Span, lhs: IRNode, rhs: IRNode) {
        match lhs.kind {
            NodeKind::Member(object, name) => {
                self.node(*object);
                self.node(rhs);
                let idx = self.ctx.atoms.insert(name);
                self.push(Bytecode::with_code(Op::StoreMember, idx).at(span));
            }
            NodeKind::CompMember(object, index) => {
                self.node(*object);
                self.node(*index);
                self.node(rhs);
                self.push(Bytecode::new(Op::StoreElement).at(span));
            }
            _ => unreachable!(),
        };
    }

    fn node(&mut self, root: IRNode) {
        match root.kind {
            NodeKind::FnRef(id) => {
                self.push(Bytecode::with_code(Op::LoadFn, id).at(root.span));
            }
            NodeKind::ClassRef(id) => {
                self.push(Bytecode::with_code(Op::LoadClass, id).at(root.span));
            }
            NodeKind::ImportRef(id) => {
                self.push(Bytecode::with_code(Op::LoadImport, id).at(root.span));
            }
            NodeKind::Break(loop_id) => {
                let offset = self.push(Bytecode::new(Op::Jump).at(root.span));

                if let Some(markers) = self.loops.get_mut(&loop_id) {
                    markers.push(Marker::End(offset));
                }
            }
            NodeKind::Continue(loop_id) => {
                let offset = self.push(Bytecode::new(Op::Jump).at(root.span));

                if let Some(markers) = self.loops.get_mut(&loop_id) {
                    markers.push(Marker::Begin(offset));
                }
            }
            NodeKind::Const(value) => self.constant(root.span, value),
            NodeKind::Loop(for_loop) => self.for_loop(root.span, *for_loop),
            NodeKind::Array(items) => self.array(root.span, items),
            NodeKind::Return(node) => self.ret(root.span, *node),
            NodeKind::Yield(node) => {
                self.node(*node);
                self.push(Bytecode::new(Op::Yield).at(root.span));
            }
            NodeKind::Discard(node) => {
                self.node(*node);
                self.push(Bytecode::new(Op::Discard).at(root.span));
            }
            NodeKind::Compound(block, node) => {
                self.block(block);
                self.node(*node);
            }
            NodeKind::Unary(op, node) => self.unary(root.span, op, *node),
            NodeKind::Member(object, name) => self.member(root.span, *object, name),
            NodeKind::Call(node, args) => self.call(root.span, *node, args),
            NodeKind::Store(lhs, rhs) => self.store(root.span, *lhs, *rhs),
            NodeKind::Condition(cases, default) => self.condition(root.span, cases, default),
            NodeKind::CompMember(object, index) => self.comp_member(root.span, *object, *index),
            NodeKind::Binary(lhs, op, rhs) => self.binary(root.span, *lhs, op, *rhs),
            NodeKind::LoadVariable(kind, id) => self.load_variable(root.span, kind, id),
            NodeKind::StoreVariable(kind, id, value) => {
                self.store_variable(root.span, kind, id, *value)
            }
            NodeKind::Noop | NodeKind::BuiltinRef(_) | NodeKind::Range(_, _) => unreachable!(),
        };
    }

    fn block(&mut self, body: Block) {
        for node in body.children {
            self.node(node);
        }
    }

    fn compile_block(&mut self, body: Block) -> BytesMut {
        self.enter_block();
        self.block(body);
        self.exit_block()
    }

    fn func(&mut self, func: IRFn) {
        let body = self.compile_block(func.body);

        self.functions.push(Fn {
            name: Cow::Owned(func.name),
            body: body.freeze(),
            public: func.public,
            resumable: func.resumable,
            arg_count: func.arg_count as u32,
            context: Context::default(),
        });
    }

    fn class(&mut self, class: IRClass) {
        let mut output = Class {
            name: Cow::Owned(class.name),
            public: class.public,
            methods: HashMap::default(),
        };

        for (name, func) in class.methods {
            let body = self.compile_block(func.body);

            output.methods.insert(
                Cow::Owned(name),
                Fn {
                    name: Cow::Owned(func.name),
                    body: body.freeze(),
                    public: func.public,
                    resumable: false,
                    arg_count: func.arg_count as u32,
                    context: Context::default(),
                },
            );
        }

        self.classes.push(output);
    }

    pub fn compile(mut self, program: Program) -> Package {
        self.imports = program.imports;

        for func in program.funcs {
            self.func(func);
        }

        for class in program.classes {
            self.class(class);
        }

        let body = self.compile_block(program.body);

        Package {
            body: body.freeze(),
            consts: self.consts.into_vec(),
            imports: self.imports,
            classes: self.classes,
            functions: self.functions,
            offsets: self.offsets,
        }
    }
}
