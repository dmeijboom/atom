use crate::frontend::syntax::Span;
use std::slice::Iter;

#[derive(Debug)]
pub enum Type {
    //Array,
    Float32,
    Float64,
    //Fn,
    Int1,
    Int8,
    Int16,
    Int32,
    Int64,
    //Ptr,
    //Struct,
    //Vec,
    Void,
}

#[derive(Debug)]
pub enum Terminator {
    Return,
}

#[derive(Debug)]
pub struct Instr {
    pub span: Span,
    pub kind: InstrKind,
}

impl Instr {
    pub fn new(span: Span, kind: InstrKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug)]
pub enum InstrKind {
    And,
    Or,
    IntAdd,
    IntSub,
    IntMul,
    IntSDiv,
    IntUDiv,
    IntShl,
    IntSShr,
    IntUShr,
    IntSLte,
    IntSLt,
    IntULte,
    IntULt,
    IntSGte,
    IntSGt,
    IntUGte,
    IntUGt,
    IntEq,
    IntNeq,
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatLte,
    FloatLt,
    FloatGte,
    FloatGt,
    FloatEq,
    FloatNeq,
    Load(usize),
    Store(usize),
    ConstInt(i64),
    ConstUint(u64),
    ConstBool(bool),
    ConstFloat(f64),
    Branch(Block, Block),
}

#[derive(Debug, Default)]
pub struct Block {
    pub body: Vec<Instr>,
    pub term: Option<Terminator>,
}

impl Block {
    pub fn is_terminated(&self) -> bool {
        if self.term.is_some() {
            return true;
        }

        let mut terminated = false;

        for stmt in self.iter() {
            if let InstrKind::Branch(then, alt) = &stmt.kind {
                terminated = then.is_terminated() && alt.is_terminated();

                if !terminated {
                    break;
                }
            }
        }

        terminated
    }
}

impl Block {
    #[inline]
    pub fn iter(&self) -> Iter<'_, Instr> {
        self.body.iter()
    }
}

#[derive(Debug)]
pub struct Fn {
    pub name: String,
    pub body: Block,
    pub return_type: Type,
    pub locals: Vec<Type>,
}

#[derive(Default, Debug)]
pub struct Module {
    pub name: String,
    pub funcs: Vec<Fn>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn block_terminated_when_term_is_set() {
        let block = Block {
            term: Some(Terminator::Return),
            ..Block::default()
        };

        assert!(block.is_terminated());
    }

    #[test]
    fn block_not_terminated_when_term_is_not_set() {
        let block = Block::default();
        assert!(!block.is_terminated());
    }

    #[test]
    fn block_not_terminated_when_one_side_of_branch_is_not_terminated() {
        let block = Block {
            body: vec![Instr::new(
                Span::default(),
                InstrKind::Branch(
                    Block {
                        term: Some(Terminator::Return),
                        ..Block::default()
                    },
                    Block::default(),
                ),
            )],
            ..Block::default()
        };

        assert!(!block.is_terminated());
    }

    #[test]
    fn block_terminated_when_both_sides_of_branch_are_terminated() {
        let block = Block {
            body: vec![Instr::new(
                Span::default(),
                InstrKind::Branch(
                    Block {
                        term: Some(Terminator::Return),
                        ..Block::default()
                    },
                    Block {
                        term: Some(Terminator::Return),
                        ..Block::default()
                    },
                ),
            )],
            ..Block::default()
        };

        assert!(block.is_terminated());
    }

    #[test]
    fn block_terminated_when_both_sides_of_branch_are_recursively_terminated() {
        let block = Block {
            body: vec![Instr::new(
                Span::default(),
                InstrKind::Branch(
                    Block {
                        term: Some(Terminator::Return),
                        ..Block::default()
                    },
                    Block {
                        body: vec![Instr::new(
                            Span::default(),
                            InstrKind::Branch(
                                Block {
                                    term: Some(Terminator::Return),
                                    ..Block::default()
                                },
                                Block {
                                    term: Some(Terminator::Return),
                                    ..Block::default()
                                },
                            ),
                        )],
                        ..Block::default()
                    },
                ),
            )],
            ..Block::default()
        };

        assert!(block.is_terminated());
    }
}
