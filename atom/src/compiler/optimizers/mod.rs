use atom_ir::{Code, IR};

use crate::compiler::Module;

pub mod call_void;
pub mod load_local_twice_add;
pub mod pre_compute_labels;
pub mod remove_core_validations;

pub type Optimizer = fn(&Module, &mut IR);

pub type MatchFn = Box<dyn Fn(&Code) -> bool>;

#[derive(Debug)]
enum Position {
    Current,
    At(i64),
}

pub struct Match {
    when: MatchFn,
    pos: Position,
}

pub fn query(when: impl Fn(&Code) -> bool + 'static) -> Query {
    Query {
        clauses: vec![Match {
            when: Box::new(when),
            pos: Position::Current,
        }],
    }
}

pub struct Query {
    clauses: Vec<Match>,
}

impl Query {
    pub fn if_next(mut self, when: impl Fn(&Code) -> bool + 'static) -> Self {
        self.clauses.push(Match {
            when: Box::new(when),
            pos: Position::At(self.get_offset() + 1),
        });

        self
    }

    pub fn if_prev(mut self, when: impl Fn(&Code) -> bool + 'static) -> Self {
        self.clauses.push(Match {
            when: Box::new(when),
            pos: Position::At(self.get_offset() - 1),
        });

        self
    }

    pub fn get(&self, instructions: &IR) -> Option<usize> {
        let mut i = 0;

        'm: while i < instructions.len() {
            for m in self.clauses.iter() {
                let code = match m.pos {
                    Position::Current => &instructions[i],
                    Position::At(n) => {
                        if let Some(code) = instructions.get(((i as i64) + n) as usize) {
                            code
                        } else {
                            continue;
                        }
                    }
                };

                if !(m.when)(code) {
                    i += 1;

                    continue 'm;
                }
            }

            return Some(i);
        }

        None
    }

    fn get_offset(&self) -> i64 {
        let mut offset: i64 = 0;

        for m in self.clauses.iter() {
            offset += match m.pos {
                Position::Current => 0,
                Position::At(n) => n,
            };
        }

        offset
    }
}
