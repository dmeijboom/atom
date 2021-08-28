use atom_ir::{Code, IR};

pub mod call_void;
pub mod load_local_twice_add;
pub mod pre_compute_labels;
//pub mod remove_core_validations;

pub type Optimizer = fn(&mut Vec<IR>);

pub type MatchFn = fn(&Code) -> bool;

#[derive(Debug)]
enum Position {
    Current,
    At(i64),
}

pub struct Match {
    when: MatchFn,
    pos: Position,
}

pub fn query(when: MatchFn) -> Query {
    Query {
        clauses: vec![Match {
            when,
            pos: Position::Current,
        }],
    }
}

pub struct Query {
    clauses: Vec<Match>,
}

impl Query {
    pub fn if_next(mut self, when: MatchFn) -> Self {
        self.clauses.push(Match {
            when,
            pos: Position::At(self.get_offset() + 1),
        });

        self
    }

    pub fn if_prev(mut self, when: MatchFn) -> Self {
        self.clauses.push(Match {
            when,
            pos: Position::At(self.get_offset() - 1),
        });

        self
    }

    pub fn get(&self, instructions: &[IR]) -> Option<usize> {
        let mut i = 0;

        'm: while i < instructions.len() {
            for m in self.clauses.iter() {
                let code = match m.pos {
                    Position::Current => &instructions[i].code,
                    Position::At(n) => {
                        if let Some(ir) = instructions.get(((i as i64) + n) as usize) {
                            &ir.code
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
