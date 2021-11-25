use std::collections::HashMap;

use crate::compiler::ir::{Code, IR};
use crate::compiler::Module;

pub mod call_void;
pub mod pre_compute_labels;
pub mod remove_load_store_single_use;
pub mod remove_type_cast;
pub mod replace_load_with_const;
pub mod tail_call;

pub type Optimizer = fn(&Module, &mut IR);

pub fn parse_locals_usage(instructions: &IR) -> HashMap<usize, usize> {
    let mut usages = HashMap::new();

    for code in instructions.iter() {
        match code {
            Code::Store(id) | Code::StoreMut(id) => {
                usages
                    .entry(*id)
                    .and_modify(|count| *count += 1)
                    .or_insert(0);
            }
            Code::Load(id) => {
                usages.entry(*id).and_modify(|count| *count += 1);
            }
            // If we encounter a closure all previous locals are copied and should always stay alive
            Code::MakeClosure(_) => {
                for (_, count) in usages.iter_mut() {
                    *count += 2;
                }
            }
            _ => {}
        }
    }

    usages
}
