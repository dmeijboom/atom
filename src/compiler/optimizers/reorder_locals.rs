use crate::compiler::ir::{Code, IR};
use crate::compiler::Module;

/// Reorder locals so that IDs are in ascending order and calculate max locals size
pub fn optimize(_module: &Module, instructions: &mut IR) {
    let mut max_id = None;

    for code in instructions.iter() {
        match code {
            Code::Load(id) | Code::Store(id) | Code::StoreMut(id) => {
                if max_id.is_none() || Some(*id) > max_id {
                    max_id = Some(*id);
                }
            }
            _ => {}
        }
    }

    match max_id {
        None => instructions.set_locals_size(0),
        Some(max_id) => instructions.set_locals_size(max_id + 1),
    }
}
