use crate::compiler::ir::{Code, IR};
use crate::compiler::optimizers::parse_locals_usage;
use crate::compiler::Module;

/// Replace load/store instructions with constants when used only once
pub fn optimize(_module: &Module, instructions: &mut IR) {
    // Remove load/store instructions when a local is only loaded once after a store instruction
    loop {
        let locals_usage = parse_locals_usage(instructions);
        let index = instructions.iter().enumerate().position(|(i, code)| {
            let id = match code {
                Code::Store(id) | Code::StoreMut(id) => *id,
                _ => return false,
            };

            if *locals_usage.get(&id).unwrap_or(&2) > 1 {
                return false;
            }

            matches!(instructions.get(i + 1), Some(Code::Load(load_id)) if *load_id == id)
        });

        if let Some(index) = index {
            instructions.remove(index);
            instructions.remove(index);

            continue;
        }

        break;
    }
}
