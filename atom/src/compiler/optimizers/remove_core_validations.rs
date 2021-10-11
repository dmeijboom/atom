use atom_ir::{Code, IR};

use crate::compiler::optimizers::query;
use crate::compiler::Module;

/// Skip 'Iterable' validations for known core iterators
pub fn optimize(module: &Module, instructions: &mut IR) {
    if let Some((id, _, global)) = module.globals.get_full("Iterable") {
        if global.module_name != "std.core" {
            return;
        }

        loop {
            let query = query(|c| matches!(c, Code::MakeRange | Code::MakeArray(_)))
                .if_next(move |c| c == &Code::LoadGlobal(id))
                .if_next(|c| c == &Code::Validate);

            if let Some(i) = query.get(instructions) {
                instructions.remove(i + 1);
                instructions.remove(i + 1);

                continue;
            }

            break;
        }
    }
}
