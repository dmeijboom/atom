use atom_ir::{Code, IR};

use crate::compiler::Module;

/// Skip 'Iterable' validations for known core iterators
pub fn optimize(module: &Module, instructions: &mut IR) {
    if let Some((id, _, global)) = module.imports.get_full("Iterable") {
        if global.module_name != "std.core" {
            return;
        }

        loop {
            let index = instructions.iter().enumerate().position(|(i, code)| {
                matches!(code, Code::MakeRange | Code::MakeArray(_))
                    && instructions.get(i + 1) == Some(&Code::LoadGlobal(id))
                    && instructions.get(i + 2) == Some(&Code::Validate)
            });

            if let Some(i) = index {
                instructions.remove(i + 1);
                instructions.remove(i + 1);

                continue;
            }

            break;
        }
    }
}
