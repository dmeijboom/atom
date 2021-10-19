use atom_ir::{Code, IR};

use crate::compiler::{Module, Type};

/// Skip 'Iterable' validations for known core iterators
pub fn optimize(module: &Module, instructions: &mut IR) {
    if let Some((id, _, import)) = module.imports.get_full("Iterable") {
        if !(matches!(import.known_type, Type::Interface(_)) && import.origin == "std.core") {
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
