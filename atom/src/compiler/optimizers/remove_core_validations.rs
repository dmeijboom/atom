use atom_ir::{Code, IR};

use crate::compiler::optimizers::query;

/// Skip 'Iterable' validations for known core iterators
pub fn optimize(instructions: &mut Vec<IR>) {
    loop {
        let query = query(|c| matches!(c, Code::MakeRange | Code::MakeArray(_)))
            .if_next(|c| c == &Code::LoadName("Iterable".to_string()))
            .if_next(|c| c == &Code::Validate);

        if let Some(i) = query.get(instructions) {
            instructions.remove(i + 1);
            instructions.remove(i + 1);

            continue;
        }

        break;
    }
}
