use atom_ir::{Code, IR};

use crate::compiler::Module;

fn get_local_id(code: &Code) -> Option<usize> {
    if let Code::Load(id) = code {
        return Some(*id);
    }

    None
}

/// Instead of adding a local to the same local, multiply it by 2 so that there is no need for a second lookup
pub fn optimize(_module: &Module, instructions: &mut IR) {
    let mut i = 0;
    let mut last_local_id = None;

    while i < instructions.len() {
        let local_id = instructions.get(i).and_then(|code| get_local_id(code));

        if let Some(id) = local_id {
            if let Some(last_id) = last_local_id {
                if last_id == id {
                    if let Some(Code::ArithmeticAdd) = instructions.get(i + 1) {
                        instructions[i] = Code::ConstInt(2);
                        instructions[i + 1] = Code::ArithmeticMul;

                        i += 2;
                        continue;
                    }
                }
            }

            last_local_id = Some(id);
        }

        i += 1;
    }
}
