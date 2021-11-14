use std::collections::HashMap;

use crate::compiler::ir::{Code, IR};
use crate::compiler::Module;

fn is_const(code: &Code) -> bool {
    matches!(
        code,
        Code::ConstUint8(_)
            | Code::ConstUint16(_)
            | Code::ConstUint32(_)
            | Code::ConstUint64(_)
            | Code::ConstUint128(_)
            | Code::ConstInt8(_)
            | Code::ConstInt16(_)
            | Code::ConstInt32(_)
            | Code::ConstInt64(_)
            | Code::ConstInt128(_)
            | Code::ConstBool(_)
            | Code::ConstByte(_)
            | Code::ConstChar(_)
            | Code::ConstFloat(_)
            | Code::ConstString(_)
            | Code::ConstSymbol(_)
    )
}

/// Replace load/store instructions with constants when used only once
pub fn optimize(_module: &Module, instructions: &mut IR) {
    // In case of a closure we can no longer inspect the usages of load/store instructions as
    // the will propagate to the closure which has it's own instruction set
    if instructions
        .iter()
        .any(|code| matches!(code, Code::MakeClosure(_)))
    {
        return;
    }

    // Detect usages for locals
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
            _ => {}
        }
    }

    // Replace load/store instructions with constants or delete store instructions entirely
    loop {
        let index = instructions.iter().enumerate().position(|(i, code)| {
            matches!(code, Code::Store(id) | Code::StoreMut(id) if *usages.get(id).unwrap_or(&2) <= 1)
                && is_const(&instructions[i - 1])
        });

        if let Some(index) = index {
            let local_id = match instructions.remove(index) {
                Code::Store(id) | Code::StoreMut(id) => id,
                _ => unreachable!(),
            };
            let const_instr = instructions.remove(index - 1);

            // Replace with the constant instruction or just remove it if we can't find a load instruction
            if let Some(load_index) = instructions
                .iter()
                .position(|code| matches!(code, Code::Load(id) if *id == local_id))
            {
                instructions[load_index] = const_instr;
            }

            continue;
        }

        break;
    }
}
