use crate::compiler::ir::{Code, IR};
use crate::compiler::Module;

fn is_valid_type(name: &str) -> bool {
    matches!(name, "Int" | "Uint")
}

pub fn optimize(_module: &Module, instructions: &mut IR) {
    loop {
        let index = instructions.iter().enumerate().position(|(i, code)| {
            matches!(code, Code::ConstInt(_))
                && matches!(instructions.get(i + 1), Some(Code::Cast(id)) if is_valid_type(instructions.get_data(*id)))
        });

        if let Some(i) = index {
            let type_name = if let Code::Cast(type_name) = instructions.remove(i + 1) {
                instructions.get_data(type_name).clone()
            } else {
                unreachable!()
            };

            let code = &mut instructions[i];
            let value = if let Code::ConstInt(value) = code {
                *value
            } else {
                unreachable!()
            };

            *code = match type_name.as_str() {
                "Int" => Code::ConstInt(value),
                "Uint" => Code::ConstUint(value as u64),
                _ => unreachable!(),
            };

            continue;
        }

        break;
    }
}
