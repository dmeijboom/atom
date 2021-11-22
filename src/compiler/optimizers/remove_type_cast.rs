use crate::compiler::ir::{Code, IR};
use crate::compiler::Module;

fn is_valid_type(name: &str) -> bool {
    matches!(
        name,
        "Uint8"
            | "Int8"
            | "Uint16"
            | "Int16"
            | "Uint32"
            | "Int32"
            | "Uint64"
            | "Int64"
            | "Uint128"
            | "Int128"
    )
}

pub fn optimize(_module: &Module, instructions: &mut IR) {
    loop {
        let index = instructions.iter().enumerate().position(|(i, code)| {
            matches!(code, Code::ConstInt32(_))
                && matches!(instructions.get(i + 1), Some(Code::Cast(id)) if is_valid_type(instructions.get_data(*id)))
        });

        if let Some(i) = index {
            let type_name = if let Code::Cast(type_name) = instructions.remove(i + 1) {
                instructions.get_data(type_name).clone()
            } else {
                unreachable!()
            };

            let code = &mut instructions[i];
            let value = if let Code::ConstInt32(value) = code {
                *value
            } else {
                unreachable!()
            };

            *code = match type_name.as_str() {
                "Uint8" => Code::ConstUint8(value as u8),
                "Int8" => Code::ConstInt8(value as i8),
                "Uint16" => Code::ConstUint16(value as u16),
                "Int16" => Code::ConstInt16(value as i16),
                "Uint32" => Code::ConstUint32(value as u32),
                "Int32" => Code::ConstInt32(value as i32),
                "Uint64" => Code::ConstUint64(value as u64),
                "Int64" => Code::ConstInt64(value as i64),
                "Uint128" => Code::ConstUint128(value as u128),
                "Int128" => Code::ConstInt128(value as i128),
                _ => unreachable!(),
            };

            continue;
        }

        break;
    }
}
