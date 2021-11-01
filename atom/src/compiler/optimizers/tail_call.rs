use atom_ir::{Code, IR};

use crate::compiler::Module;

fn has_side_effects(codes: &[Code]) -> bool {
    for code in codes {
        if !matches!(
            code,
            Code::Return
                | Code::ArithmeticAdd
                | Code::ArithmeticSub
                | Code::ArithmeticMul
                | Code::ArithmeticDiv
                | Code::ArithmeticExp
                | Code::ArithmeticBitAnd
                | Code::ArithmeticBitOr
                | Code::ArithmeticBitXor
                | Code::ArithmeticBitShiftLeft
                | Code::ArithmeticBitShiftRight
                | Code::ComparisonEq
                | Code::ComparisonNeq
                | Code::ComparisonGt
                | Code::ComparisonGte
                | Code::ComparisonLt
                | Code::ComparisonLte
                | Code::ConstString(_)
                | Code::ConstBool(_)
                | Code::ConstSymbol(_)
                | Code::ConstByte(_)
                | Code::ConstChar(_)
                | Code::ConstFloat(_)
                | Code::ConstInt128(_)
                | Code::ConstUint128(_)
                | Code::ConstInt64(_)
                | Code::ConstUint64(_)
                | Code::ConstInt32(_)
                | Code::ConstUint32(_)
                | Code::ConstInt16(_)
                | Code::ConstUint16(_)
                | Code::ConstInt8(_)
                | Code::ConstUint8(_)
                | Code::Discard
        ) {
            return true;
        }
    }

    false
}

/// Basic tail call optimization
pub fn optimize(_module: &Module, instructions: &mut IR) {
    loop {
        let index = instructions.iter().enumerate().position(|(i, code)| {
            matches!(code, Code::LoadTarget)
                && matches!(instructions.get(i + 1), Some(Code::Call(_)))
                && !has_side_effects(instructions.slice(i + 2))
        });

        if let Some(i) = index {
            instructions.remove(i);
            instructions[i] = match &instructions[i] {
                Code::Call(arg_count) => Code::TailCall(*arg_count),
                _ => unreachable!(),
            };

            continue;
        }

        break;
    }
}
