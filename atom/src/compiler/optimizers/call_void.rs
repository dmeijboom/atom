use atom_ir::{Code, IR};

use crate::compiler::optimizers::query;
use crate::compiler::Module;

/// If we know that we're about to discard the result on the stack we can simply avoid pushing it
pub fn optimize(_module: &Module, instructions: &mut IR) {
    loop {
        let query = query(|c| c == &Code::Discard)
            .if_prev(|c| matches!(c, Code::Call(_) | Code::CallKeywords(_)));

        if let Some(i) = query.get(instructions) {
            let replacement = match &instructions[i - 1] {
                Code::Call(arg_count) => Code::CallVoid(*arg_count),
                Code::CallKeywords((keywords, arg_count)) => {
                    Code::CallKeywordsVoid((keywords.to_vec(), *arg_count))
                }
                _ => unreachable!(),
            };

            instructions[i - 1] = replacement;
            instructions.remove(i);

            continue;
        }

        break;
    }
}
