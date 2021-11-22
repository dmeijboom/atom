use crate::compiler::ir::{Code, IR};
use crate::compiler::Module;

/// If we know that we're about to discard the result on the stack we can simply avoid pushing it
pub fn optimize(_module: &Module, instructions: &mut IR) {
    loop {
        let index = instructions.iter().enumerate().position(|(i, code)| {
            matches!(code, Code::Call(_) | Code::CallKeywords(_))
                && instructions.get(i + 1) == Some(&Code::Discard)
        });

        if let Some(i) = index {
            let replacement = match &instructions[i] {
                Code::Call(arg_count) => Code::CallVoid(*arg_count),
                Code::CallKeywords((keywords, arg_count)) => {
                    Code::CallKeywordsVoid((keywords.clone(), *arg_count))
                }
                _ => unreachable!(),
            };

            instructions[i] = replacement;
            instructions.remove(i + 1);

            continue;
        }

        break;
    }
}
