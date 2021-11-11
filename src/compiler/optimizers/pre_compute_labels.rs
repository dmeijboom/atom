use std::collections::HashMap;

use crate::compiler::ir::{Code, IR};

use crate::compiler::Module;

/// Calculate the absolute offset for labels by using their name so that the vm can jump to it directly
pub fn optimize(_module: &Module, instructions: &mut IR) {
    let mut labels = HashMap::new();

    for (i, code) in instructions.iter().enumerate() {
        if let Code::SetLabel(name) = &code {
            labels.insert(name.clone(), i);
        }
    }

    for code in instructions.iter_mut() {
        match code {
            Code::Jump(label) => {
                if let Some(index) = labels.get(&label.name) {
                    label.index = Some(*index);
                }
            }
            Code::JumpIfTrue(label) => {
                if let Some(index) = labels.get(&label.name) {
                    label.index = Some(*index);
                }
            }
            Code::Branch((true_label, false_label)) => {
                if let Some(index) = labels.get(&true_label.name) {
                    true_label.index = Some(*index);
                }

                if let Some(index) = labels.get(&false_label.name) {
                    false_label.index = Some(*index);
                }
            }
            _ => {}
        }
    }
}
