use std::collections::HashMap;

use atom_ir::{Code, IR};

/// Calculate the absolute offset for labels by using their name so that the vm can jump to it directly
pub fn optimize(instructions: &mut Vec<IR>) {
    let mut labels = HashMap::new();

    for (i, ir) in instructions.iter().enumerate() {
        if let Code::SetLabel(name) = &ir.code {
            labels.insert(name.clone(), i);
        }
    }

    for ir in instructions.iter_mut() {
        match &mut ir.code {
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
