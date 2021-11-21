use std::collections::HashMap;

use crate::compiler::ir::{Code, Label, IR};
use crate::compiler::Module;

macro_rules! label_name {
    ($label:expr) => {
        match $label {
            Label::Name(name) => name,
            Label::Index(_) => unreachable!(),
        }
    };
}

/// Calculate the absolute offset for labels by using their name so that the vm can jump to it directly
pub fn optimize(_module: &Module, instructions: &mut IR) {
    let mut labels = HashMap::new();

    'find_set_label: loop {
        for (i, code) in instructions.iter().enumerate() {
            if let Code::SetLabel(name) = &code {
                labels.insert(name.clone(), i);
                instructions.remove(i);

                continue 'find_set_label;
            }
        }

        break;
    }

    for code in instructions.iter_mut() {
        match code {
            Code::Jump(label) => {
                *label = Label::Index(*labels.get(label_name!(&label)).unwrap());
            }
            Code::JumpIfTrue(label) | Code::JumpOnError(label) => {
                *label = Label::Index(*labels.get(label_name!(&label)).unwrap());
            }
            Code::Branch((true_label, false_label)) => {
                *true_label = Label::Index(*labels.get(label_name!(&true_label)).unwrap());
                *false_label = Label::Index(*labels.get(label_name!(&false_label)).unwrap());
            }
            _ => {}
        }
    }
}
