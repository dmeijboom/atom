use crate::compiler::ir::Label;

#[derive(Debug)]
pub struct GlobalLabel {
    pub label: Label,
    pub frame: usize,
}

impl GlobalLabel {
    pub fn new(frame: usize, label: Label) -> Self {
        Self { frame, label }
    }
}
