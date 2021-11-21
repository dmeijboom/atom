use crate::compiler::ir::Label;

#[derive(Debug)]
pub struct GlobalLabel {
    pub label: Label,
    pub context_id: usize,
}

impl GlobalLabel {
    pub fn new(context_id: usize, label: Label) -> Self {
        Self { context_id, label }
    }
}
