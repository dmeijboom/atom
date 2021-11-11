use crate::compiler::ir::Location;

#[derive(Debug, Clone, PartialEq)]
pub struct Origin {
    pub filename: Option<String>,
    pub module_id: usize,
    pub module_name: String,
    pub location: Location,
}

impl Origin {
    pub fn new(
        module_id: usize,
        module_name: String,
        filename: Option<String>,
        location: Location,
    ) -> Self {
        Self {
            filename,
            module_id,
            module_name,
            location,
        }
    }

    pub fn with_filename(mut self, filename: String) -> Self {
        self.filename = Some(filename);

        self
    }
}
