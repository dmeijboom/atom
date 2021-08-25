use atom_ir::Pos;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Origin {
    pub pos: Pos,
    pub location: String,
    pub module_name: String,
}

impl Origin {
    pub fn new(module_name: String, location: String, pos: Pos) -> Self {
        Self {
            pos,
            location,
            module_name,
        }
    }

    pub fn with_location(mut self, location: String) -> Self {
        self.location = location;

        self
    }
}
