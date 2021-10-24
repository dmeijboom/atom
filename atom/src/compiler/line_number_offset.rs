use atom_ir::Location;
use std::ops::Range;

/// Unfortunately the parser doesn't expose line or column information so we're using a map of
/// newline positions to calculate the line number and column instead
#[derive(Clone)]
pub struct LineNumberOffset {
    data: Vec<usize>,
}

impl LineNumberOffset {
    pub fn parse(source: &str) -> Self {
        let mut i = 0;
        let mut data = vec![];

        while i < source.len() {
            if let Some(offset) = source[i + 1..].find('\n') {
                data.push(offset + i);

                i += offset + 1;

                continue;
            }

            break;
        }

        Self { data }
    }

    pub fn get_location(&self, offset: &Range<usize>) -> Location {
        let index = self.data.iter().position(|start| offset.start < *start);

        if let Some(index) = index {
            let length = self.data.len();

            if length > 0 && index > 0 {
                let start = self.data[index - 1];

                return Location::new(offset.clone(), index + 1, offset.start - start + 1);
            }
        }

        Location::new(offset.clone(), 1, offset.start + 1)
    }
}
