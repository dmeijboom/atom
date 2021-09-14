pub fn parse_line_numbers_offset(source: &str) -> Vec<usize> {
    let mut i = 0;
    let mut line_numbers_offset = vec![];

    while i < source.len() {
        if let Some(offset) = source[i + 1..].find('\n') {
            line_numbers_offset.push(offset + i);

            i += offset + 1;

            continue;
        }

        break;
    }

    line_numbers_offset
}
