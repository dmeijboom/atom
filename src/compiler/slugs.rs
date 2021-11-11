use std::collections::HashMap;

use wyhash2::WyHash;

pub struct Slugs {
    slugs: HashMap<String, bool, WyHash>,
}

impl Slugs {
    pub fn new() -> Self {
        Self {
            slugs: HashMap::with_hasher(WyHash::default()),
        }
    }

    pub fn get(&mut self, prefix: &str) -> String {
        let mut i: i64 = 0;

        loop {
            let slug = if i == 0 {
                prefix.to_string()
            } else {
                format!("{}{}", prefix, i)
            };

            if !self.slugs.contains_key(&slug) {
                self.slugs.insert(slug.clone(), true);

                return slug;
            }

            i += 1;
        }
    }
}
