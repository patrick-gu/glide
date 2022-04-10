use std::collections::HashMap;

pub(crate) struct Strings {
    strings: Vec<Vec<u8>>,
    lookup: HashMap<Vec<u8>, u32>,
}

impl Strings {
    pub(crate) fn new() -> Self {
        Self {
            strings: Vec::new(),
            lookup: HashMap::new(),
        }
    }

    pub(crate) fn add(&mut self, string: Vec<u8>) -> u32 {
        if let Some(id) = self.lookup.get(&string) {
            *id
        } else {
            let id = self.strings.len().try_into().unwrap();
            self.strings.push(string.clone());
            self.lookup.insert(string, id);
            id
        }
    }

    pub(crate) fn into_inner(self) -> Vec<Vec<u8>> {
        self.strings
    }
}
