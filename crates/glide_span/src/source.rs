use std::fmt;

/// A source file.
pub struct Source {
    pub name: String,
    pub data: String,
}

impl Source {
    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn anonymous(data: impl Into<String>) -> Self {
        Self {
            name: "source file <anonymous>".to_owned(),
            data: data.into(),
        }
    }
}

impl fmt::Debug for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
