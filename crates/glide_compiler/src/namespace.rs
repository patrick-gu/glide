use std::collections::HashMap;

use crate::{
    attribute::AttributeId,
    error::{Error, Result},
    func::FuncId,
    struc::StructId,
    ty::TyId,
    visibility::Visibility,
};

#[derive(Clone, Debug)]
pub(crate) struct Namespace<'a> {
    previous: Option<&'a Namespace<'a>>,
    entries: HashMap<String, NamespaceEntry>,
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct NamespaceEntry {
    pub(crate) visibility: Visibility,
    pub(crate) kind: NamespaceEntryKind,
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum NamespaceEntryKind {
    Void,
    Int,
    Bool,
    String,
    Slice,
    TyParam(TyId),
    Struct(StructId),
    Local(usize),
    Func(FuncId),
    Attribute(AttributeId),
}

impl Namespace<'static> {
    pub(crate) fn new() -> Self {
        Self {
            previous: None,
            entries: HashMap::new(),
        }
    }
}

impl<'a> Namespace<'a> {
    pub(crate) fn sub(&self) -> Namespace<'_> {
        Namespace {
            previous: Some(self),
            entries: HashMap::new(),
        }
    }

    pub(crate) fn insert(&mut self, name: String, entry: NamespaceEntry) -> Result<()> {
        if self.get(&name).is_some() {
            return Err(Error::Shadow);
        }
        self.entries.insert(name, entry);
        Ok(())
    }

    pub(crate) fn get(&self, name: &str) -> Option<NamespaceEntry> {
        self.entries
            .get(name)
            .copied()
            .or_else(|| self.get_deep(name))
    }

    fn get_deep(&self, name: &str) -> Option<NamespaceEntry> {
        self.previous.and_then(|p| p.get(name))
    }
}
