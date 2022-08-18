use std::collections::HashMap;

use crate::{
    attribute::AttributeId,
    error::{Error, Result},
    ty_constr::TyConstrId,
    value::ValueRef,
};

pub(crate) struct Namespace<'a> {
    previous: Option<&'a Namespace<'a>>,
    ty_constrs: HashMap<String, TyConstrId>,
    values: HashMap<String, ValueRef>,
    attributes: HashMap<String, AttributeId>,
}

impl<'a> Namespace<'a> {
    pub(crate) fn new() -> Self {
        Self {
            previous: None,
            ty_constrs: HashMap::new(),
            values: HashMap::new(),
            attributes: HashMap::new(),
        }
    }

    pub(crate) fn sub(&self) -> Namespace<'_> {
        Namespace {
            previous: Some(self),
            ty_constrs: HashMap::new(),
            values: HashMap::new(),
            attributes: HashMap::new(),
        }
    }

    pub(crate) fn insert_ty_constr(&mut self, name: String, id: TyConstrId) -> Result<()> {
        if self.get_ty_constr(&name).is_some() {
            return Err(Error::Shadow);
        }
        self.ty_constrs.insert(name, id);
        Ok(())
    }

    pub(crate) fn get_ty_constr(&self, name: &str) -> Option<TyConstrId> {
        self.ty_constrs
            .get(name)
            .copied()
            .or_else(|| self.get_ty_constr_deep(name))
    }

    fn get_ty_constr_deep(&self, name: &str) -> Option<TyConstrId> {
        self.previous.and_then(|p| p.get_ty_constr(name))
    }

    pub(crate) fn insert_value(&mut self, name: String, value: ValueRef) -> Result<()> {
        if self.get_value(&name).is_some() {
            return Err(Error::Shadow);
        }
        self.values.insert(name, value);
        Ok(())
    }

    pub(crate) fn get_value(&self, name: &str) -> Option<ValueRef> {
        self.values
            .get(name)
            .copied()
            .or_else(|| self.get_value_deep(name))
    }

    fn get_value_deep(&self, name: &str) -> Option<ValueRef> {
        self.previous.and_then(|p| p.get_value(name))
    }

    pub(crate) fn insert_attribute(&mut self, name: String, value: AttributeId) -> Result<()> {
        if self.get_attribute(&name).is_some() {
            return Err(Error::Shadow);
        }
        self.attributes.insert(name, value);
        Ok(())
    }

    pub(crate) fn get_attribute(&self, name: &str) -> Option<AttributeId> {
        self.attributes
            .get(name)
            .copied()
            .or_else(|| self.get_attribute_deep(name))
    }

    fn get_attribute_deep(&self, name: &str) -> Option<AttributeId> {
        self.previous.and_then(|p| p.get_attribute(name))
    }
}
