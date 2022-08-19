use crate::registry::{Id, Registrable, Registry};

pub(crate) struct Attribute {
    pub(crate) name: String,
}

impl Registrable for Attribute {}

pub(crate) type AttributeId = Id<Attribute>;

pub(crate) type Attributes = Registry<Attribute>;
