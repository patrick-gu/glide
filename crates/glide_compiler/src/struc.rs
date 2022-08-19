use crate::registry::{Id, Registrable, Registry};

#[derive(Debug)]
pub(crate) struct Struct {}

impl Registrable for Struct {}

pub(crate) type Structs = Registry<Struct>;

pub(crate) type StructId = Id<Struct>;
