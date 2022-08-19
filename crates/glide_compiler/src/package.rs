use crate::{
    namespace::Namespace,
    registry::{Id, Registrable, Registry},
};

pub(crate) struct Package {
    pub(crate) namespace: Namespace<'static>,
}

impl Registrable for Package {}

pub(crate) type Packages = Registry<Package>;

pub(crate) type PackageId = Id<Package>;
