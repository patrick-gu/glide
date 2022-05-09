use crate::ty::TyId;

/// A type constructor.
#[derive(Debug)]
pub(crate) enum TyConstr {
    Void,
    Int,
    String,
    Slice,
    Param(TyId),
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct TyConstrId(usize);

pub(crate) struct TyConstrs(Vec<TyConstr>);

impl TyConstrs {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn add(&mut self, ty: TyConstr) -> TyConstrId {
        let id = TyConstrId(self.0.len());
        self.0.push(ty);
        id
    }

    pub(crate) fn get(&self, id: TyConstrId) -> &TyConstr {
        &self.0[id.0]
    }
}
