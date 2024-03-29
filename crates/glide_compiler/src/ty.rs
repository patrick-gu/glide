use std::collections::HashMap;

use crate::{
    error::{Error, Result},
    registry::{Id, Registrable, Registry},
};

/// A type, consisting of a type constructor instantiated with type parameters.
#[derive(Debug)]
pub(crate) enum Ty {
    Void,
    Int,
    Bool,
    String,
    Slice(TyId),
    Func(Vec<TyId>, TyId),

    /// A parametric type. Each has a unique [`TyId`], which is compared to determine equality.
    Param,

    /// Type will be inferred to be another type.
    Infer,

    /// Equal to another type.
    Equal(TyId),
}

impl Registrable for Ty {
    fn init(registry: &mut Registry<Self>) {
        registry.add(Ty::Void);
        registry.add(Ty::Int);
        registry.add(Ty::Bool);
        registry.add(Ty::String);
    }
}

/// An identifier for a [`Ty`] in a [`Tys`].
pub(crate) type TyId = Id<Ty>;

impl TyId {
    pub(crate) const VOID: Self = Self::manual(0);
    pub(crate) const INT: Self = Self::manual(1);
    pub(crate) const BOOL: Self = Self::manual(2);
    pub(crate) const STRING: Self = Self::manual(3);
}

/// Contains [`Ty`]s and assigns [`TyId`]s.
pub(crate) type Tys = Registry<Ty>;

impl Tys {
    /// Unifies two types, by trying to make them equal.
    pub(crate) fn unify(&mut self, l: TyId, r: TyId) -> Result<()> {
        if l == r {
            return Ok(());
        }
        match self.get_mut_2(l, r) {
            (_, infer @ Ty::Infer) => {
                *infer = Ty::Equal(l);
                Ok(())
            }
            (infer @ Ty::Infer, _) => {
                *infer = Ty::Equal(r);
                Ok(())
            }
            (Ty::Void, Ty::Void)
            | (Ty::Int, Ty::Int)
            | (Ty::String, Ty::String)
            | (Ty::Bool, Ty::Bool) => Ok(()),
            (Ty::Equal(l), _) => {
                let l = *l;
                self.unify(l, r)
            }
            (_, Ty::Equal(r)) => {
                let r = *r;
                self.unify(l, r)
            }
            (Ty::Slice(a), Ty::Slice(b)) => {
                let a = *a;
                let b = *b;
                self.unify(a, b)
            }
            (Ty::Func(params_a, ret_a), Ty::Func(params_b, ret_b)) => {
                let ret_a = *ret_a;
                let ret_b = *ret_b;
                if params_a.len() != params_b.len() {
                    return Err(Error::TyMismatch);
                }
                for (a, b) in params_a.clone().into_iter().zip(params_b.clone()) {
                    self.unify(a, b)?;
                }
                self.unify(ret_a, ret_b)
            }
            (Ty::Param, Ty::Param) | (_, _) => Err(Error::TyMismatch),
        }
    }

    pub(crate) fn clone_substitute(&mut self, ty: TyId, map: &HashMap<TyId, TyId>) -> TyId {
        if let Some(sub) = map.get(&ty) {
            assert!(matches!(self.get(ty), Ty::Param));
            return *sub;
        }
        match self.get(ty) {
            Ty::Void | Ty::Int | Ty::Bool | Ty::String => ty,
            Ty::Slice(ty) => {
                let ty = *ty;
                let ty = self.clone_substitute(ty, map);
                self.add(Ty::Slice(ty))
            }
            Ty::Func(params, ret) => {
                let mut params = params.clone();
                let ret = *ret;
                for param in &mut params {
                    *param = self.clone_substitute(*param, map);
                }
                let ret = self.clone_substitute(ret, map);
                self.add(Ty::Func(params, ret))
            }
            Ty::Param => todo!(),
            Ty::Infer => todo!(),
            Ty::Equal(_) => todo!(),
        }
    }

    pub(crate) fn is_void(&self, ty: TyId) -> bool {
        match self.get(ty) {
            Ty::Void => true,
            Ty::Equal(ty) => {
                let ty = *ty;
                self.is_void(ty)
            }
            _ => false,
        }
    }
}
