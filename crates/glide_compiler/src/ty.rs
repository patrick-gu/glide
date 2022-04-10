use std::{collections::HashMap, mem};

use crate::error::{Error, Result};

#[derive(Debug)]
pub(crate) enum Ty {
    Void,
    Int,
    String,
    Slice(TyId),
    Func(Vec<TyId>, TyId),
    Param,
    Infer,
    Equal(TyId),
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct TyId(usize);

#[derive(Debug)]
pub(crate) struct Tys(Vec<Ty>);

impl Tys {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn add(&mut self, ty: Ty) -> TyId {
        let id = TyId(self.0.len());
        self.0.push(ty);
        id
    }

    pub(crate) fn get(&self, id: TyId) -> &Ty {
        &self.0[id.0]
    }

    fn get_mut_2(&mut self, TyId(mut a): TyId, TyId(mut b): TyId) -> (&mut Ty, &mut Ty) {
        assert_ne!(a, b);
        if a > b {
            mem::swap(&mut a, &mut b);
        }
        let (h, t) = self.0.split_at_mut(b);
        (&mut h[a], &mut t[0])
    }

    pub(crate) fn unify(&mut self, l: TyId, r: TyId) -> Result<()> {
        if l.0 == r.0 {
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
            (Ty::Void, Ty::Void) | (Ty::Int, Ty::Int) | (Ty::String, Ty::String) => Ok(()),
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
            Ty::Void | Ty::Int | Ty::String => ty,
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
