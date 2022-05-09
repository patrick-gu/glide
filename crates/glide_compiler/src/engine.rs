use std::{collections::HashMap, iter};

use crate::{
    error::{Error, Result},
    func::{FuncId, FuncUsage, Funcs},
    ty::{Ty, TyId, Tys},
    ty_constr::{TyConstr, TyConstrId, TyConstrs},
};

pub(crate) struct Engine {
    pub(crate) tys: Tys,
    pub(crate) ty_constrs: TyConstrs,
    pub(crate) funcs: Funcs,
}

impl Engine {
    pub(crate) fn new() -> Self {
        Self {
            tys: Tys::new(),
            ty_constrs: TyConstrs::new(),
            funcs: Funcs::new(),
        }
    }

    pub(crate) fn instantiate_ty(
        &mut self,
        ty_constr: TyConstrId,
        ty_args: Vec<TyId>,
    ) -> Result<TyId> {
        match self.ty_constrs.get(ty_constr) {
            TyConstr::Void if ty_args.is_empty() => Ok(self.tys.add(Ty::Void)),
            TyConstr::Int if ty_args.is_empty() => Ok(self.tys.add(Ty::Int)),
            TyConstr::String if ty_args.is_empty() => Ok(self.tys.add(Ty::String)),
            TyConstr::Slice if ty_args.len() == 1 => Ok(self.tys.add(Ty::Slice(ty_args[0]))),
            TyConstr::Param(ty) if ty_args.is_empty() => Ok(*ty),
            _ => Err(Error::WrongTyArgs),
        }
    }

    pub(crate) fn use_func(&mut self, func_id: FuncId) -> FuncUsage {
        let func = self.funcs.get(func_id);

        let ty_args: Vec<TyId> = iter::repeat_with(|| self.tys.add(Ty::Infer))
            .take(func.ty_params.len())
            .collect();

        let map: HashMap<TyId, TyId> = func
            .ty_params
            .iter()
            .copied()
            .zip(ty_args.iter().copied())
            .collect();
        let signature = self.tys.clone_substitute(func.signature, &map);

        FuncUsage {
            func: func_id,
            ty_args,
            signature,
        }
    }
}
