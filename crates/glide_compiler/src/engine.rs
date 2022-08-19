use std::{collections::HashMap, iter};

use crate::{
    attribute::Attributes,
    func::{FuncId, FuncUsage, Funcs},
    ty::{Ty, TyId, Tys},
};

pub(crate) struct Engine {
    pub(crate) tys: Tys,
    pub(crate) funcs: Funcs,
    pub(crate) attributes: Attributes,
}

impl Engine {
    pub(crate) fn new() -> Self {
        Self {
            tys: Tys::new(),
            funcs: Funcs::new(),
            attributes: Attributes::new(),
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
