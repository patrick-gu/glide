use std::collections::HashMap;

use crate::{engine::Engine, ty::TyId, value::ValueId};

#[derive(Debug)]
pub(crate) struct Func {
    pub(crate) name: String,
    pub(crate) ty_params: Vec<TyId>,
    pub(crate) signature: TyId,
    pub(crate) body: FuncBody,
    pub(crate) cur_mono: bool,
}

#[derive(Clone, Debug)]
pub(crate) enum FuncBody {
    Normal(Vec<Insn>),
    Print,
    StringConcat,
}

#[derive(Debug)]
pub(crate) struct InstantiatedFunc {
    pub(crate) func: ValueId,
    pub(crate) ty_args: Vec<TyId>,
    pub(crate) signature: TyId,
    pub(crate) comp_idx: Option<glide_ir::FuncId>,
}

impl InstantiatedFunc {
    pub(crate) fn map(&self, engine: &Engine) -> HashMap<TyId, TyId> {
        let ty_params = &engine.values.get(self.func).as_func().unwrap().ty_params;
        assert_eq!(self.ty_args.len(), ty_params.len());
        ty_params
            .iter()
            .copied()
            .zip(self.ty_args.iter().copied())
            .collect()
    }
}

pub(crate) struct InstantiatedFuncs(Vec<InstantiatedFunc>);

impl InstantiatedFuncs {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn add(&mut self, func: InstantiatedFunc) -> InstantiatedFuncId {
        let id = InstantiatedFuncId(self.0.len());
        self.0.push(func);
        id
    }

    pub(crate) fn get(&self, id: InstantiatedFuncId) -> &InstantiatedFunc {
        &self.0[id.0]
    }

    pub(crate) fn get_mut(&mut self, id: InstantiatedFuncId) -> &mut InstantiatedFunc {
        &mut self.0[id.0]
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct InstantiatedFuncId(usize);

#[derive(Clone, Debug)]
pub(crate) enum Insn {
    PushVoid,
    PushInt(isize),
    PushString(Vec<u8>),
    PushLocal(usize),
    PushFunc(InstantiatedFuncId),
    Pop,
    Call { at: usize, ret: TyId },
    Ret,
}
