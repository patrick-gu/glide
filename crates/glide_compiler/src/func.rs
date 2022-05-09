use std::collections::HashMap;

use crate::{ty::TyId, value::Value};

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
    Normal(Vec<Value>),
    Print,
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub(crate) struct FuncId(usize);

#[derive(Debug)]
pub(crate) struct Funcs(Vec<Func>);

impl Funcs {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn add(&mut self, func: Func) -> FuncId {
        let id = FuncId(self.0.len());
        self.0.push(func);
        id
    }

    pub(crate) fn get(&self, id: FuncId) -> &Func {
        &self.0[id.0]
    }

    pub(crate) fn get_mut(&mut self, id: FuncId) -> &mut Func {
        &mut self.0[id.0]
    }
}

#[derive(Clone, Debug)]
pub(crate) struct FuncUsage {
    pub(crate) func: FuncId,
    pub(crate) ty_args: Vec<TyId>,
    pub(crate) signature: TyId,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct FuncCompiled {
    pub(crate) func: FuncId,
    pub(crate) ty_args: Vec<glide_ir::Ty>,
}

pub(crate) struct FuncsCompiled {
    lookup: HashMap<FuncCompiled, glide_ir::FuncId>,
}

impl FuncsCompiled {
    pub(crate) fn new() -> Self {
        Self {
            lookup: HashMap::new(),
        }
    }

    pub(crate) fn get(&self, key: &FuncCompiled) -> Option<glide_ir::FuncId> {
        self.lookup.get(key).copied()
    }

    pub(crate) fn insert(&mut self, key: FuncCompiled, id: glide_ir::FuncId) {
        let previous = self.lookup.insert(key, id);
        assert!(previous.is_none());
    }
}
