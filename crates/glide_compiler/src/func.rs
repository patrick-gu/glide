use std::collections::HashMap;

use crate::{
    registry::{Id, Registrable, Registry},
    ty::TyId,
    value::Value,
};

#[derive(Debug)]
pub(crate) struct Func {
    pub(crate) name: String,
    pub(crate) ty_params: Vec<TyId>,
    pub(crate) signature: TyId,
    pub(crate) body: FuncBody,
    pub(crate) cur_mono: bool,
}
impl Registrable for Func {}

pub(crate) type FuncId = Id<Func>;

pub(crate) type Funcs = Registry<Func>;

#[derive(Clone, Debug)]
pub(crate) enum FuncBody {
    Placeholder,
    Normal(Value),
    Println,
    PrintlnInt,
    Add,
    Sub,
    EqInt,
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
