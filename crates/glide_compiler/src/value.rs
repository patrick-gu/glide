use crate::func::{FuncId, FuncUsage};

pub(crate) type Value = glide_ir::Value<FuncUsage>;

#[derive(Copy, Clone, Debug)]
pub(crate) enum ValueRef {
    Local(usize),
    Func(FuncId),
}
