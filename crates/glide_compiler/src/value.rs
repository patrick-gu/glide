use crate::func::{FuncId, FuncUsage};

#[derive(Clone, Debug)]
pub(crate) enum Value {
    Void,
    ConstantInt(i64),
    ConstantString(Vec<u8>),
    Local(usize),
    Param(usize),
    Func(FuncUsage),
    Call(Box<Value>, Vec<Value>),
    Ret(Box<Value>),
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum ValueRef {
    Param(usize),
    Local(usize),
    Func(FuncId),
}
