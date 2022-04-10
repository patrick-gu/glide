use std::result;

#[derive(Debug)]
pub enum Error {
    WrongTyArgs,
    TyMismatch,
    Shadow,
    UnresolvedName,
    IntegerOverflow,
    NoMain,
    TyRecursion,
    CannotInfer,
}

pub(crate) type Result<T> = result::Result<T, Error>;
