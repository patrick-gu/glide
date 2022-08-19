use std::result;

#[derive(Debug)]
pub enum Error {
    WrongTyArgs,
    NotTy,
    TyMismatch,
    Shadow,
    UnresolvedName,
    IntegerOverflow,
    NoMain,
    TyRecursion,
    CannotInfer,
    NotValue,
    VisiblityRestricted,
}

pub(crate) type Result<T> = result::Result<T, Error>;
