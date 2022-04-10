use std::{collections::HashMap, hash::Hash};

#[derive(Debug)]
pub struct Bytecode {
    pub funcs: Vec<Func>,
    pub main_func: usize,
    pub tys: Tys,
    pub strings: Vec<Vec<u8>>,
}

#[derive(Debug)]
pub struct Func {
    pub name: u32,
    pub ret_size: u32,
    pub data: FuncData,
}

#[derive(Debug)]
pub enum FuncData {
    Custom(Vec<u8>),
    Print,
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Ty {
    Scalar,
    String,
    Func,
    Slice(Vec<TyId>),
}

pub struct TysBuilder {
    tys: Tys,
    lookup: HashMap<Ty, TyId>,
}

impl TysBuilder {
    pub fn new() -> Self {
        Self {
            tys: Tys::new(),
            lookup: HashMap::from([
                (Ty::Scalar, TyId::SCALAR),
                (Ty::String, TyId::STRING),
                (Ty::Func, TyId::FUNC),
            ]),
        }
    }

    pub fn add(&mut self, ty: Ty) -> TyId {
        if let Some(id) = self.lookup.get(&ty) {
            *id
        } else {
            let id = self.tys.add(ty.clone());
            self.lookup.insert(ty, id);
            id
        }
    }

    pub fn build(self) -> Tys {
        self.tys
    }
}

#[derive(Debug)]
pub struct Tys(Vec<Ty>);

impl Tys {
    fn new() -> Self {
        Self(vec![Ty::Scalar, Ty::String, Ty::Func])
    }

    fn add(&mut self, ty: Ty) -> TyId {
        let id = TyId(self.0.len().try_into().unwrap());
        self.0.push(ty);
        id
    }

    pub fn get(&self, id: TyId) -> &Ty {
        &self.0[id.0 as usize]
    }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub struct TyId(u16);

impl TyId {
    pub const SCALAR: Self = Self(0);
    pub const STRING: Self = Self(1);
    pub const FUNC: Self = Self(2);
}

pub mod insn {
    pub const PUSH_SCALAR: u8 = 0;
    pub const PUSH_CONSTANT_STRING: u8 = 1;
    pub const PUSH_FUNC: u8 = 2;
    pub const CALL: u8 = 3;
    pub const RET: u8 = 4;
    pub const POP: u8 = 5;
    pub const PUSH_LOCAL: u8 = 6;
}
