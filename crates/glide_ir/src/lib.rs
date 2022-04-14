use std::{collections::HashMap, fmt};

#[derive(Debug)]
pub struct Ir {
    pub funcs: Funcs,
    pub main_func: FuncId,
    pub tys: Tys,
}

impl Ir {
    pub fn new() -> Self {
        Self {
            funcs: Funcs::new(),
            main_func: FuncId(0),
            tys: Tys::new(),
        }
    }
}

#[derive(Debug)]
pub struct Funcs(Vec<Func>);

impl Funcs {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn add(&mut self, func: Func) -> FuncId {
        let id = FuncId(self.0.len());
        self.0.push(func);
        id
    }

    pub fn get_mut(&mut self, id: FuncId) -> &mut Func {
        &mut self.0[id.0]
    }

    pub fn inner(&self) -> &[Func] {
        &self.0
    }
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub signature: TyId,
    pub data: FuncData,
}

#[derive(Debug)]
pub enum FuncData {
    Print,
    StringConcat,
    Normal(Vec<Insn>),
}

#[derive(Debug)]
pub enum Insn {
    PushVoid,
    PushInt(isize),
    PushString(Vec<u8>),
    PushLocal(usize),
    PushFunc(FuncId),
    Pop,
    Call { at: usize, ret: TyId },
    Ret,
}

#[derive(Copy, Clone, Debug)]
pub struct FuncId(pub usize);

#[derive(Debug)]
pub struct Tys {
    tys: Vec<Ty>,
    lookup: HashMap<Ty, TyId>,
}

impl Tys {
    pub fn new() -> Self {
        let mut tys = Self {
            tys: Vec::new(),
            lookup: HashMap::new(),
        };
        tys.add(Ty::Void);
        tys.add(Ty::Int);
        tys.add(Ty::String);
        tys
    }

    pub fn add(&mut self, ty: Ty) -> TyId {
        if let Some(id) = self.lookup.get(&ty) {
            *id
        } else {
            let id = TyId(self.tys.len());
            self.tys.push(ty.clone());
            self.lookup.insert(ty, id);
            id
        }
    }

    pub fn get(&self, id: TyId) -> &Ty {
        &self.tys[id.0]
    }

    pub fn inner(&self) -> &[Ty] {
        &self.tys
    }

    pub fn display(&self, ty: TyId) -> impl fmt::Display + '_ {
        TyDisplay { tys: self, ty }
    }

    pub fn size(&self, ty: TyId) -> usize {
        match self.get(ty) {
            Ty::Void => 0,
            Ty::Int => 1,
            Ty::String => 1,
            Ty::Slice(_) => 1,
            Ty::Func(_, _) => 1,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub enum Ty {
    Void,
    Int,
    String,
    Slice(TyId),
    Func(Vec<TyId>, TyId),
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub struct TyId(usize);

struct TyDisplay<'a> {
    tys: &'a Tys,
    ty: TyId,
}

impl fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.tys.get(self.ty) {
            Ty::Void => write!(f, "Void"),
            Ty::Int => write!(f, "Int"),
            Ty::String => write!(f, "String"),
            Ty::Slice(elem) => {
                write!(f, "Slice<{}>", self.tys.display(*elem))
            }
            Ty::Func(params, ret) => {
                write!(f, "func (")?;
                if let Some((last, rest)) = params.split_last() {
                    for &param in rest {
                        write!(f, "{}, ", self.tys.display(param))?;
                    }
                    write!(f, "{}", self.tys.display(*last))?;
                }
                write!(f, ") {}", self.tys.display(*ret))
            }
        }
    }
}
