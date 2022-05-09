use std::fmt;

#[derive(Debug)]
pub struct Ir {
    pub funcs: Funcs,
    pub main_func: FuncId,
}

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub signature: Ty,
    pub body: FuncBody,
}

#[derive(Copy, Clone, Debug)]
pub struct FuncId(usize);

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

    pub fn get(&self, id: FuncId) -> &Func {
        &self.0[id.0]
    }

    pub fn get_mut(&mut self, id: FuncId) -> &mut Func {
        &mut self.0[id.0]
    }

    pub fn inner(&self) -> &[Func] {
        &self.0
    }
}

impl Default for Funcs {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub enum FuncBody {
    Normal(Vec<Value>),
    Print,
}

/// A type.
#[derive(Eq, PartialEq, Hash, Clone)]
pub enum Ty {
    Void,
    Int,
    String,
    Slice(Box<Ty>),
    Func(Vec<Ty>, Box<Ty>),
}

impl Ty {
    /// Returns `true` if this type is empty.
    fn is_empty(&self) -> bool {
        matches!(self, Self::Void)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Int => write!(f, "Int"),
            Self::String => write!(f, "String"),
            Self::Slice(elem) => write!(f, "Slice<{}>", elem),
            Self::Func(params, ret) => {
                write!(f, "func (")?;
                if let Some((last, rest)) = params.split_last() {
                    for param in rest {
                        write!(f, "{}, ", param)?;
                    }
                    write!(f, "{}", last)?;
                }
                write!(f, ") {}", ret)
            }
        }
    }
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug)]
pub enum Value {
    Void,
    ConstantInt(i64),
    ConstantString(Vec<u8>),
    Local(usize),
    Param(usize),
    Func(FuncId),
    Call(Box<Value>, Vec<Value>),
    Ret(Box<Value>),
}
