use crate::func::Func;

#[derive(Debug)]
pub(crate) enum Value {
    Local(usize),
    Func(Func),
}

impl Value {
    pub(crate) fn as_func(&self) -> Option<&Func> {
        if let Self::Func(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub(crate) fn as_mut_func(&mut self) -> Option<&mut Func> {
        if let Self::Func(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
pub(crate) struct ValueId(usize);

pub(crate) struct Values(Vec<Value>);

impl Values {
    pub(crate) fn new() -> Self {
        Self(Vec::new())
    }

    pub(crate) fn add(&mut self, value: Value) -> ValueId {
        let id = ValueId(self.0.len());
        self.0.push(value);
        id
    }

    pub(crate) fn get(&self, id: ValueId) -> &Value {
        &self.0[id.0]
    }

    pub(crate) fn get_mut(&mut self, id: ValueId) -> &mut Value {
        &mut self.0[id.0]
    }
}
