use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
};

use unused::Unused;

pub(crate) trait Registrable: Sized {
    fn init(_registry: &mut Registry<Self>) {}
}

#[derive(Debug)]
pub(crate) struct Registry<T>(Vec<T>);

impl<T: Registrable> Registry<T> {
    pub(crate) fn new() -> Self {
        let mut registry = Self(Vec::new());
        T::init(&mut registry);
        registry
    }

    pub(crate) fn add(&mut self, value: T) -> Id<T> {
        let id = Id {
            idx: self.0.len(),
            _unused: Unused,
        };
        self.0.push(value);
        id
    }

    pub(crate) fn get(&self, id: Id<T>) -> &T {
        &self.0[id.idx]
    }

    pub(crate) fn get_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.0[id.idx]
    }

    pub(crate) fn get_mut_2(
        &mut self,
        Id { idx: a, .. }: Id<T>,
        Id { idx: b, .. }: Id<T>,
    ) -> (&mut T, &mut T) {
        match a.cmp(&b) {
            Ordering::Less => {
                let (h, t) = self.0.split_at_mut(b);
                (&mut h[a], &mut t[0])
            }
            Ordering::Equal => panic!(),
            Ordering::Greater => {
                let (h, t) = self.0.split_at_mut(a);
                (&mut t[0], &mut h[b])
            }
        }
    }
}

pub(crate) struct Id<T> {
    idx: usize,
    _unused: Unused!(T),
}

impl<T> Id<T> {
    pub(crate) const PLACEHOLDER: Self = Self {
        idx: usize::MAX,
        _unused: Unused,
    };

    pub(crate) const fn manual(idx: usize) -> Self {
        Self {
            idx,
            _unused: Unused,
        }
    }
}

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Id").field(&self.idx).finish()
    }
}

impl<T> Copy for Id<T> {}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<T> Eq for Id<T> {}

impl<T> PartialOrd for Id<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Id<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.idx.cmp(&other.idx)
    }
}

impl<T> Hash for Id<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.idx.hash(state);
    }
}
