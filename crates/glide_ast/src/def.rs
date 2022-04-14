use glide_span::Span;

use crate::{stmt::Stmt, ty::Ty};

#[derive(Debug)]
pub enum Def<'a> {
    Func(Func<'a>),
}

#[derive(Debug)]
pub struct Func<'a> {
    pub name: Span<'a>,
    pub generics: Vec<Span<'a>>,
    pub params: Vec<(Span<'a>, Ty<'a>)>,
    pub ret: Option<Ty<'a>>,
    pub stmts: Vec<Stmt<'a>>,
}
