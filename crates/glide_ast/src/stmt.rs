use glide_span::Span;

use crate::{expr::Expr, ty::Ty};

#[derive(Debug)]
pub enum Stmt<'a> {
    Var(VarDecl<'a>),
    Expr(Expr<'a>),
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    pub name: Span<'a>,
    pub ty: Option<Ty<'a>>,
    pub value: Expr<'a>,
}
