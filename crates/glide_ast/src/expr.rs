use glide_span::Span;

use crate::stmt::Stmt;

#[derive(Debug)]
pub enum Expr<'a> {
    Integer(Span<'a>),
    String { data: Vec<u8> },
    Var(Span<'a>),
    Call(Call<'a>),
    If(If<'a>),
}

#[derive(Debug)]
pub struct Call<'a> {
    pub receiver: Box<Expr<'a>>,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub stmts: Vec<Stmt<'a>>,
}

#[derive(Debug)]
pub struct If<'a> {
    pub branches: Vec<(Expr<'a>, Block<'a>)>,
    pub els: Option<Block<'a>>,
}
