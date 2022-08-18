use glide_span::Span;

use crate::{expr::Block, ty::Ty};

#[derive(Debug)]
pub struct Def<'a> {
    pub attributes: Vec<AttributeUsage<'a>>,
    pub kind: DefKind<'a>,
}

#[derive(Debug)]
pub struct AttributeUsage<'a> {
    pub name: Span<'a>,
}

#[derive(Debug)]
pub enum DefKind<'a> {
    Func(Func<'a>),
    Attribute(AttributeDef<'a>),
    Struct(Struct<'a>),
}

#[derive(Debug)]
pub struct Func<'a> {
    pub name: Span<'a>,
    pub generics: Vec<Span<'a>>,
    pub params: Vec<Field<'a>>,
    pub ret: Option<Ty<'a>>,
    pub block: Block<'a>,
}

#[derive(Debug)]
pub struct AttributeDef<'a> {
    pub name: Span<'a>,
}

#[derive(Debug)]
pub struct Struct<'a> {
    pub name: Span<'a>,
    pub generics: Vec<Span<'a>>,
    pub fields: Vec<Field<'a>>,
}

#[derive(Debug)]
pub struct Field<'a> {
    pub name: Span<'a>,
    pub ty: Ty<'a>,
}
