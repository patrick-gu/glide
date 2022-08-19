use glide_span::Span;

use crate::{expr::Block, path::Path, ty::Ty};

#[derive(Debug)]
pub struct Def<'a> {
    pub visibility: Visibility,
    pub attributes: Vec<AttributeUsage<'a>>,
    pub kind: DefKind<'a>,
}

#[derive(Debug)]
pub enum Visibility {
    Pub,
    Priv,
    Unspecified,
}

#[derive(Debug)]
pub struct AttributeUsage<'a> {
    pub path: Path<'a>,
}

#[derive(Debug)]
pub enum DefKind<'a> {
    Func(Func<'a>),
    Attribute(AttributeDef<'a>),
    Struct(Struct<'a>),
    Use(Use<'a>),
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

#[derive(Debug)]
pub struct Use<'a> {
    pub path: Path<'a>,
}
