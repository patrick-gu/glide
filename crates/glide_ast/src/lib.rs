pub mod def;
pub mod expr;
pub mod ty;

use crate::def::Def;

#[derive(Debug)]
pub struct Ast<'a> {
    pub defs: Vec<Def<'a>>,
}
