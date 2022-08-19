pub mod def;
pub mod expr;
pub mod path;
pub mod stmt;
pub mod ty;

use crate::def::Def;

#[derive(Debug)]
pub struct Package<'a> {
    pub defs: Vec<Def<'a>>,
}
