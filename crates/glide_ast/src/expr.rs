use glide_span::Span;

#[derive(Debug)]
pub enum Expr<'a> {
    Integer(Span<'a>),
    String { data: Vec<u8> },
    Var(Span<'a>),
    Call(Call<'a>),
}

#[derive(Debug)]
pub struct Call<'a> {
    pub receiver: Box<Expr<'a>>,
    pub args: Vec<Expr<'a>>,
}
