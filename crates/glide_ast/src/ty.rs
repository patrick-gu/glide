use glide_span::Span;

#[derive(Debug)]
pub struct Ty<'a> {
    pub name: Span<'a>,
    pub generics: Vec<Ty<'a>>,
}
