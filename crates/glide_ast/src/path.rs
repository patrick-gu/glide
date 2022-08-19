use glide_span::Span;

#[derive(Debug)]
pub struct Path<'a> {
    pub package: Option<Span<'a>>,
    pub element: Span<'a>,
}
