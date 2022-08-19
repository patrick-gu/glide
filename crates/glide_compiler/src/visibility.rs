#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug)]
pub(crate) enum Visibility {
    Private,
    Public,
}
