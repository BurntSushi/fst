/// Stream is a streaming iterator.
///
/// It provides a mechanism for writing code that is generic over streams
/// produced by this crate.
///
/// Note that this is strictly less useful than `Iterator` because the item
/// associated type is bound to a specific lifetime. However, this does permit
/// us to write *some* generic code over iterators that produce values tied
/// to the lifetime of the iterator.
pub trait Stream<'a> {
    type Item: 'a;

    fn next(&'a mut self) -> Option<Self::Item>;
}
