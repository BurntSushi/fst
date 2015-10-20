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

/// IntoStream describes types that can be converted to streams.
///
/// This is analogous to the `IntoIterator` trait for `Iterator` in
/// `std::iter`.
pub trait IntoStream<'a> {
    type Item: 'a;
    type Into: Stream<'a, Item=Self::Item>;

    fn into_stream(self) -> Self::Into;
}

impl<'a, S: Stream<'a>> IntoStream<'a> for S {
    type Item = S::Item;
    type Into = S;

    fn into_stream(self) -> S {
        self
    }
}
