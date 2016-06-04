use std::num::Wrapping;

/// An abelian group; that is, a type `T` with the
/// [functionally pure](https://en.wikipedia.org/wiki/Pure_function)
/// methods `identity`, `plus`, and `minus` such that the following
/// equations are obeyed:
///
/// - for all `x: T`, `y: T`, and `z: T`, `x.plus(y).plus(z) = x.plus(y.plus(z))`
/// - for all `x: T` and `y: T`, `x.plus(y) = y.plus(x)`
/// - for all `x: T`, `x.plus(T::identity()) = T::identity().plus(x) = x.minus(T::identity()) = x`
/// - for all `x: T` and `y: T`, `x.plus(y).minus(y) = x.minus(y).plus(y) = x`
///
/// While this trait is already implemented for the common numeric types,
/// it can also be implemented for other types in order to make those types
/// usable as outputs from the finite state machines of this library.
pub trait AbelianGroup {
    /// The identity value of the abelian group.
    fn identity() -> Self;

    /// The addition operation of the abelian group.
    fn plus(self, other: Self) -> Self;

    /// The subtraction operation of the abelian group.
    fn minus(self, other: Self) -> Self;
}

macro_rules! numeric_abelian_group {
    ($t:ty, $i:expr) => {
        impl $crate::algebra::AbelianGroup for $t {
            fn identity() -> $t { $i }

            fn plus(self, other: $t) -> $t { self + other }

            fn minus(self, other: $t) -> $t { self - other }
        }
    }
}
numeric_abelian_group!(u8,0);
numeric_abelian_group!(u16,0);
numeric_abelian_group!(u32,0);
numeric_abelian_group!(u64,0);
numeric_abelian_group!(usize,0);
numeric_abelian_group!(i8,0);
numeric_abelian_group!(i16,0);
numeric_abelian_group!(i32,0);
numeric_abelian_group!(i64,0);
numeric_abelian_group!(isize,0);
numeric_abelian_group!(f32,0.0);
numeric_abelian_group!(f64,0.0);
numeric_abelian_group!(Wrapping<u8>,Wrapping(0));
numeric_abelian_group!(Wrapping<u16>,Wrapping(0));
numeric_abelian_group!(Wrapping<u32>,Wrapping(0));
numeric_abelian_group!(Wrapping<u64>,Wrapping(0));
numeric_abelian_group!(Wrapping<usize>,Wrapping(0));
numeric_abelian_group!(Wrapping<i8>,Wrapping(0));
numeric_abelian_group!(Wrapping<i16>,Wrapping(0));
numeric_abelian_group!(Wrapping<i32>,Wrapping(0));
numeric_abelian_group!(Wrapping<i64>,Wrapping(0));
numeric_abelian_group!(Wrapping<isize>,Wrapping(0));
