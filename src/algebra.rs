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
/// usable as inputs to the finite state machines of this library.
pub trait AbelianGroup {
    /// The identity value of the abelian group.
    fn identity() -> Self;

    /// The addition operation of the abelian group.
    fn plus(self, other: Self) -> Self;

    /// The subtraction operation of the abelian group.
    fn minus(self, other: Self) -> Self;
}

impl AbelianGroup for u8 {
    fn identity() -> u8 { 0 }

    fn plus(self, other: u8) -> u8 { self + other }

    fn minus(self, other: u8) -> u8 { self - other }
}

impl AbelianGroup for u16 {
    fn identity() -> u16 { 0 }

    fn plus(self, other: u16) -> u16 { self + other }

    fn minus(self, other: u16) -> u16 { self - other }
}

impl AbelianGroup for u32 {
    fn identity() -> u32 { 0 }

    fn plus(self, other: u32) -> u32 { self + other }

    fn minus(self, other: u32) -> u32 { self - other }
}

impl AbelianGroup for u64 {
    fn identity() -> u64 { 0 }

    fn plus(self, other: u64) -> u64 { self + other }

    fn minus(self, other: u64) -> u64 { self - other }
}

impl AbelianGroup for usize {
    fn identity() -> usize { 0 }

    fn plus(self, other: usize) -> usize { self + other }

    fn minus(self, other: usize) -> usize { self - other }
}

impl AbelianGroup for i8 {
    fn identity() -> i8 { 0 }

    fn plus(self, other: i8) -> i8 { self + other }

    fn minus(self, other: i8) -> i8 { self - other }
}

impl AbelianGroup for i16 {
    fn identity() -> i16 { 0 }

    fn plus(self, other: i16) -> i16 { self + other }

    fn minus(self, other: i16) -> i16 { self - other }
}

impl AbelianGroup for i32 {
    fn identity() -> i32 { 0 }

    fn plus(self, other: i32) -> i32 { self + other }

    fn minus(self, other: i32) -> i32 { self - other }
}

impl AbelianGroup for i64 {
    fn identity() -> i64 { 0 }

    fn plus(self, other: i64) -> i64 { self + other }

    fn minus(self, other: i64) -> i64 { self - other }
}

impl AbelianGroup for isize {
    fn identity() -> isize { 0 }

    fn plus(self, other: isize) -> isize { self + other }

    fn minus(self, other: isize) -> isize { self - other }
}

impl AbelianGroup for f32 {
    fn identity() -> f32 { 0.0 }

    fn plus(self, other: f32) -> f32 { self + other }

    fn minus(self, other: f32) -> f32 { self - other }
}

impl AbelianGroup for f64 {
    fn identity() -> f64 { 0.0 }

    fn plus(self, other: f64) -> f64 { self + other }

    fn minus(self, other: f64) -> f64 { self - other }
}
