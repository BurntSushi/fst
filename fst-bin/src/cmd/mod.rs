pub mod csv;
pub mod dot;
pub mod dupes;
pub mod fuzzy;
pub mod grep;
pub mod hamming;
pub mod map;
pub mod node;
pub mod range;
pub mod rust;
pub mod set;
pub mod union;

// If compile times become unruly, comment out unused modules above and use
// the following macro to satisfying the compiler.
// macro_rules! unused {
    // ($($name:ident),*) => {
        // $(
            // pub mod $name {
                // pub fn run(_: Vec<String>) -> Result<(), ::Error> {
                    // unimplemented!()
                // }
            // }
        // )*
    // }
// }
//
// unused! { csv, dot, fuzzy, grep, map, union }
