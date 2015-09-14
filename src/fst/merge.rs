use fst::build::Builder;
use fst::Fst;

pub fn merge<B, W>(bfst: &mut Builder<W>, fsts: &[Fst<B>])
        where B: AsRef<[u8]>, W: io::Write {
}
