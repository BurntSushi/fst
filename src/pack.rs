use std::cmp;
use std::io;
use std::mem;

use byteorder::{ReadBytesExt, LittleEndian};

pub struct Packer {
    nbytes: u8,
}

impl Packer {
    pub fn for_range(min: u64, max: u64) -> Packer {
        if max < min {
            Packer { nbytes: 0 }
        } else {
            Packer { nbytes: cmp::max(bytes_needed(min), bytes_needed(max)) }
        }
    }

    pub fn for_num_bytes(nbytes: u8) -> Packer {
        Packer { nbytes: nbytes }
    }

    pub fn bytes_needed(&self) -> u8 {
        self.nbytes
    }

    pub fn read<R: io::Read>(&self, mut rdr: R) -> io::Result<u64> {
        Ok(try!(rdr.read_uint::<LittleEndian>(self.nbytes as usize)))
    }

    pub fn write<W: io::Write>(&self, mut wtr: W, n: u64) -> io::Result<()> {
        let n: [u8; 8] = unsafe { mem::transmute(n.to_le()) };
        wtr.write_all(&n[0..self.nbytes as usize])
    }
}

fn bytes_needed(n: u64) -> u8 {
    if n < 1 << 8 as u64 {
        1
    } else if n < 1 << 16 as u64 {
        2
    } else if n < 1 << 24 as u64 {
        3
    } else if n < 1 << 32 as u64 {
        4
    } else if n < 1 << 40 as u64 {
        5
    } else if n < 1 << 48 as u64 {
        6
    } else if n < 1 << 56 as u64 {
        7
    } else {
        8
    }
}

#[cfg(test)]
mod tests {
    use std::io;

    use quickcheck::{QuickCheck, StdGen, TestResult};

    use super::Packer;

    #[test]
    fn prop_pack_in_out() {
        fn p(nums: Vec<u64>) -> TestResult {
            if nums.is_empty() {
                return TestResult::discard();
            }
            let min = *nums.iter().min().unwrap();
            let max = *nums.iter().max().unwrap();
            let packer = Packer::for_range(min, max);

            let mut buf = io::Cursor::new(vec![]);
            for &n in &nums {
                packer.write(&mut buf, n).unwrap();
            }
            buf.set_position(0);
            for &n1 in &nums {
                let n2 = packer.read(&mut buf).unwrap();
                if n1 != n2 {
                    return TestResult::failed();
                }
            }
            TestResult::passed()
        }
        QuickCheck::new()
            .gen(StdGen::new(::rand::thread_rng(), 257)) // pick byte boundary
            .quickcheck(p as fn(Vec<u64>) -> TestResult);
    }
}
