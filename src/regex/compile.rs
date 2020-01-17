use super::regex_syntax::{CharClass, ClassRange, Expr, Repeater};
use super::utf8_ranges::{Utf8Sequence, Utf8Sequences};
use super::Error;
use super::Inst;

pub struct Compiler {
    size_limit: usize,
    insts: Vec<Inst>,
}

impl Compiler {
    pub fn new(size_limit: usize) -> Compiler {
        Compiler {
            size_limit: size_limit,
            insts: vec![],
        }
    }

    pub fn compile(mut self, ast: &Expr) -> Result<Vec<Inst>, Error> {
        self.c(ast)?;
        self.insts.push(Inst::Match);
        Ok(self.insts)
    }

    fn c(&mut self, ast: &Expr) -> Result<(), Error> {
        match *ast {
            Expr::StartLine | Expr::EndLine | Expr::StartText | Expr::EndText => {
                return Err(From::from(Error::NoEmpty))
            }
            Expr::WordBoundary
            | Expr::NotWordBoundary
            | Expr::WordBoundaryAscii
            | Expr::NotWordBoundaryAscii => {
                return Err(From::from(Error::NoWordBoundary));
            }
            Expr::LiteralBytes { .. }
            | Expr::AnyByte
            | Expr::AnyByteNoNL
            | Expr::ClassBytes(..) => {
                return Err(From::from(Error::NoBytes));
            }
            Expr::Empty => {}
            Expr::Literal { ref chars, casei } => {
                for &c in chars {
                    if casei {
                        self.c(&Expr::Class(
                            CharClass::new(vec![ClassRange { start: c, end: c }]).case_fold(),
                        ))?;
                    } else {
                        // One scalar value, so we're guaranteed to get a
                        // single byte sequence.
                        for seq in Utf8Sequences::new(c, c) {
                            self.compile_utf8_ranges(&seq);
                        }
                    }
                }
            }
            Expr::AnyChar => self.c(&Expr::Class(CharClass::new(vec![ClassRange {
                start: '\u{0}',
                end: '\u{10FFFF}',
            }])))?,
            Expr::AnyCharNoNL => self.c(&Expr::Class(CharClass::new(vec![
                ClassRange {
                    start: '\u{0}',
                    end: '\u{09}',
                },
                ClassRange {
                    start: '\u{0B}',
                    end: '\u{10FFFF}',
                },
            ])))?,
            Expr::Class(ref cls) => {
                self.compile_class(cls)?;
            }
            Expr::Group { ref e, .. } => self.c(e)?,
            Expr::Concat(ref es) => {
                for e in es {
                    self.c(e)?;
                }
            }
            Expr::Alternate(ref es) => {
                if es.len() == 0 {
                    return Ok(());
                }
                let mut jmps_to_end = vec![];
                for e in &es[0..es.len() - 1] {
                    let split = self.empty_split();
                    let j1 = self.insts.len();
                    self.c(e)?;
                    jmps_to_end.push(self.empty_jump());
                    let j2 = self.insts.len();
                    self.set_split(split, j1, j2);
                }
                self.c(&es[es.len() - 1])?;
                let end = self.insts.len();
                for jmp_to_end in jmps_to_end {
                    self.set_jump(jmp_to_end, end);
                }
            }
            Expr::Repeat { greedy: false, .. } => {
                return Err(Error::NoLazy.into());
            }
            Expr::Repeat {
                ref e,
                r: Repeater::ZeroOrOne,
                ..
            } => {
                let split = self.empty_split();
                let j1 = self.insts.len();
                self.c(e)?;
                let j2 = self.insts.len();
                self.set_split(split, j1, j2);
            }
            Expr::Repeat {
                ref e,
                r: Repeater::ZeroOrMore,
                ..
            } => {
                let j1 = self.insts.len();
                let split = self.empty_split();
                let j2 = self.insts.len();
                self.c(e)?;
                let jmp = self.empty_jump();
                let j3 = self.insts.len();

                self.set_jump(jmp, j1);
                self.set_split(split, j2, j3);
            }
            Expr::Repeat {
                ref e,
                r: Repeater::OneOrMore,
                ..
            } => {
                let j1 = self.insts.len();
                self.c(e)?;
                let split = self.empty_split();
                let j2 = self.insts.len();
                self.set_split(split, j1, j2);
            }
            Expr::Repeat {
                ref e,
                r: Repeater::Range { min, max: None },
                ..
            } => {
                for _ in 0..min {
                    self.c(e)?;
                }
                self.c(&Expr::Repeat {
                    e: e.clone(),
                    r: Repeater::ZeroOrMore,
                    greedy: true,
                })?;
            }
            Expr::Repeat {
                ref e,
                r:
                    Repeater::Range {
                        min,
                        max: Some(max),
                    },
                ..
            } => {
                for _ in 0..min {
                    self.c(e)?;
                }
                let (mut splits, mut starts) = (vec![], vec![]);
                for _ in min..max {
                    splits.push(self.empty_split());
                    starts.push(self.insts.len());
                    self.c(e)?;
                }
                let end = self.insts.len();
                for (split, start) in splits.into_iter().zip(starts) {
                    self.set_split(split, start, end);
                }
            }
        }
        self.check_size()
    }

    fn compile_class(&mut self, class: &CharClass) -> Result<(), Error> {
        if class.is_empty() {
            return Ok(());
        }
        let mut jmps = vec![];
        for r in &class[0..class.len() - 1] {
            let split = self.empty_split();
            let j1 = self.insts.len();
            self.compile_class_range(r)?;
            jmps.push(self.empty_jump());
            let j2 = self.insts.len();
            self.set_split(split, j1, j2);
        }
        self.compile_class_range(&class[class.len() - 1])?;
        let end = self.insts.len();
        for jmp in jmps {
            self.set_jump(jmp, end);
        }
        Ok(())
    }

    fn compile_class_range(&mut self, char_range: &ClassRange) -> Result<(), Error> {
        let mut it = Utf8Sequences::new(char_range.start, char_range.end).peekable();
        let mut jmps = vec![];
        let mut utf8_ranges = it.next().expect("non-empty char class");
        while it.peek().is_some() {
            let split = self.empty_split();
            let j1 = self.insts.len();
            self.compile_utf8_ranges(&utf8_ranges);
            jmps.push(self.empty_jump());
            let j2 = self.insts.len();
            self.set_split(split, j1, j2);
            utf8_ranges = it.next().unwrap(); // because peek says so
        }
        self.compile_utf8_ranges(&utf8_ranges);
        let end = self.insts.len();
        for jmp in jmps {
            self.set_jump(jmp, end);
        }
        Ok(())
    }

    fn compile_utf8_ranges(&mut self, utf8_ranges: &Utf8Sequence) {
        for r in utf8_ranges {
            self.push(Inst::Range(r.start, r.end));
        }
    }

    fn check_size(&self) -> Result<(), Error> {
        use std::mem::size_of;

        if self.insts.len() * size_of::<Inst>() > self.size_limit {
            Err(Error::CompiledTooBig(self.size_limit).into())
        } else {
            Ok(())
        }
    }

    /// Appends the given instruction to the program.
    #[inline]
    fn push(&mut self, x: Inst) {
        self.insts.push(x)
    }

    /// Appends an *empty* `Split` instruction to the program and returns
    /// the index of that instruction. (The index can then be used to "patch"
    /// the actual locations of the split in later.)
    #[inline]
    fn empty_split(&mut self) -> usize {
        self.insts.push(Inst::Split(0, 0));
        self.insts.len() - 1
    }

    /// Sets the left and right locations of a `Split` instruction at index
    /// `i` to `pc1` and `pc2`, respectively.
    /// If the instruction at index `i` isn't a `Split` instruction, then
    /// `panic!` is called.
    #[inline]
    fn set_split(&mut self, i: usize, pc1: usize, pc2: usize) {
        let split = &mut self.insts[i];
        match *split {
            Inst::Split(_, _) => *split = Inst::Split(pc1, pc2),
            _ => panic!("BUG: Invalid split index."),
        }
    }

    /// Appends an *empty* `Jump` instruction to the program and returns the
    /// index of that instruction.
    #[inline]
    fn empty_jump(&mut self) -> usize {
        self.insts.push(Inst::Jump(0));
        self.insts.len() - 1
    }

    /// Sets the location of a `Jump` instruction at index `i` to `pc`.
    /// If the instruction at index `i` isn't a `Jump` instruction, then
    /// `panic!` is called.
    #[inline]
    fn set_jump(&mut self, i: usize, pc: usize) {
        let jmp = &mut self.insts[i];
        match *jmp {
            Inst::Jump(_) => *jmp = Inst::Jump(pc),
            _ => panic!("BUG: Invalid jump index."),
        }
    }
}
