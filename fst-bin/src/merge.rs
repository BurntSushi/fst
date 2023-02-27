use std::cmp;
use std::env;
use std::fs::{self, File};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::thread;

use bstr::BString;
use crossbeam_channel as chan;
use fst::{self, raw, Streamer};
use tempfile;

use crate::util;
use crate::Error;

pub struct Merger<I> {
    it: I,
    output: PathBuf,
    value_merger: Option<Arc<dyn Fn(u64, u64) -> u64 + Send + Sync + 'static>>,
    fd_limit: u32,
    batch_size: u32,
    threads: u32,
    tmp_dir: PathBuf,
    keep_tmp_dir: bool,
}

type KV = (BString, u64);

impl<I> Merger<I>
where
    I: Iterator<Item = Result<(BString, u64), Error>> + Send + 'static,
{
    pub fn new<T, P>(it: T, output: P) -> Merger<I>
    where
        P: AsRef<Path>,
        T: IntoIterator<IntoIter = I, Item = I::Item>,
    {
        Merger {
            it: it.into_iter(),
            output: output.as_ref().to_path_buf(),
            value_merger: None,
            fd_limit: 5,
            batch_size: 100_000,
            threads: std::thread::available_parallelism()
                .map_or(1, |x| x.get() as u32),
            tmp_dir: env::temp_dir(),
            keep_tmp_dir: false,
        }
    }

    pub fn value_merger<F>(mut self, f: F) -> Merger<I>
    where
        F: Fn(u64, u64) -> u64 + Send + Sync + 'static,
    {
        self.value_merger = Some(Arc::new(f));
        self
    }

    pub fn fd_limit(mut self, fd_limit: u32) -> Merger<I> {
        self.fd_limit = fd_limit;
        self
    }

    pub fn batch_size(mut self, batch_size: u32) -> Merger<I> {
        self.batch_size = batch_size;
        self
    }

    pub fn threads(mut self, threads: u32) -> Merger<I> {
        self.threads = threads;
        self
    }

    pub fn tmp_dir<P: AsRef<Path>>(mut self, path: P) -> Merger<I> {
        self.tmp_dir = path.as_ref().to_path_buf();
        self
    }

    pub fn keep_tmp_dir(mut self, yes: bool) -> Merger<I> {
        self.keep_tmp_dir = yes;
        self
    }

    pub fn merge(self) -> Result<(), Error> {
        let tmp_dir = tempfile::Builder::new()
            .prefix("rust-fst")
            .tempdir_in(&self.tmp_dir)?;
        let tmp_dir_path = Arc::new(tmp_dir.path().to_path_buf());

        // Do the initial round of creating FSTs for every batch in the input.
        let batches = batcher(self.it, self.batch_size, self.threads);
        let sorters = Sorters::new(self.threads);
        for (i, kv_batch) in batches.into_iter().enumerate() {
            sorters.create_fst(KvBatch {
                tmp_dir: tmp_dir_path.clone(),
                index: i,
                kvs: kv_batch?,
            });
        }
        let mut results = sorters.results();

        // Nothing? Create an empty FST and be done with it.
        if results.is_empty() {
            let wtr = io::BufWriter::new(File::create(&self.output)?);
            let builder = raw::Builder::new(wtr)?;
            builder.finish()?;
            return Ok(());
        }

        // Now union batches of FSTs until only one remains.
        // That one is our final FST with all key/values.
        let mut gen = 0;
        while results.len() > 1 {
            let batches = batcher(results, self.fd_limit, self.threads);
            let sorters = Sorters::new(self.threads);
            for (i, union_batch) in batches.into_iter().enumerate() {
                sorters.create_fst(UnionBatch {
                    tmp_dir: tmp_dir_path.clone(),
                    gen,
                    index: i,
                    fsts: union_batch?,
                    value_merger: self.value_merger.clone(),
                });
            }
            results = sorters.results();
            gen += 1;
        }
        assert_eq!(results.len(), 1);
        fs::copy(results.pop().unwrap()?, &self.output)?;
        if self.keep_tmp_dir {
            drop(tmp_dir.into_path());
        }
        Ok(())
    }
}

fn batcher<I, T, IT>(
    it: I,
    batch_size: u32,
    threads: u32,
) -> chan::Receiver<Result<Vec<T>, Error>>
where
    T: Send + 'static,
    IT: Iterator<Item = Result<T, Error>> + Send + 'static,
    I: IntoIterator<IntoIter = IT, Item = IT::Item> + Send + 'static,
{
    let batch_size = batch_size as usize;
    let (send, recv) = chan::bounded(cmp::min(1, threads as usize / 3));
    let it = it.into_iter();
    thread::spawn(move || {
        let mut batch = Vec::with_capacity(batch_size);
        for item in it {
            match item {
                Err(err) => {
                    send.send(Err(From::from(err))).unwrap();
                    return;
                }
                Ok(item) => {
                    batch.push(item);
                    if batch.len() >= batch_size {
                        send.send(Ok(batch)).unwrap();
                        batch = Vec::with_capacity(batch_size);
                    }
                }
            }
        }
        if !batch.is_empty() {
            send.send(Ok(batch)).unwrap();
        }
    });
    recv
}

struct Sorters<B> {
    send: chan::Sender<B>,
    results: chan::Receiver<Vec<Result<PathBuf, Error>>>,
}

impl<B: Batchable + Send + 'static> Sorters<B> {
    fn new(threads: u32) -> Sorters<B> {
        let (bsend, brecv) = chan::bounded::<B>(0);
        let (rsend, rrecv) = chan::bounded(0);
        for _ in 0..threads {
            let brecv = brecv.clone();
            let rsend = rsend.clone();
            thread::spawn(move || {
                let mut results = vec![];
                for mut batch in brecv {
                    results.push(batch.create_fst());
                }
                rsend.send(results).unwrap();
            });
        }
        Sorters { send: bsend, results: rrecv }
    }

    fn create_fst(&self, batch: B) {
        self.send.send(batch).unwrap();
    }

    fn results(self) -> Vec<Result<PathBuf, Error>> {
        drop(self.send);
        let mut results = vec![];
        for rs in self.results {
            results.extend(rs);
        }
        results
    }
}

trait Batchable {
    fn create_fst(&mut self) -> Result<PathBuf, Error>;
}

struct KvBatch {
    tmp_dir: Arc<PathBuf>,
    index: usize,
    kvs: Vec<KV>,
}

impl Batchable for KvBatch {
    fn create_fst(&mut self) -> Result<PathBuf, Error> {
        self.kvs.sort();
        self.kvs.dedup();
        let file_name = format!("batch{}", self.index);
        let path = self.tmp_dir.join(file_name).to_path_buf();
        let wtr = io::BufWriter::new(File::create(&path)?);
        let mut builder = raw::Builder::new(wtr)?;
        for &(ref k, v) in &self.kvs {
            match builder.insert(k, v) {
                Ok(_) => {}
                Err(fst::Error::Fst(fst::raw::Error::DuplicateKey {
                    ..
                })) => {}
                Err(err) => return Err(From::from(err)),
            }
        }
        builder.finish()?;
        Ok(path)
    }
}

struct UnionBatch {
    tmp_dir: Arc<PathBuf>,
    gen: usize,
    index: usize,
    fsts: Vec<PathBuf>,
    value_merger: Option<Arc<dyn Fn(u64, u64) -> u64 + Send + Sync + 'static>>,
}

impl Batchable for UnionBatch {
    fn create_fst(&mut self) -> Result<PathBuf, Error> {
        let file_name = format!("union-gen{}-batch{}", self.gen, self.index);
        let path = self.tmp_dir.join(file_name).to_path_buf();
        let wtr = io::BufWriter::new(File::create(&path)?);
        let mut builder = raw::Builder::new(wtr)?;

        let mut fsts = vec![];
        for path in &self.fsts {
            fsts.push(unsafe { util::mmap_fst(path)? });
        }
        let mut union = fsts.iter().collect::<raw::OpBuilder>().union();
        while let Some((key, outputs)) = union.next() {
            let mut merged = 0;
            if let Some(ref value_merger) = self.value_merger {
                merged = outputs
                    .iter()
                    .fold(0, |merged, iv| value_merger(merged, iv.value));
            }
            builder.insert(key, merged)?;
        }
        builder.finish()?;
        Ok(path)
    }
}
