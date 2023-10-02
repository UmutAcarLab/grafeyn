use std::alloc::{alloc, dealloc, Layout};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use crate::types::{BasisIdx, Complex};
use crate::utility;

use std::sync::{atomic::AtomicU64, atomic::Ordering};

use super::SparseStateTableInserion;

pub struct HeapArray<T> {
    ptr: *mut T,
    len: usize,
}

impl<T> HeapArray<T> {
    pub fn new(len: usize) -> Self {
        let ptr = unsafe {
            let layout = Layout::from_size_align_unchecked(len, std::mem::size_of::<T>());
            alloc(layout) as *mut T
        };
        Self { ptr, len }
    }
    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx < self.len {
            unsafe { Some(&*(self.ptr.add(idx))) }
        } else {
            None
        }
    }
    pub fn get_mut(&self, idx: usize) -> Option<&mut T> {
        if idx < self.len {
            unsafe { Some(&mut *(self.ptr.add(idx))) }
        } else {
            None
        }
    }
    pub fn len(&self) -> usize {
        self.len
    }
}

impl<T> Drop for HeapArray<T> {
    fn drop(&mut self) {
        unsafe {
            dealloc(
                self.ptr as *mut u8,
                Layout::from_size_align_unchecked(self.len, std::mem::size_of::<T>()),
            )
        };
    }
}

impl<T> std::ops::Index<usize> for HeapArray<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}
impl<T> std::ops::IndexMut<usize> for HeapArray<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

const EMPTY_KEY: BasisIdx = BasisIdx::flip_unsafe(&BasisIdx::from_u64(0), 63);

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

pub struct ConcurrentSparseStateTable {
    keys: HeapArray<AtomicU64>,
    packed_weights: HeapArray<AtomicU64>,
}

unsafe impl Sync for ConcurrentSparseStateTable {}

impl ConcurrentSparseStateTable {
    pub fn new2(capacity: usize) -> Self {
        let mut keys = HeapArray::<AtomicU64>::new(capacity);
        for i in 0..capacity {
            keys[i] = AtomicU64::new(0);
        }
        let mut packed_weights = HeapArray::<AtomicU64>::new(2 * capacity);
        for i in 0..(2 * capacity) {
            packed_weights[i] = AtomicU64::new(0);
        }
        Self {
            keys,
            packed_weights,
        }
    }
    pub fn new() -> Self {
        let capacity = 1000;
        Self::new2(capacity)
    }
    pub fn capacity(&self) -> usize {
        self.keys.len
    }
    fn put_value_at(&self, i: usize, v: Complex) {
        let k = 2 * i;
        loop {
            let old: u64 = self.packed_weights[k].load(Ordering::Relaxed);
            let (re, im) = utility::unpack_complex(old);
            let new = utility::pack_complex(re + v.re, im + v.im);
            match self.packed_weights[k].compare_exchange(
                old,
                new,
                Ordering::SeqCst,
                Ordering::Acquire,
            ) {
                Ok(_) => return,
                Err(_) => (),
            }
        }
    }
    pub fn insert_add_weights_limit_probes(
        &self,
        tolerance: usize,
        x: BasisIdx,
        v: Complex,
    ) -> SparseStateTableInserion {
        let n = self.keys.len;
        let mut probes: usize = 0;
        let mut i: usize = calculate_hash(&x) as usize;
        let y = x.into_u64();
        loop {
            if probes >= tolerance {
                return SparseStateTableInserion::Full;
            }
            if i >= n {
                i = 0;
                continue;
            }
            let k = self.keys[i].load(Ordering::Relaxed);
            if k == BasisIdx::into_u64(EMPTY_KEY) {
                match self.keys[i].compare_exchange(k, y, Ordering::SeqCst, Ordering::Acquire) {
                    Ok(_) => {
                        self.put_value_at(i, v);
                        break;
                    }
                    Err(_) => continue,
                }
            } else if k == y {
                self.put_value_at(i, v);
                break;
            } else {
                i = i + 1;
                probes = probes + 1;
            }
        }
        SparseStateTableInserion::Success
    }
    pub fn lookup(&self, x: BasisIdx) -> Option<Complex> {
        let n = self.keys.len;
        let mut i: usize = calculate_hash(&x) as usize;
        let y = x.into_u64();
        loop {
            let k = self.keys[i].load(Ordering::Relaxed);
            if k == BasisIdx::into_u64(EMPTY_KEY) {
                return None;
            } else if k == y {
                let old: u64 = self.packed_weights[2 * i].load(Ordering::Relaxed);
                let (re, im) = utility::unpack_complex(old);
                return Some(Complex::new(re, im));
            } else {
                i = i + 1
            }
        }
    }
    pub fn increase_capacity_by_factor(&self, alpha: f32) -> Self {
        let new_capacity = (alpha * self.keys.len as f32).ceil() as usize;
        let mut new_table = Self::new2(new_capacity);
        for i in 0..self.keys.len {
            let k = BasisIdx::from_u64(new_table.keys[i].load(Ordering::Relaxed));
            let (re, im) =
                utility::unpack_complex(self.packed_weights[2 * i].load(Ordering::Relaxed));
            let c = Complex::new(re, im);
            self.insert_add_weights_limit_probes(1, k, c);
        }
        new_table
    }
}

#[derive(Debug)]
pub struct SparseStateTable {
    pub table: HashMap<BasisIdx, Complex>,
}

impl SparseStateTable {
    pub fn singleton(bidx: BasisIdx, weight: Complex) -> Self {
        Self {
            table: HashMap::from([(bidx, weight)]),
        }
    }

    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }

    pub fn num_nonzeros(&self) -> usize {
        self.table
            .iter()
            .filter(|(_, w)| utility::is_nonzero(**w))
            .count()
    }

    pub fn put(&mut self, bidx: BasisIdx, weight: Complex) {
        self.table
            .entry(bidx)
            .and_modify(|w| *w += weight)
            .or_insert(weight);
    }

    /*
    #[cfg(test)]
    pub fn get(&self, bidx: &BasisIdx) -> Option<&Complex> {
    self.table.get(&bidx)
    } */

    pub fn get(&self, bidx: &BasisIdx) -> Option<Complex> {
        self.table.get(&bidx).map(Clone::clone)
    }
}
