use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, Ordering};
use crate::alloca::ptr;
use crate::alloca::arena::Arena3;

#[derive(Debug)]
pub(crate) struct HedgeArena {
  start: ptr,
  cur: ptr,
  size: usize, // = 32x
  
  span_start: ptr,
  
  // gray: Box<[u8]>,
  // black: Box<[u8]>,
  
  live: AtomicBool, // хз пока нужен ли, мб оставлю только map, юуду смотреть на None
  
  /// dbg only
  objects: Vec<usize>,
}

impl Arena3 for HedgeArena {
  fn new(start: ptr, size: usize) -> Self {
    Self {
      start,
      cur: start,
      size,
      span_start: start + (size >> 5),
      live: Default::default(),
      objects: vec![]
    }
  }
  
  fn start(&self) -> ptr {
    self.start
  }
  
  fn cur(&self) -> ptr {
    self.cur
  }
  
  fn add(&mut self, size: usize) {
    self.cur += size
  }
  
  fn size(&self) -> usize {
    self.size
  }
  
  fn how_much(&self) -> usize {
    self.start + self.size - self.cur
  }
  
  fn gray_map(&self) -> (ptr, usize) {
    (self.start, self.size >> 6)
  }
  
  fn black_map(&self) -> (ptr, usize) {
    (self.start + self.size >> 6, self.size >> 6)
  }
  
  fn clear_mark(&self) {
    let size_mark = self.span_start - self.start;
    assert!(size_mark > 0);
    unsafe {
      std::ptr::write_bytes(self.start as *mut u8, 0, size_mark as usize);
    }
  }
  
  fn live(&self) -> bool {
    self.live.load(Ordering::Relaxed)
  }
  
  fn on(&mut self) {
    self.live.store(true, Ordering::Relaxed);
    
    unsafe {
      let res = libc::mmap(self.start as *mut c_void, self.size,
                           libc::PROT_READ | libc::PROT_WRITE,
                           libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_FIXED,
                           -1, 0);
      assert_ne!(res, libc::MAP_FAILED);
    }
    unsafe { *(self.start as *mut u8) = 1 };
    self.clear_mark();
  }
  
  fn off(&mut self) {
    self.live.store(false, Ordering::Relaxed);
  }
  
  fn mark_gray(&mut self, ptr: ptr) {
    let offset = ptr - self.span_start;
    let index = offset >> 3;
    unsafe {
      std::ptr::write((self.start + (index >> 3)) as *mut u8, (index % 8) as u8);
    }
  }
  
  fn mark_black(&mut self, ptr: ptr) {
    let offset = ptr - self.span_start;
    let index = offset >> 3;
    unsafe {
      std::ptr::write(((self.start + self.span_start >> 1) + (index >> 3)) as *mut u8, (index % 8) as u8);
    }
  }
}

impl Eq for HedgeArena {}

impl PartialEq<Self> for HedgeArena {
  fn eq(&self, other: &Self) -> bool {
    false
  }
}

impl PartialOrd<Self> for HedgeArena {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for HedgeArena {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    if self.how_much() == other.how_much() {
      self.start().cmp(&other.start())
    } else {
      self.how_much().cmp(&other.how_much())
    }
  }
}