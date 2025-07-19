use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};

pub(crate) type ptr = usize;

pub trait Object {
  fn size(&self) -> usize;
  fn get_bitset_of_ref(&self) -> &'static [u8];
}

pub trait ArenaAllocator3<T: Object> {
  const SIZE: usize;
  
  const LOG_BLOCK_SIZE:usize;
  
  fn new(max_size: usize) -> Self;
  
  fn alloc(&mut self, o: &T) -> *mut u8;
  
  fn mark_white(&mut self);
  
  fn arena_by_ptr(&self, ptr: usize) -> dyn Arena3<T>;
  
  fn mark_gray(&mut self, ptr: ptr);
  
  fn mark_black(&mut self, ptr: ptr);
}

pub trait Arena3<T: Object> {
  fn new(start: ptr, size: usize) -> Self;
  
  fn gray_map(&self) ->(ptr, usize);
  
  fn black_map(&self) ->(ptr, usize);
}

pub enum AllocateError {
  OutOfMemory,
}


pub struct Heap<T: Ord> {
  items: Mutex<std::collections::BTreeSet<Arc<T>>>
}

impl<T: Ord> Heap<T> {
  pub fn new() -> Self {
    Self {items: Mutex::new(std::collections::BTreeSet::new())}
  }
  
  pub fn insert(&mut self, item: Arc<T>) {
    self.items.lock().unwrap().insert(item);
  }
  
  pub fn del(&mut self, item: Arc<T>) {
    self.items.lock().unwrap().remove(&item);
  }
  
  pub fn lower_bound(&self, n: usize) -> Arc<T> {
    Arc::new(self.items
        .lock()
        .unwrap()
        .range(n..)
        .next()
        .unwrap())
  }
}

pub struct HAllocator {
  start: ptr,
  max_size: usize,
  
  blocks: Box<[Option<*HedgeBlock>]>,
  large_objects: Vec<Arc<HedgeArena>>,
}

struct HedgeBlock {
  start: ptr,
  
  size: usize,
  
  log_t_size: usize,
  
  // only for find object by ref.
  // const
  items: Box<[Arc<HedgeArena>]>,
  
  // dynamic
  slots: Vec<Arc<HedgeArena>>,
}

impl HedgeBlock {
  fn new(start: ptr, size: usize, t_size: usize) -> Self {
    let count = size / t_size;
    let mut items = Vec::with_capacity(count);
    let mut slots = Vec::with_capacity(count);
    for i in 0..count {
      let new_arena = HedgeArena::new(start + i * t_size, size);
      let ptr_to_arena = Arc::new(new_arena);
      items.push(Arc::clone(&ptr_to_arena));
      slots.push(Arc::clone(&ptr_to_arena));
    }
    
    Self {
      start,
      size,
      log_t_size,
      items: items.into_boxed_slice(),
      slots,
    }
  }
  
  fn arena_by_ptr(&self, ptr: ptr) -> Arc<HedgeArena> {
    assert!(self.log_t_size);
    assert!(self.start <= ptr);
    assert!(ptr < self.start + self.size);
    Arc::clone(&self.items[(ptr - self.start) >> self.log_t_size])
  }
}

struct HedgeArena {
  start: ptr,
  cur: ptr,
  size: usize, // = 32x
  
  span_start: ptr,
  
  // gray: Box<[u8]>,
  // black: Box<[u8]>,
  
  live: AtomicBool,
  
  objects: Vec<usize>,
}

impl<T> Arena3<T> for HedgeArena {
  fn new(start: ptr, size: usize) -> Self {
    let mut new_arena = Self {
      start,
      cur: start,
      size,
      span_start: start + size >> 5,
      live: Default::default(),
      objects: vec![],
    };
    new_arena.clear_mark();
    
    new_arena
  }
  
  fn gray_map(&self) -> (ptr, usize) {
    (self.start, self.size >> 6)
  }
  
  fn black_map(&self) -> (ptr, usize) {
    (self.start + self.size >> 6, self.size >> 6)
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
      self.start.cmp(&other.start)
    } else {
      self.start.cmp(&other.start)
    }
  }
}

impl HedgeArena {
  
  fn clear_mark(&mut self) {
    let size_mark = self.span_start - self.start;
    unsafe {
      std::ptr::write_bytes(self.start as *mut u8, 0, size_mark as usize);
    }
  }
  
  fn how_much(&self) -> usize {
    self.cur + self.size - self.cur
  }
}


impl<T> ArenaAllocator3<T> for HAllocator {
  const SIZE: usize = 128usize << 30;
  const LOG_BLOCK_SIZE: usize = 26;
  
  fn new(max_size: usize) -> Self {
    
    unsafe {
      let start = libc::mmap(
        std::ptr::null_mut(),
        Self::SIZE,
        libc::PROT_READ | libc::PROT_WRITE,
        libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
        -1,
        0
      ) as ptr;
      
      if !start {
        panic!("reserve mmap failed");
      }
      
      
      Self {
        start,
        max_size,
        blocks: Box::new([None; Self::SIZE >> Self::LOG_BLOCK_SIZE]),
        large_objects: vec![],
      }
    }
    
  }
  
  fn alloc(&mut self, o: &T) -> ptr {
    todo!()
  }
  
  fn mark_white(&mut self) {
    for block in self.blocks.iter_mut() {
      match block {
        &mut Some(ref mut block) => {
          for mut span in block.slots {
            if span.live.load(Ordering::Relaxed) {
              span.clear_mark();
            }
          }
        }
        &mut None => {}
      }
    }
  }
  
  fn arena_by_ptr(&self, ptr: usize) -> Arc<HedgeArena> {
    self.block_by_ptr(ptr).unwrap().arena_by_ptr(ptr)
  }
  
  
  fn mark_gray(&mut self, ptr: ptr) {
    todo!()
  }
  
  fn mark_black(&mut self, ptr: ptr) {
    todo!()
  }
}

impl HAllocator {
  fn block_by_ptr(&self, ptr: usize) -> Option<*HedgeBlock> {
    assert!(self.start <= ptr);
    assert!(ptr < self.start + Self::LOG_BLOCK_SIZE);
    self.blocks[ptr >> Self::LOG_BLOCK_SIZE]
  }
}