use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use mmap_rs::{Reserved, ReservedNone};

pub(crate) type ptr = usize;

pub trait Object {
  fn size(&self) -> usize;
  fn get_bitset_of_ref(&self) -> &'static [u8];
}

pub trait ArenaAllocator3<T: Object> {
  const SIZE: usize;
  
  const LOG_BLOCK_SIZE: usize;
  
  const LOG_START_ARENA_SIZE: usize;
  
  const LOG_MAX_ARENA_SIZE: usize;
  
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
  
  fn live(&self) -> bool;
  
  fn on(&self);
  
  fn off(&self);
}

pub enum AllocateError {
  OutOfMemory,
}


pub struct Heap<'a, T: Ord> {
  items: Mutex<std::collections::BTreeSet<&'a T>>
}

impl<'a, T: Ord> Heap<T> {
  pub fn new() -> Self {
    Self {items: Mutex::new(std::collections::BTreeSet::new())}
  }
  
  pub fn insert(&mut self, item: &'a T) {
    self.items.lock().unwrap().insert(item);
  }
  
  pub fn del(&mut self, item: &'a T) {
    self.items.lock().unwrap().remove(&item);
  }
  
  pub fn lower_bound(&self, n: usize) -> Option<&T> {
    self.items
        .lock()
        .unwrap()
        .range(n..)
        .next()
  }
}

pub struct HAllocator<'a, T: Object, U: Arena3<T>> {
  start: ptr,
  max_size: usize,
  used_memory: AtomicUsize,
  
  heap: Heap<'a, HedgeArena>,
  
  blocks: Box<[Option<&'a HedgeBlock<'a, T, U>>]>,
  large_objects: Vec<Arc<U>>,
}

struct HedgeBlock<'a, T: Object, U: Arena3<T>> {
  start: ptr,
  
  size: usize,
  
  log_arena_size: usize,
  
  // only for find object by ref.
  // const
  items: Box<[&'a U]>,
  
  // dynamic
  slots: std::cell::RefCell<Vec<&'a U>>,
}

impl<T: Object, U: Arena3<T>> HedgeBlock<T, U> {
  fn new(start: ptr, size: usize, log_arena_size: usize) -> Self {
    let count = size / log_arena_size;
    let mut items = Vec::with_capacity(count);
    let mut slots = Vec::with_capacity(count);
    for i in 0..count {
      let new_arena = U::new(start + i * log_arena_size, size);
      items.push(&new_arena);
      slots.push(&new_arena);
    }
    
    Self {
      start,
      size,
      log_arena_size,
      items: items.into_boxed_slice(),
      slots: std::cell::RefCell::new(slots),
    }
  }
  
  fn arena_by_ptr(&self, ptr: ptr) -> &HedgeArena {
    assert!(self.log_arena_size);
    assert!(self.start <= ptr);
    assert!(ptr < self.start + self.size);
    &self.items[(ptr - self.start) >> self.log_arena_size]
  }
  
}

struct HedgeArena {
  start: ptr,
  cur: ptr,
  size: usize, // = 32x
  
  span_start: ptr,
  
  // gray: Box<[u8]>,
  // black: Box<[u8]>,
  
  live: AtomicBool, // хз пока нужен ли, мб оставлю только map, юуду смотреть на None
  map: Option<mmap_rs::MmapMut>,
  
  /// dbg only
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
      map: None,
      objects: vec![]
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
  
  fn live(&self) -> bool {
    self.live.load(Ordering::Relaxed)
  }
  
  fn on(&mut self) {
    self.live.store(true, Ordering::Relaxed);
    
    self.map = Some(mmap_rs::MmapOptions::new(self.size)
        .unwrap()
        .with_address(self.start)
        .map_mut()
        .unwrap());
  }
  
  fn off(&mut self) {
    self.live.store(false, Ordering::Relaxed);
    
    self.map = None;
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
      self.how_much().cmp(&other.how_much())
    }
  }
}

impl HedgeArena {
  
  fn clear_mark(&mut self) {
    let size_mark = self.span_start - self.start;
    assert!(size_mark > 0);
    unsafe {
      std::ptr::write_bytes(self.start as *mut u8, 0, size_mark as usize);
    }
  }
  
  fn how_much(&self) -> usize {
    self.start + self.size - self.cur
  }
}


impl<T: Object, U: Arena3<T>> ArenaAllocator3<T> for HAllocator<T, U> {
  const SIZE: usize = 128usize << 30;
  const LOG_BLOCK_SIZE: usize = 26;
  const LOG_START_ARENA_SIZE: usize = 12;
  const LOG_MAX_ARENA_SIZE: usize = 16;
  
  fn new(max_size: usize) -> Self {
    
    unsafe {
      // let start = memmap2::MmapOptions::map_anon(memmap2::MmapOptions::new()
      //     .len(Self::SIZE))
      //     .unwrap()
      //     .as_ptr()
      //     .cast_mut();
      
      // let start = libc::mmap(
      //   std::ptr::null_mut(),
      //   Self::SIZE,
      //   libc::PROT_NONE,
      //   libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
      //   -1,
      //   0
      // ) as ptr;
      let start = mmap_rs::MmapOptions::new(Self::SIZE)
          .unwrap()
          .map_none()
          .unwrap()
          .start();
      
      if start {
        panic!("reserve mmap failed");
      }
      
      Self {
        start,
        max_size,
        used_memory: AtomicUsize::new(0),
        heap: Heap::new(),
        blocks: Box::new([None; Self::SIZE >> Self::LOG_BLOCK_SIZE]),
        large_objects: vec![],
      }
    }
    
  }
  
  
  fn alloc(&mut self, o: &T) -> ptr {
     match self.heap
         .lower_bound(o.size()) {
       Some(arena) => {
         self.heap
             .del(arena);
         let ptr = arena.cur;
         
         if ptr == arena.start {
           let mut block = self.block_by_ptr(ptr).unwrap();
           match block.slots.borrow_mut().pop() {
             Some(slot) => {
               slot.on();
               self.heap.insert(slot);
             }
             None => {
               self.add_new_needed_block(o.size());
               todo!()
             }
           }
         }
         
         *arena.cur += o.size(); //wtf?????
         self.used_memory.fetch_add(o.size(), Ordering::Relaxed);
         self.heap
             .insert(arena);
         todo!();
         ptr
       }
       _ => {
         todo!();
       }
     };
  }
  
  fn mark_white(&mut self) {
    for block in self.blocks {
      match block {
        Some(b) => {
          for mut span in b.slots {
            if span.live() {
              span.clear_mark();
            }
          }
        }
        None => {}
      }
    }
  }
  
  fn arena_by_ptr(&self, ptr: usize) -> &HedgeArena {
    self.block_by_ptr(ptr)
        .unwrap()
        .arena_by_ptr(ptr)
  }
  
  
  fn mark_gray(&mut self, ptr: ptr) {
    todo!()
  }
  
  fn mark_black(&mut self, ptr: ptr) {
    todo!()
  }
}

impl<T: Object, U: Arena3<T>> HAllocator<T, U> {
  fn block_by_ptr(&self, ptr: ptr) -> Option<&HedgeBlock<T, U>> {
    assert!(self.start <= ptr);
    assert!(ptr < self.start + Self::SIZE);
    self.blocks[ptr >> Self::LOG_BLOCK_SIZE]
  }
  
  fn add_new_needed_block(&mut self, size: usize) {
    for power in (Self::LOG_START_ARENA_SIZE..=Self::LOG_MAX_ARENA_SIZE).step_by(2) {
      if 1 << power > size {
        self.blocks
            .push(HedgeBlock::new(
              self.start + self.blocks.len() << Self::LOG_BLOCK_SIZE,
              1 << Self::LOG_BLOCK_SIZE, power))
      }
    }
  }
}
