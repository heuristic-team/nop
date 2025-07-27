use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::alloca::{ptr, Object};
use crate::alloca::allocator::ArenaAllocator3;
use crate::alloca::heap::{Heap, HeapedArena3};
use crate::alloca::arena::Arena3;

pub(crate) struct HAllocator<T: Object, U: HeapedArena3> {
  start: ptr,
  max_size: usize,
  used_memory: AtomicUsize,
  
  heap: Heap<U>,
  
  blocks: Box<[Option<HedgeBlock<U>>]>,
  count_of_blocks: AtomicUsize,
  large_objects: Vec<Arc<U>>,
  
  _marker: std::marker::PhantomData<T>,
}

impl<T: Object, U: HeapedArena3> HAllocator<T, U> {
  fn block_by_ptr(&mut self, ptr: ptr) -> &mut HedgeBlock<U> {
    assert!(self.start <= ptr);
    assert!(ptr < self.start + Self::SIZE);
    self.blocks[ptr >> Self::LOG_BLOCK_SIZE].as_mut().unwrap()
  }
  
  fn add_new_needed_block(&mut self, size: usize) -> &mut HedgeBlock<U> {
    for power in (Self::LOG_START_ARENA_SIZE..=Self::LOG_MAX_ARENA_SIZE).step_by(2) {
      if 1 << power > size {
        let index = self.count_of_blocks.fetch_add(1, Ordering::Relaxed);
        self.blocks[index] = Some(HedgeBlock::new
            (self.start + (index << Self::LOG_BLOCK_SIZE),
             1 << Self::LOG_BLOCK_SIZE, power));
        
        return self.blocks[index].as_mut().unwrap()
      }
    }
    unreachable!();
  }
}

impl<T: Object, U: HeapedArena3> ArenaAllocator3<T, U> for HAllocator<T, U> {
  const SIZE: usize = 128usize << 30;
  const LOG_BLOCK_SIZE: usize = 26;
  const LOG_START_ARENA_SIZE: usize = 12;
  const LOG_MAX_ARENA_SIZE: usize = 16;
  
  fn new(max_size: usize) -> Self {
    
    unsafe {
      let start = libc::mmap(
        std::ptr::null_mut(),
        Self::SIZE,
        libc::PROT_NONE,
        libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
        -1,
        0
      ) as ptr;
      
      
      if start == 0 {
        panic!("reserve mmap failed");
      }
      
      Self {
        start,
        max_size,
        used_memory: AtomicUsize::new(0),
        heap: Heap::new(),
        blocks: Box::new([const {None}; <Self as ArenaAllocator3<T, U>>::SIZE >> <Self as ArenaAllocator3<T, U>>::LOG_BLOCK_SIZE]),
        count_of_blocks: AtomicUsize::new(0),
        large_objects: vec![],
        _marker: Default::default(),
      }
    }
    
  }
  
  
  unsafe fn alloc(&mut self, o: &T) -> ptr {
    let mut maybe_arena = self.heap.lower_bound(o.size());
    if maybe_arena.is_none() {
      let first_arena = self.add_new_needed_block(o.size()).slots
          .pop()
          .unwrap();
      first_arena.as_mut().unwrap().on();
      maybe_arena = Some(first_arena);
    }
    let arena = maybe_arena.unwrap();
    self.heap.del(arena);
    let ref_arena = arena.as_mut().unwrap();
    ref_arena.add(o.size());
    let ptr = ref_arena.cur();
    
    if ptr == ref_arena.start() {
      let mut block = self.block_by_ptr(ptr);
      match block.slots.pop() {
        Some(slot) => {
          slot.as_mut().unwrap().on();
          self.heap.insert(slot);
        }
        None => {
          let mut new_arena = self.add_new_needed_block(o.size()).slots.pop().unwrap();
          new_arena.as_mut().unwrap().on();
          self.heap.insert(new_arena);
        }
      }
    }
    
    self.used_memory.fetch_add(o.size(), Ordering::Relaxed);
    self.heap.insert(arena);
    ptr
  }
  
  fn mark_white(&mut self) {
    for block in &self.blocks {
      match block {
        Some(b) => unsafe {
          for i in 0..b.slots.len() {
            let mut span = b.slots[i];
            unsafe {
              let ref_span = span.as_mut().unwrap();
              if ref_span.live() {
                ref_span.clear_mark();
              }
            }
          }
        }
        None => {}
      }
    }
  }
  
  fn arena_by_ptr(&mut self, ptr: usize) -> &mut U {
    self.block_by_ptr(ptr)
        .arena_by_ptr(ptr)
  }
  
  
  fn mark_gray(&mut self, ptr: ptr) {
    self.arena_by_ptr(ptr)
        .mark_gray(ptr);
  }
  
  fn mark_black(&mut self, ptr: ptr) {
    self.arena_by_ptr(ptr)
        .mark_black(ptr);
  }
}

struct HedgeBlock<U: Arena3> {
  start: ptr,
  
  size: usize,
  
  log_arena_size: usize,
  
  // only for find object by ref.
  // const
  items: Box<[U]>,
  
  // dynamic
  slots: Vec<*mut U>,
}

impl<U: Arena3> HedgeBlock<U> {
  fn new(start: ptr, size: usize, log_arena_size: usize) -> Self {
    let count = size >> log_arena_size;
    
    let mut arenas = Vec::with_capacity(count);
    for i in 0..count {
      arenas.push(U::new(start + (i << log_arena_size), 1 << log_arena_size));
    }
    
    let box_arenas = arenas.into_boxed_slice();
    
    let slots: Vec<*mut U> = box_arenas.iter().map(|x| x as *const U as *mut U).collect();
    
    Self {
      start,
      size,
      log_arena_size,
      items: box_arenas,
      slots,
    }
  }
  
  fn arena_by_ptr(&mut self, ptr: ptr) -> &mut U {
    assert!(self.log_arena_size > 0);
    assert!(self.start <= ptr);
    assert!(ptr < self.start + self.size);
    &mut self.items[(ptr - self.start) >> self.log_arena_size]
  }
}
