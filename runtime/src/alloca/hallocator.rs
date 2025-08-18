use std::collections::LinkedList;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::alloca::{ptr, Object};
use crate::alloca::allocator::ArenaAllocator3;
use crate::alloca::arena::Arena3;
use crate::alloca::heapedarena::{Heap, HeapedArena};

pub struct HAllocator<T: Object, U: Arena3> {
  start: ptr,
  max_size: usize,
  used_memory: AtomicUsize,
  
  heap: Heap,
  
  blocks: Box<[Option<HedgeBlock<U>>]>,
  count_of_blocks: AtomicUsize,
  large_objects: LinkedList<ptr>,
  
  _marker: std::marker::PhantomData<T>,
}

impl<T: Object, U: Arena3> HAllocator<T, U> {
  fn block_by_ptr(&mut self, ptr: ptr) -> Option<&mut HedgeBlock<U>> {
    assert!(self.start <= ptr);
    assert!(ptr < self.start + (1 << Self::LOG_CAPACITY_SIZE));
    self.blocks[ptr >> Self::LOG_BLOCK_SIZE]
        .as_mut()
  }
  
  fn add_new_needed_block(&mut self, size: usize) -> &mut HedgeBlock<U> {
    for power in (Self::LOG_START_ARENA_SIZE..=Self::LOG_MAX_ARENA_SIZE).step_by(2) {
      if 1 << power > size {
        let index = self.count_of_blocks.fetch_add(1, Ordering::Relaxed);
        self.blocks[index] = Some(HedgeBlock::new
            (self.start + (index << Self::LOG_BLOCK_SIZE),
             1 << Self::LOG_BLOCK_SIZE, power, index));
        
        return self.blocks[index]
            .as_mut()
            .expect(&format!("arena_by_heaped:\n\
            num_of_block: {}\n", index));
      }
    }
    unreachable!();
  }
  
  fn arena_by_heaped(&mut self, arena_from_heap: HeapedArena) -> &mut U {
    &mut self.blocks[arena_from_heap.num_of_block]
        .as_mut()
        .expect(&format!("arena_by_heaped:\n\
         num_of_block: {}\n", arena_from_heap.num_of_block))
        .items[arena_from_heap.num_of_arena]
  }
}


impl<T: Object, U: Arena3> ArenaAllocator3<T, U> for HAllocator<T, U> {
  const LOG_CAPACITY_SIZE: usize = 37;
  const LOG_BLOCK_SIZE: usize = 26;
  const LOG_START_ARENA_SIZE: usize = 12;
  const LOG_MAX_ARENA_SIZE: usize = 16;
  
  fn new(max_size: usize) -> Self {
    assert!(Self::LOG_CAPACITY_SIZE > Self::LOG_BLOCK_SIZE);
    assert!(Self::LOG_BLOCK_SIZE > Self::LOG_MAX_ARENA_SIZE);
    assert!(Self::LOG_MAX_ARENA_SIZE > Self::LOG_START_ARENA_SIZE);
    
    unsafe {
      let start = libc::mmap(
        std::ptr::null_mut(),
        1 << Self::LOG_CAPACITY_SIZE,
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
        blocks: Box::new([const {None}; 1 << (<Self as ArenaAllocator3<T, U>>::LOG_CAPACITY_SIZE - <Self as ArenaAllocator3<T, U>>::LOG_BLOCK_SIZE)]),
        count_of_blocks: AtomicUsize::new(0),
        large_objects: LinkedList::new(),
        _marker: Default::default(),
      }
    }
    
  }
  
  
  fn alloc(&mut self, o: &T) -> (ptr, bool) {
    let mut maybe_heaped_arena = self.heap.get_min_more_then(o.size());
    if maybe_heaped_arena.is_none() {
    
    }
    let heaped_arena = maybe_heaped_arena.unwrap_or_else(|| {
      let heaped_version_of_first_arena = self
          .add_new_needed_block(o.size())
          .slots
          .pop()
          .expect("In new needed_block len(slots)==0\n");
    
          self.arena_by_heaped(heaped_version_of_first_arena).on();
          heaped_version_of_first_arena
    });
    let ref_arena = self.arena_by_heaped(heaped_arena);
    ref_arena.add(o.size());
    let ptr = ref_arena.cur();
    
    if ptr == ref_arena.start() {
      let mut block = self.block_by_ptr(ptr)
          .expect("hz");
      match block.slots.pop() {
        Some(heaped_arena_from_slots) => {
          self.arena_by_heaped(heaped_arena_from_slots).on();
          self.heap.insert(heaped_arena_from_slots);
        }
        None => {
          let new_heaped_arena = self
              .add_new_needed_block(o.size())
              .slots
              .pop()
              .expect("In new needed_block (when a last arena of the block is not empty) len(slots)==0\n");
          self.arena_by_heaped(new_heaped_arena).on();
          self.heap.insert(new_heaped_arena);
        }
      }
    }
    self.heap.insert(heaped_arena);
    
    let used = self.used_memory.load(Ordering::Relaxed);
    if used > self.max_size {
      (ptr, true)
    } else {
      (ptr, false)
    }
  }
  
  fn mark_white(&mut self) {
    let locked = self.heap.items.lock().expect("lock in white");
    let copy = locked.clone();
    drop(locked);
    for heaped_arena_from_slots in copy.iter() {
      if let arena = self.arena_by_heaped(heaped_arena_from_slots.clone()) && arena.live() {
        arena.clear_mark();
      }
    }
    
    for large in self.large_objects.iter_mut() {
      *large >>= 2;
      *large <<= 2;
    }
  }
  
  fn arena_by_ptr(&mut self, ptr: usize) -> Option<&mut U> {
    self.block_by_ptr(ptr)
        .map(|block| block.arena_by_ptr(ptr))
  }
  
  
  fn mark_gray(&mut self, ptr: ptr) -> Option<&mut U> {
    match self.arena_by_ptr(ptr) {
      Some(arena) => {
        arena.mark_gray(ptr);
        Some(arena)
      }
      None => {
        *self.large_objects.iter_mut()
            .find(|p| (**p >> 2) == (ptr >> 2)) |= 1;
        None
      }
    }
  }
  
  fn mark_black(&mut self, ptr: ptr) -> Option<&mut U> {
    match self.arena_by_ptr(ptr) {
      Some(arena) => {
        arena.mark_gray(ptr);
        Some(arena)
      }
      None => {
        *self.large_objects.iter_mut()
            .find(|p| (**p >> 2) == (ptr >> 2)) |= 2;
        None
      }
    }
  }
}

#[derive(Debug)]
struct HedgeBlock<U: Arena3> {
  start: ptr,
  
  size: usize,
  
  log_arena_size: usize,
  
  // only for find object by ref.
  // const
  items: Box<[U]>,
  
  // dynamic
  slots: Vec<HeapedArena>,
}

impl<U: Arena3> HedgeBlock<U> {
  fn new(start: ptr, size: usize, log_arena_size: usize, number_of_block: usize) -> Self {
    let count = size >> log_arena_size;
    
    let mut arenas = Vec::with_capacity(count);
    let mut slots = Vec::with_capacity(count);
    for i in 0..count {
      arenas.push(U::new(start + (i << log_arena_size), 1 << log_arena_size));
      slots.push(HeapedArena {
        num_of_block: number_of_block,
        num_of_arena: i,
        param: 1 << log_arena_size,
      })
    }
    
    let box_arenas = arenas.into_boxed_slice();
    
    
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
