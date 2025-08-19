use std::collections::LinkedList;
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::alloca::{ptr, Object};
use crate::alloca::allocator::ArenaAllocator3;
use crate::alloca::arena::Arena3;
use crate::alloca::cfg::Cfg;

pub struct HAllocator<T: Object, U: Arena3> {
  start: ptr,
  used_memory: AtomicUsize,
  
  blocks: Vec<HedgeBlock<U>>,
  num_of_blocks_by_tier: Box<[Box<[usize]>]>,
  
  count_of_blocks: AtomicUsize,
  large_objects: LinkedList<ptr>,
  
  _marker: std::marker::PhantomData<T>,
  
  
  log_capacity_size: usize,
  log_block_size: usize,
  log_start_arena_size: usize,
  step_arena_size: usize,
  log_max_arena_size: usize,
  count_of_tiers: usize,
  max_size: usize,
  max_object_size_by_tier: Box<dyn Fn(usize) -> usize>,
}

impl<T: Object, U: Arena3> HAllocator<T, U> {
  fn block_by_ptr(&mut self, ptr: ptr) -> Option<&mut HedgeBlock<U>> {
    if ptr < self.start || ptr >= self.start + (1 << self.log_capacity_size) {
      None
    } else {
      Some(&mut self.blocks[ptr >> self.log_block_size])
    }
  }
  
  fn add_new_needed_block(&mut self, size: usize) -> &mut U {
    for tier in 0..self.count_of_tiers {
      if (self.max_object_size_by_tier)(tier) >= size {
        
        let index = self.count_of_blocks.fetch_add(1, Ordering::Relaxed);
        let mut new_block = HedgeBlock::new(
          self.start + (index << self.log_block_size),
          1 << self.log_block_size,
          self.log_start_arena_size + tier * self.step_arena_size,
          index, // number_of_block
          tier, // tier
          (self.max_object_size_by_tier)(tier));
        
        let index_of_new_arena = new_block.archive.pop().unwrap();
        let ref_arena: &mut U = new_block.items
            .get_mut(index_of_new_arena.num_of_arena)
            .expect("index out of bounds in add_new_needed_block");
        ref_arena.alive();
        new_block.current = Some(index_of_new_arena);
        
        self.blocks.push(new_block);
        return ref_arena
      }
    }
    unreachable!();
  }
  
  fn arena_tier_by_size(&self, size: usize) -> usize {
    for tier in 1..self.count_of_tiers {
      if (self.max_object_size_by_tier)(tier) > size {
        return tier - 1
      }
    }
    unreachable!();
  }
  
  fn find(&mut self, size: usize) -> Option<&mut U> {
    let tier = self.arena_tier_by_size(size);
    for num_of_block in self.num_of_blocks_by_tier[tier].iter() {
      let mut block = &self.blocks[*num_of_block];
      if block.current.is_some() {
        Some(&mut block.items[block.current.unwrap().num_of_arena]);
      }
    }
    None
  }
  
  fn for_each_arena<F>(&mut self, mut f: F)
  where
      F: FnMut(&mut U),
  {
    let current_count = self.count_of_blocks.load(Ordering::Relaxed);
    
    for i in 0..current_count {
      let mut block = &self.blocks[i];
      for index_arena in block.active.iter() {
        let arena = &mut block.items[index_arena.num_of_arena];
        f(arena);
      }
    }
  }
}


impl<T: Object, U: Arena3> ArenaAllocator3<T, U> for HAllocator<T, U> {
  fn new(config: Cfg) -> Self {
   
    unsafe {
      let start = libc::mmap(
        std::ptr::null_mut(),
        1 << config.log_capacity_size,
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
        max_size: config.max_size,
        used_memory: AtomicUsize::new(0),
        blocks: Vec::new(),
        num_of_blocks_by_tier: Box::new([]),
        count_of_blocks: AtomicUsize::new(0),
        large_objects: LinkedList::new(),
        _marker: Default::default(),
        
        log_capacity_size: config.log_capacity_size,
        log_block_size: config.log_block_size,
        log_start_arena_size: config.log_start_arena_size,
        step_arena_size: config.step_arena_size,
        log_max_arena_size: config.log_max_arena_size,
        count_of_tiers: config.count_of_tiers,
        max_object_size_by_tier: Box::new(move |tier| {
          (config.max_object_size_by_size)(
            1 << (config.log_start_arena_size + config.step_arena_size * tier)
          )}),
      }
    }
  }
  
  
  
  fn alloc(&mut self, o: &T) -> (ptr, bool) {
    let real_size = o.size() + 8;
    
    let mut maybe_ref_arena = self.find(real_size);
    
    let ref_arena = maybe_ref_arena.unwrap_or_else(|| {
      self.add_new_needed_block(o.size())
    });
    let ptr = ref_arena.cur();
    
    ref_arena.add(real_size);
    let block_of_arena: &mut HedgeBlock<U> = self
        .block_by_ptr(ref_arena.cur())
        .expect("something went wrong");
    
    if ref_arena.how_much() < block_of_arena.tier {
      block_of_arena.active
          .push(block_of_arena.current.unwrap());
      block_of_arena.current = block_of_arena.archive.pop();
    }
    
    unsafe {
      *(ptr as *mut *const T) = o as *const T;
    }
    
    let used = self.used_memory.load(Ordering::Relaxed);
    if used > self.max_size {
      (ptr, true)
    } else {
      (ptr, false)
    }
  }
  
  fn mark_white(&mut self) {
    self.for_each_arena(|arena| {
      arena.temp_kill();
    });
   
    for large in self.large_objects.iter_mut() {
      *large >>= 2;
      *large <<= 2;
    }
    
  }
  
  fn arena_by_ptr(&mut self, ptr: usize) -> Option<&mut U> {
    Some(self.block_by_ptr(ptr)?
        .arena_by_ptr(ptr))
  }
  
  
  fn mark_gray(&mut self, ptr: ptr) -> Option<&mut U> {
    match self.arena_by_ptr(ptr) {
      Some(arena) => {
        arena.mark_gray(ptr);
        Some(arena)
      }
      None => {
        *self.large_objects.iter_mut()
            .find(|p| (**p >> 2) == (ptr >> 2))
            .expect("mark_gray")
            |= 1;
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
            .find(|p| (**p >> 2) == (ptr >> 2))
            .expect("mark_black")
            |= 2;
        None
      }
    }
  }
  
  fn sweep(&mut self) {
    self.for_each_arena(|arena| {
      if !arena.live() {
        arena.kill();
      };
    });
  }
}

#[derive(Debug, Clone, Copy)]
pub struct IndexArena {
  pub num_of_block: usize,
  pub num_of_arena: usize,
}

#[derive(Debug)]
struct HedgeBlock<U: Arena3> {
  start: ptr,
  
  size: usize,
  
  tier: usize, // the tier define log_arena_size and max_object_size
  
  log_arena_size: usize,
  max_object_size: usize,
  
  // only for find object by ref.
  // const
  items: Box<[U]>,
  
  // dynamic
  archive: Vec<IndexArena>,
  current: Option<IndexArena>,
  active: Vec<IndexArena>,
}

impl<U: Arena3> HedgeBlock<U> {
  fn new(start: ptr,
         size: usize,
         log_arena_size: usize,
         number_of_block: usize,
         tier: usize,
         max_object_size: usize) -> Self {
    
    let count = size >> log_arena_size;
    
    let mut arenas = Vec::with_capacity(count);
    let mut archive = Vec::with_capacity(count);
    for i in 0..count {
      arenas.push(U::new(start + (i << log_arena_size), 1 << log_arena_size));
      archive.push(IndexArena {
        num_of_block: number_of_block,
        num_of_arena: i,
      })
    }
    
    let box_arenas = arenas.into_boxed_slice();
    
    Self {
      start,
      size,
      tier,
      log_arena_size,
      max_object_size,
      items: box_arenas,
      archive,
      current: None,
      active: Vec::new(),
    }
  }
  
  fn arena_by_ptr(&mut self, ptr: ptr) -> &mut U {
    assert!(self.log_arena_size > 0);
    assert!(self.start <= ptr);
    assert!(ptr < self.start + self.size);
    &mut self.items[(ptr - self.start) >> self.log_arena_size]
  }
}
