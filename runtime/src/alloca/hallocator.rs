use crate::alloca::allocator::ArenaAllocator3;
use crate::alloca::arena::Arena3;
use crate::alloca::cfg::Cfg;
use crate::alloca::ptr;
use crate::utils::Object;
use std::collections::LinkedList;
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct HAllocator<U: Arena3> {
    start: ptr,
    used_memory: AtomicUsize,

    blocks: Vec<HedgeBlock<U>>,
    num_of_blocks_by_tier: Vec<Vec<usize>>,

    large_objects: LinkedList<ptr>,

    log_capacity_size: usize,
    log_block_size: usize,
    log_start_arena_size: usize,
    step_arena_size: usize,
    log_max_arena_size: usize,
    count_of_tiers: usize,
    max_size: usize,
    max_object_size_by_tier: Box<dyn Fn(usize) -> usize + Send + Sync>,
}

impl<U: Arena3> HAllocator<U> {
    fn block_by_ptr(&self, ptr: ptr) -> Option<&HedgeBlock<U>> {
        if ptr < self.start || ptr >= self.start + (1 << self.log_capacity_size) {
            None
        } else {
            Some(&self.blocks[ptr >> self.log_block_size])
        }
    }

    fn mut_block_by_ptr(&mut self, ptr: ptr) -> Option<&mut HedgeBlock<U>> {
        if ptr < self.start || ptr >= self.start + (1 << self.log_capacity_size) {
            None
        } else {
            Some(&mut self.blocks[(ptr - self.start) >> self.log_block_size])
        }
    }

    fn add_new_needed_block(&mut self, size: usize) -> &mut U {
        for tier in 0..self.count_of_tiers {
            if (self.max_object_size_by_tier)(tier) >= size {
                let index = self.blocks.len();
                let new_block = HedgeBlock::new(
                    self.start + (index << self.log_block_size),
                    1 << self.log_block_size,
                    self.log_start_arena_size + tier * self.step_arena_size,
                    index, // number_of_block
                    tier,  // tier
                    (self.max_object_size_by_tier)(tier),
                );

                self.blocks.push(new_block);

                let mut new_block = self.blocks.last_mut().unwrap();
                let index_of_new_arena = new_block.archive.pop().unwrap();
                let ref_arena: &mut U = new_block
                    .items
                    .get_mut(index_of_new_arena.num_of_arena)
                    .expect("index out of bounds in add_new_needed_block");
                ref_arena.alive();
                new_block.current = Some(index_of_new_arena);

                self.num_of_blocks_by_tier[tier].push(index);
                return ref_arena;
            }
        }
        unreachable!();
    }

    fn arena_tier_by_size(&self, size: usize) -> usize {
        for tier in 1..self.count_of_tiers {
            if (self.max_object_size_by_tier)(tier) > size {
                return tier - 1;
            }
        }
        unreachable!();
    }
    
    fn find(&mut self, size: usize) -> Option<&mut U> {
        let tier = self.arena_tier_by_size(size);
        let mut found = None;
        
        for &num_of_block in &self.num_of_blocks_by_tier[tier] {
            let block = &self.blocks[num_of_block];
            if let Some(current) = block.current {
                found = Some((num_of_block, current.num_of_arena));
                break;
            }
        }
        
        if let Some((block_idx, arena_idx)) = found {
            let block = &mut self.blocks[block_idx];
            return Some(&mut block.items[arena_idx]);
        }
        
        None
    }
    
    
    fn for_each_arena<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut U),
    {
        let current_count = self.blocks.len();

        for i in 0..current_count {
            let mut block = &mut self.blocks[i];
            for index_arena in block.active.iter() {
                let arena = &mut block.items[index_arena.num_of_arena];
                f(arena);
            }
        }
    }
}

impl<U: Arena3> ArenaAllocator3<U> for HAllocator<U> {
    fn new(config: &Cfg) -> Self {
        unsafe {
            let start = libc::mmap(
                std::ptr::null_mut(),
                1 << config.log_capacity_size,
                libc::PROT_NONE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as ptr;

            if start == 0 {
                panic!("reserve mmap failed");
            }

            let mosns = config.max_object_size_by_size;
            let lsas = config.log_start_arena_size;
            let sas = config.step_arena_size;
            Self {
                start,
                max_size: config.max_size,
                used_memory: AtomicUsize::new(0),
                blocks: Vec::new(),
                num_of_blocks_by_tier: vec![Vec::new(); config.count_of_tiers],
                large_objects: LinkedList::new(),

                log_capacity_size: config.log_capacity_size,
                log_block_size: config.log_block_size,
                log_start_arena_size: config.log_start_arena_size,
                step_arena_size: config.step_arena_size,
                log_max_arena_size: config.log_max_arena_size,
                count_of_tiers: config.count_of_tiers,
                max_object_size_by_tier: Box::new(move |tier| mosns(1 << (lsas + sas * tier))),
            }
        }
    }

    fn alloc(&mut self, o: &Object) -> (ptr, bool) {
        let real_size = o.size + 8;

        // let mut maybe_ref_arena = self.find(real_size);
        //
        // // let ref_arena = maybe_ref_arena.unwrap_or(self.add_new_needed_block(real_size));
        // let ref_arena = match maybe_ref_arena {
        //   Some(a) => a,
        //   None => self.add_new_needed_block(real_size),
        // };

        let ref_arena = {
            let maybe_ref_arena = self.find(real_size);
            match maybe_ref_arena {
                Some(a) => a,
                None => self.add_new_needed_block(real_size),
            }
        };

        let ptr = ref_arena.cur();

        ref_arena.add(real_size);
        let empty_space = ref_arena.how_much();

        let block_of_arena = self.mut_block_by_ptr(ptr).expect("something went wrong");

        if empty_space < block_of_arena.max_object_size {
            block_of_arena.active.push(block_of_arena.current.unwrap());
            block_of_arena.current = block_of_arena.archive.pop();
            if block_of_arena.current.is_some() {
                block_of_arena.items[block_of_arena.current.unwrap().num_of_arena].alive();
            }
        }

        unsafe {
            *(ptr as *mut *const Object) = o as *const Object;
        }

        let used = self.used_memory.load(Ordering::Relaxed);
        if used > self.max_size {
            (ptr + 8, true)
        } else {
            (ptr + 8, false)
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

    fn arena_by_ptr(&self, ptr: usize) -> Option<&U> {
        Some(self.block_by_ptr(ptr)?.arena_by_ptr(ptr))
    }

    fn mut_arena_by_ptr(&mut self, ptr: usize) -> Option<&mut U> {
        Some(self.mut_block_by_ptr(ptr)?.mut_arena_by_ptr(ptr))
    }

    fn mark_gray(&mut self, ptr: ptr) -> Option<&mut U> {
        match self.arena_by_ptr(ptr) {
            Some(_) => {
                let arena = self.mut_arena_by_ptr(ptr).unwrap();
                arena.mark_gray(ptr);
                Some(arena)
            }
            None => {
                *self
                    .large_objects
                    .iter_mut()
                    .find(|p| (**p >> 2) == (ptr >> 2))
                    .expect("mark_gray") |= 1;
                None
            }
        }
    }

    fn mark_black(&mut self, ptr: ptr) -> Option<&mut U> {
        match self.arena_by_ptr(ptr) {
            Some(_) => {
                let arena = self.mut_arena_by_ptr(ptr).unwrap();
                arena.mark_gray(ptr);
                Some(arena)
            }
            None => {
                *self
                    .large_objects
                    .iter_mut()
                    .find(|p| (**p >> 2) == (ptr >> 2))
                    .expect("mark_black") |= 2;
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
    fn new(
        start: ptr,
        size: usize,
        log_arena_size: usize,
        number_of_block: usize,
        tier: usize,
        max_object_size: usize,
    ) -> Self {
        let count = size >> log_arena_size;

        let mut arenas = Vec::with_capacity(count);
        let mut archive = Vec::with_capacity(count);
        for i in 0..count {
            arenas.push(U::new(
                start + ((count - i - 1) << log_arena_size),
                1 << log_arena_size,
            ));
            archive.push(IndexArena {
                num_of_block: number_of_block,
                num_of_arena: count - i - 1, // literally for good debug
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

    fn mut_arena_by_ptr(&mut self, ptr: ptr) -> &mut U {
        assert!(self.log_arena_size > 0);
        assert!(self.start <= ptr);
        assert!(ptr < self.start + self.size);
        &mut self.items[(ptr - self.start) >> self.log_arena_size]
    }

    fn arena_by_ptr(&self, ptr: ptr) -> &U {
        assert!(self.log_arena_size > 0);
        assert!(self.start <= ptr);
        assert!(ptr < self.start + self.size);
        &self.items[(ptr - self.start) >> self.log_arena_size]
    }
}
