
pub struct Cfg {
  pub log_capacity_size: usize,
  pub log_block_size: usize,
  pub log_start_arena_size: usize,
  pub step_arena_size: usize,
  pub log_max_arena_size: usize,
  pub count_of_tiers: usize,
  pub max_size: usize,
  pub max_object_size_by_size: fn(usize) -> usize,
}

impl Cfg {
  pub fn new(log_capacity_size: usize,
             log_block_size: usize,
             log_start_arena_size: usize,
             log_max_arena_size: usize,
             step_arena_size: usize,
             max_size: usize,
             max_object_size_by_size: fn(usize) -> usize) -> Self {
    
    assert!(log_capacity_size > log_block_size);
    assert!(log_block_size > log_max_arena_size);
    assert!(log_max_arena_size > log_start_arena_size);
    assert!(step_arena_size < log_max_arena_size - log_start_arena_size);
    
    Self {
      log_capacity_size,
      log_block_size,
      log_start_arena_size,
      step_arena_size,
      log_max_arena_size,
      count_of_tiers: ((log_max_arena_size - log_start_arena_size) / step_arena_size) + 1,
      max_size,
      max_object_size_by_size,
    }
  }
}