use crate::alloca::{ptr, Object};
use crate::alloca::arena::Arena3;

pub trait ArenaAllocator3<T: Object, U: Arena3> {
  const LOG_CAPACITY_SIZE: usize;
  
  const LOG_BLOCK_SIZE: usize;
  
  const LOG_START_ARENA_SIZE: usize;
  
  const LOG_MAX_ARENA_SIZE: usize;
  
  fn new(max_size: usize) -> Self;
  
  fn alloc(&mut self, object_instance: &T) -> ptr;
  
  fn mark_white(&mut self);
  
  fn arena_by_ptr(&mut self, ptr: usize) -> &mut U;
  
  fn mark_gray(&mut self, ptr: ptr);
  
  fn mark_black(&mut self, ptr: ptr);
}