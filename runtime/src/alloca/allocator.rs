use std::sync::Arc;
use crate::alloca::{ptr, Cfg, Object};
use crate::alloca::arena::Arena3;

pub trait ArenaAllocator3<T: Object, U: Arena3> {
  fn new(config: Cfg) -> Self;
  
  fn alloc(&mut self, o: &T) -> (ptr, bool);
  
  fn mark_white(&mut self);
  
  fn arena_by_ptr(&mut self, ptr: usize) -> Option<&mut U>;
  
  fn mark_gray(&mut self, ptr: ptr) -> Option<&mut U>;
  
  fn mark_black(&mut self, ptr: ptr) -> Option<&mut U>;
  
  fn sweep(&mut self);
}