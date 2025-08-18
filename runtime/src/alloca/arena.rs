use crate::alloca::ptr;

pub trait Arena3 {
  fn new(start: ptr, size: usize) -> Self;
  
  fn start(&self) -> ptr;
  
  fn cur(&self) -> ptr;
  
  fn add(&mut self, size: usize);
  
  fn size(&self) -> usize;
  
  fn how_much(&self) -> usize;
  
  fn gray_map(&self) ->(ptr, usize);
  
  fn black_map(&self) ->(ptr, usize);
  
  fn clear_mark(&mut self);
  
  fn live(&self) -> bool;
  
  fn on(&mut self);
  
  fn off(&mut self);
  
  fn mark_gray(&mut self, ptr: ptr);
  
  fn mark_black(&mut self, ptr: ptr);

  fn fetch_and_add_in_queue(&mut self) -> bool;
  
  fn fetch_and_take_from_queue(&mut self) -> bool;
}