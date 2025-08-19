use std::cmp::min;
pub use crate::alloca::*;

pub enum MarkQueueElement<'a, T: Arena3> {
  arena(&'a mut T),
  object(ptr),
  End
}

pub struct MarkQueue<'a, T: Arena3> {
  queue: std::sync::RwLock<Vec<MarkQueueElement<'a, T>>>,
}

impl<'a, T: Arena3> MarkQueue<'a, T> {
  pub fn new() -> MarkQueue<'a, T> {
    Self {
      queue: std::sync::RwLock::new(vec![]),
    }
  }
  
  pub fn pushn(&mut self, elements: &mut Vec<MarkQueueElement<'a, T>>) {
    self.queue
        .write()
        .expect("pushn")
        .extend(elements);
  }
  
  pub fn popn(&mut self, n: usize) -> Vec<MarkQueueElement<'a, T>> {
    let mut lock = self.queue
        .write()
        .expect("popn");
    let bound = min(lock.len(), n);
    let mut result = vec![];
    for _ in 0..bound {
      result.push(lock.pop().expect("popn copy"));
    }
    result
  }
  
  pub fn check(&self) -> Option<&MarkQueueElement<'a, T>> {
    self.queue.read()
        .expect("check")
        .last()
  }
}