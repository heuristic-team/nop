use std::cmp::min;
pub use crate::alloca::*;

pub enum MarkQueueElement {
  arena(ptr), // start of arena lol
  object(ptr),
  End
}

pub struct MarkQueue{
  queue: std::sync::RwLock<Vec<MarkQueueElement>>,
}

impl MarkQueue {
  pub fn new() -> MarkQueue {
    Self {
      queue: std::sync::RwLock::new(vec![]),
    }
  }
  
  pub fn pushn(&mut self, elements: &mut Vec<MarkQueueElement>) {
    self.queue
        .write()
        .expect("pushn1")
        .append(elements);
  }
  
  pub fn popn(&mut self, n: usize) -> Vec<MarkQueueElement> {
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
  
  pub fn last_is_end(&self) -> bool {
    match self.queue.read().expect("last_is_end").last() {
      Some(MarkQueueElement::End) => true,
      _ => false
    }
  }
}