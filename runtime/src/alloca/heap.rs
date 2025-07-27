use std::sync::Mutex;
use crate::alloca::arena::Arena3;

pub(crate) trait HeapedArena3: Arena3 + Ord {}

pub(crate) struct Heap<U: HeapedArena3> {
  items: Mutex<std::collections::BTreeSet<*mut U>>,
}

impl<U: HeapedArena3> Heap<U> {
  pub fn new() -> Self {
    Self {items: Mutex::new(std::collections::BTreeSet::new())}
  }
  
  pub fn insert(&mut self, item: *mut U) {
    self.items.lock().unwrap().insert(item);
  }
  
  pub fn del(&mut self, item: *mut U) {
    self.items.lock().unwrap().remove(&item);
  }
  
  pub fn lower_bound(&self, n: usize) -> Option<*mut U> {
    let etalon = &U::new(0, n) as *const U as *mut U;
    self.items
        .lock()
        .unwrap()
        .range(etalon..)
        .next()
        .map(|item| *item)
  }
}
