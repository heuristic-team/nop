use std::collections::BTreeSet;

pub(crate) trait KeyFromUsize {
  fn from_usize(n: usize) -> Self;
}

#[derive(Debug)]
pub(crate) struct TreeHeap<T: Ord + KeyFromUsize> {
  pub items: std::sync::RwLock<BTreeSet<T>>,
}

impl<T: Ord + KeyFromUsize + Clone> TreeHeap<T> {
  pub fn new() -> Self {
    Self {
      items: std::sync::RwLock::new(BTreeSet::new()),
    }
  }
  
  pub fn insert(&self, item: T) {
    self.items.write().unwrap().insert(item);
  }
  
  pub fn get_min_more_than(&self, n: usize) -> Option<T> {
    let etalon = T::from_usize(n - 1);
    
    let mut guarded = self.items.read().unwrap();
    
    let key = guarded.range(etalon..).next()?.clone();
    
    Some(guarded.take(&key).expect("Never fails in treeheap"))
  }
}
