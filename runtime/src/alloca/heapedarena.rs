use std::sync::Mutex;
use crate::alloca::arena::Arena3;

#[derive(Debug, Clone, Copy)]
pub(crate) struct HeapedArena {
  pub(crate) num_of_block: usize,
  pub(crate) num_of_arena: usize,
  pub(crate) param: usize
}

impl Eq for HeapedArena {}

impl PartialEq<Self> for HeapedArena {
  fn eq(&self, other: &Self) -> bool {
    false
  }
}

impl PartialOrd<Self> for HeapedArena {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for HeapedArena {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.param.cmp(&other.param)
  }
}


pub(crate) struct Heap {
  pub(crate) items: Mutex<std::collections::BTreeSet<HeapedArena>>
}

impl Heap {
  pub(crate) fn new() -> Self {
    Self {
      items: Mutex::new(std::collections::BTreeSet::new())
    }
  }
  
  pub(crate) fn insert(&mut self, item: HeapedArena) {
    self.items.lock().unwrap().insert(item);
  }
  
  
  pub(crate) fn get_min_more_then(&mut self, n: usize) -> Option<HeapedArena> {
    assert!(n > 0);
    
    let etalon = HeapedArena {
      num_of_block: 0,
      num_of_arena: 0,
      param: n - 1,
    };
    
    let mut guarded = self.items.lock().expect("lock poisoned");
    
    let key = guarded.range(etalon..).next().map(|k| k.clone())?;
    
    Some(guarded.take(&key).expect("error take\n"))
  }
}
