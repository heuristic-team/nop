mod allocator;
mod arena;
mod hallocator;
mod hedgearena;
mod cfg;

pub use hedgearena::HedgeArena;
pub use hallocator::HAllocator;
pub use arena::Arena3;
pub use cfg::Cfg;

pub(crate) type ptr = usize;

pub trait Object {
  fn size(&self) -> usize;
  fn get_bitset_of_ref(&self) -> &'static [u8];
}

pub struct ObjectImpl {
  size: usize,
  bitset: &'static [u8],
}

impl Object for ObjectImpl {
  fn size(&self) -> usize {
    self.size
  }
  
  fn get_bitset_of_ref(&self) -> &'static [u8] {
    self.bitset
  }
}

#[cfg(test)]
mod tests {
  use crate::alloca::allocator::ArenaAllocator3;
  use crate::alloca::cfg::Cfg;
  use crate::alloca::hallocator::HAllocator;
  use crate::alloca::hedgearena::HedgeArena;
  use super::*;
  
  struct TestObj {
    size: usize,
  }
  
  impl Object for TestObj {
    fn size(&self) -> usize {
      self.size
    }
    
    fn get_bitset_of_ref(&self) -> &'static [u8] {
      todo!()
    }
  }
  
  fn config1() -> Cfg {
    Cfg::new(
      37,
      26,
      12,
      20,
      0,
      0,
      |size| {
        size / 64
      })
  }
  
  #[test]
  fn create_allocator() {
    let aa: HAllocator<TestObj, HedgeArena> = HAllocator::new(config1());
  }
  
  #[test]
  fn first_alloc() {
    let mut aa: HAllocator<TestObj, HedgeArena> = HAllocator::new(config1());
    
    let inst_1 = TestObj { size: 24 };
    unsafe {
      let (ptr, pred) = aa.alloc(&inst_1);
      assert_eq!(pred, false);
      assert_ne!(ptr, 0);
      std::ptr::write_bytes(ptr as *mut u8, 126, inst_1.size / 8);
    }
    
  }
  
  #[test]
  fn alloc2() {
    let mut aa: HAllocator<TestObj, HedgeArena> = HAllocator::new(config1());
    
    let inst_1 = TestObj { size: 1024 };
    let inst_2 = TestObj { size: 512 };
    unsafe {
      let (ptr, _) = aa.alloc(&inst_1);
      assert_ne!(ptr, 0);
      std::ptr::write_bytes(ptr as *mut u8, 126, inst_1.size / 8);
      
      let (ptr, _) = aa.alloc(&inst_2);
      assert_ne!(ptr, 0);
      std::ptr::write_bytes(ptr as *mut u8, 126, inst_2.size / 8);
    }
  }
  
}