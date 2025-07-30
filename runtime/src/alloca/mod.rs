mod allocator;
mod arena;
mod hallocator;
mod hedgearena;
mod heapedarena;

pub(crate) type ptr = usize;

pub trait Object {
  fn size(&self) -> usize;
  fn get_bitset_of_ref(&self) -> &'static [u8];
}


#[cfg(test)]
mod tests {
  use crate::alloca::allocator::ArenaAllocator3;
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
  
  #[test]
  fn create_allocator() {
    let aa: HAllocator<TestObj, HedgeArena> = HAllocator::new(1<<30);
  }
  
  #[test]
  fn first_alloc() {
    let mut aa: HAllocator<TestObj, HedgeArena> = HAllocator::new(1<<30);
    
    let inst_1 = TestObj { size: 24 };
    unsafe {
      let ptr = aa.alloc(&inst_1);
      assert_ne!(ptr, 0);
      std::ptr::write_bytes(ptr as *mut u8, 126, inst_1.size / 8);
    }
    
  }
  
  #[test]
  fn alloc2() {
    let mut aa: HAllocator<TestObj, HedgeArena> = HAllocator::new(1<<30);
    
    let inst_1 = TestObj { size: 1024 };
    let inst_2 = TestObj { size: 512 };
    unsafe {
      let ptr = aa.alloc(&inst_1);
      assert_ne!(ptr, 0);
      std::ptr::write_bytes(ptr as *mut u8, 126, inst_1.size / 8);
      
      let ptr = aa.alloc(&inst_2);
      assert_ne!(ptr, 0);
      std::ptr::write_bytes(ptr as *mut u8, 126, inst_2.size / 8);
    }
  }
  
}