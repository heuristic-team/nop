#![feature(let_chains)]

mod alloca;
mod nni;
mod threads;
mod utils;
mod gc;

use std::sync::Arc;
use utils::*;

static mut ALLOCA: Option<alloca::HAllocator<alloca::ObjectImpl, alloca::HedgeArena>> = None;

pub extern "C" fn init(main: fn(reg, reg, reg, reg, reg), stw: &'static bool) {
  unsafe {
    ALLOCA = Some(alloca::HAllocator::
    <alloca::ObjectImpl,
      alloca::HedgeArena>::
    new(8 << 40));
    
    THREADS = Some(Arc::new(threads::Threads::new(stw)));
    
    THREADS.as_mut()
        .unwrap()
        .append(main, 0, 0, 0, 0, 0);
    
    GC.as_mut()
        .unwrap()
        .master()
  }
}

pub extern "C" fn alloc(t: &dyn alloca::Object) -> alloca::ptr {
  unsafe {
    match ALLOCA.as_mut() {
      None => { panic!("ALLOCA is None")}
      Some(mut alloca) => {
        let (ptr, heap_is_overflow) = alloca.alloc(t);
        if heap_is_overflow {
          GC.as_mut().unwrap().notify_master();
        }
        ptr
      }
    }
  }
}

static mut THREADS: Option<Arc<threads::Threads>> = None;
static mut GC: Option<gc::Gc> = None;

pub extern "C" fn go(func: fn(reg, reg, reg, reg, reg),
                     r1: reg, r2: reg, r3: reg, r4: reg, r5: reg) {
  func(r1, r2, r3, r4, r5);
}

extern "C" unsafe fn go_gc(rbp: reg, rsp: reg) {
  gc::go_gc(rbp, rsp, &(THREADS.as_mut().unwrap()));
}

extern "C" fn go_native(rbp: reg, rsp: reg) {
  unsafe {
    match THREADS.as_mut() {
      None => { panic!("THREADS is not set")}
      Some(mut threads) => { threads.go_native(rbp, rsp)}
    }
  }
}

extern "C" fn go_back() {
  unsafe {
    match THREADS.as_mut() {
      None => { panic!("THREADS is not set") }
      Some(mut threads) => { threads.go_back()}
    }
  }
}

#[cfg(test)]
mod tests {
    use super::*;
    

    #[test]
    fn it_works() {
        assert!(true);
    }
}
