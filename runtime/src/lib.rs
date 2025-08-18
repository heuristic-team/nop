#![feature(let_chains)]

mod alloca;
mod nni;
mod threads;
mod utils;
mod gc;

use std::sync::Arc;
use utils::*;

static mut THREADS: Option<Arc<threads::Threads>> = None;

static mut GC: Option<gc::Gc<alloca::ObjectImpl, alloca::HedgeArena>> = None;

pub extern "C" fn init(main: fn(reg, reg, reg, reg, reg), stw: &'static bool) {
  unsafe {
    let athreads = Arc::new(threads::Threads::new(stw));
    THREADS = Some(athreads.clone());
    
    GC = Some(gc::Gc::new(athreads, 8 << 40));
    
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
    let gc = GC.as_mut().expect("gc is none (alloc)");
    let (ptr, heap_is_overflow) = gc.alloca.alloc(t);;
    if heap_is_overflow {
      gc.notify_master();
    }
    ptr
  }
}


pub extern "C" fn go(func: fn(reg, reg, reg, reg, reg),
                     r1: reg, r2: reg, r3: reg, r4: reg, r5: reg) {
  func(r1, r2, r3, r4, r5);
}

pub extern "C" fn go_gc(rbp: reg) {
  unsafe {
    GC.as_mut()
        .expect("gc is None")
        .go_gc(rbp);
  }
}

pub extern "C" fn go_native(rbp: reg) {
  unsafe {
    THREADS.as_mut()
        .expect("thrds is None")
        .go_immut(rbp);
  }
}

pub extern "C" fn go_back() {
  unsafe {
    THREADS.as_mut()
        .expect("thrds is None")
        .go_mut();
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
