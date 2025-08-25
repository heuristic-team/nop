#![feature(let_chains)]
#![feature(ptr_as_ref_unchecked)]

mod alloca;
mod gc;
mod nni;
mod threads;
mod utils;

use crate::alloca::ArenaAllocator3;
use crate::alloca::Cfg;
use std::sync::{Arc, Mutex, OnceLock};
use utils::*;

static THREADS: OnceLock<Arc<Mutex<threads::Threads>>> = OnceLock::new();

static GC: OnceLock<Mutex<gc::Gc<alloca::HedgeArena>>> = OnceLock::new();

pub extern "C" fn init(main: fn(reg, reg, reg, reg, reg), stw: &'static bool) {
    let athreads = Arc::new(Mutex::new(threads::Threads::new(stw)));
    THREADS.get_or_init(|| athreads.clone());
    GC.get_or_init(|| {
        Mutex::new(gc::Gc::new(
            athreads,
            &Cfg::new(37, 26, 12, 20, 2, 1 << 20, |size| size / 64, 3),
        ))
    });
    THREADS
        .get()
        .unwrap()
        .lock()
        .unwrap()
        .append(main, 0, 0, 0, 0, 0);

    GC.get().unwrap().lock().unwrap().master()
}

pub extern "C" fn alloc(t: &Object) -> alloca::ptr {
    let gc = GC.get().expect("gc is none (alloc)");
    let (ptr, heap_is_overflow) = gc.lock().unwrap().alloca.lock().unwrap().alloc(t);
    if heap_is_overflow {
        gc.lock().unwrap().notify_master();
    }
    ptr
}

pub extern "C" fn go(
    func: fn(reg, reg, reg, reg, reg),
    r1: reg,
    r2: reg,
    r3: reg,
    r4: reg,
    r5: reg,
) {
    func(r1, r2, r3, r4, r5);
}

pub extern "C" fn go_gc(rbp: reg) {
    unsafe {
        GC.get().expect("gc is None").lock().unwrap().go_gc(rbp);
    }
}

pub extern "C" fn go_native(rbp: reg) {
    unsafe {
        THREADS
            .get()
            .expect("thrds is None")
            .lock()
            .unwrap()
            .go_immut(rbp);
    }
}

pub extern "C" fn go_back() {
    unsafe {
        THREADS
            .get()
            .expect("thrds is None")
            .lock()
            .unwrap()
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
