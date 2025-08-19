use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::thread::ThreadId;
use crate::threads::nthread::{NThread, ThreadPhase, ThreadState};
use crate::utils::reg;
use dashmap::DashMap;
use crate::THREADS;

pub struct ThreadPool {
  thread_map: Arc<DashMap<ThreadId, NThread>>,
  count_nop: AtomicUsize
}

impl ThreadPool {
  pub fn new() -> ThreadPool {
    Self {
      thread_map: Arc::new(DashMap::new()),
      count_nop: AtomicUsize::new(0)
    }
  }
  
  pub fn append(&mut self, func: fn(reg, reg, reg, reg, reg), r1: reg, r2: reg, r3: reg, r4: reg, r5: reg) {
    let i = std::thread::current().id();
    let id = Arc::new(std::sync::Mutex::new(i));
    let id2 = Arc::clone(&id);
    
    let main_in_process = Arc::new(AtomicBool::new(true));
    let main_in_process2 = Arc::clone(&main_in_process);
    
    let routine = std::thread::spawn(move || {
      *id2.lock().unwrap() = std::thread::current().id();
      
      while main_in_process2.load(Ordering::SeqCst) {}
      func(r1, r2, r3, r4, r5);
    });
    
    while i == *id.lock().unwrap() {}
    self.count_nop.fetch_add(1, Ordering::Relaxed);
    self.thread_map.insert(*id.lock().unwrap(), NThread {
      routine,
      state: Arc::new(std::sync::RwLock::new(ThreadState::Runnable)),
      phase: ThreadPhase::Nop,
    });
    
    main_in_process.store(false, Ordering::SeqCst);
  }
  
  pub fn go_native(&mut self, rbp: reg, rsp: reg) {
    assert_ne!(self.count_nop.load(Ordering::Relaxed), 0);
    self.count_nop.fetch_sub(1, Ordering::Relaxed);
    
    self.thread_map
        .get_mut(&std::thread::current().id())
        .expect("fantom thread")
        .phase = ThreadPhase::new(rbp, rsp);
  }
  
  pub fn go_back(&mut self) {
    self.count_nop.fetch_add(1, Ordering::Relaxed);
    
    self.thread_map
        .get_mut(&std::thread::current().id())
        .expect("fantom thread")
        .phase = ThreadPhase::Nop
  }
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