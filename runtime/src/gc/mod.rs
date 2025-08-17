use std::cmp::min;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::thread::JoinHandle;
use crate::{alloca, threads};
use crate::threads::ThreadPhase;
use crate::utils::reg;

pub struct Gc {
  threads: Arc<threads::Threads>,
  
  stw_is_done: Mutex<bool>,
  stw_cv: std::sync::Condvar,
  
  heap_is_done: Mutex<bool>,
  heap_cv: std::sync::Condvar,
  
  mark_is_done: Mutex<bool>,
  mark_cv: std::sync::Condvar,
  
  root_is_done: Mutex<bool>,
  root_cv: std::sync::Condvar,
  
  work_is_done: Mutex<bool>,
  work_cv: std::sync::Condvar,
  workers: Vec<JoinHandle<()>>,
  count_active_workers: AtomicUsize,
  
  root: Vec<alloca::ptr>,
}

impl Gc {
  
  pub fn new(threads: Arc<threads::Threads>) -> Self {
    Self {
      threads,
      stw_is_done: Mutex::new(false),
      stw_cv: Default::default(),
      
      heap_is_done: Mutex::new(false),
      heap_cv: Default::default(),
      
      mark_is_done: Mutex::new(false),
      mark_cv: Default::default(),
      
      work_is_done: Mutex::new(false),
      work_cv: Default::default(),
      
      workers: Vec::with_capacity(count_worker),
      count_active_workers: AtomicUsize::new(0),
      
      root: Vec::new(),
    }
  }
  
  pub fn init_workers(&mut self, count: usize) {
    assert_ne!(self.workers.len(), 0);
    
    self.count_active_workers = AtomicUsize::from(count);
    
    for i in 0..count {
      self.workers.push(thread::spawn(move || {
        loop {
          {
            let mut root_is_done_flag = self.root_is_done.lock().unwrap();
          
            while !*root_is_done_flag {
              root_is_done_flag = self.stw_cv.wait(root_is_done_flag).unwrap()
            }
          }
          
          let count_for_scan = self.root.len() / count + 1;
          for j in min(i*count_for_scan, self.root.len())..self.root.len() {
          
          }
          // TODO WORK
          
          {
            if self.count_active_workers.fetch_sub(1, Ordering::SeqCst) == 1 {
              
              let mut root_is_done_flag = self.stw_is_done.lock().unwrap();
              self.root = Vec::new();
              *root_is_done_flag = false;
              
              let mut work_is_done_flag = self.work_is_done.lock().unwrap();
              assert!(!*work_is_done_flag);
              *work_is_done_flag = true;
              
              self.work_cv.notify_all();
            } else {
              
              let mut work_is_done_flag = self.work_is_done.lock().unwrap();
              while !*work_is_done_flag {
                work_is_done_flag = self.work_cv.wait(work_is_done_flag).unwrap()
              }
            }
          }
          
        }
      }))
    }
  }
  
  pub fn go_gc(&mut self, rbp: reg) {
    self.threads.go_immut(rbp);
    
    
    let mut root_is_done_flag = self.stw_is_done.lock().unwrap();
    while !*root_is_done_flag {
      root_is_done_flag = self.stw_cv.wait(root_is_done_flag).unwrap()
    }
    
    // TODO
  }
  
  pub fn master(&mut self) {
    
    let mut heap_is_done_flag = self.heap_is_done.lock().unwrap();
    while !*heap_is_done_flag {
      heap_is_done_flag = self.heap_cv.wait(heap_is_done_flag).unwrap()
    }
    
    for item in self.threads.pool.thread_map.iter() {
      let nthread = item.value();
      match nthread.phase {
        ThreadPhase::Mutable => {
          unreachable!();
        }
        ThreadPhase::Immutable(cntxt) => {
          self.add_to_root(cntxt.rbp);
        }
      }
    }
    
    *(self.stw_is_done) = true;
    self.stw_cv.notify_all();
  }
  
  pub fn notify_master(&mut self) {
    *(self.heap_is_done) = true;
    self.heap_cv.notify_all();
  }
  
  fn add_to_root(&mut self, rbp: reg) {
    // TODO
  }
  
  fn mark(&mut self) {
  
  }
}