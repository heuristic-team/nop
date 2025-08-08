use std::sync::atomic::{AtomicBool, Ordering};
use crate::threads::threadpool::ThreadPool;
use crate::utils::*;
mod nthread;
mod threadpool;

pub(crate) struct Threads {
  pool: ThreadPool,
  stw: &'static bool
}

impl Threads {
  pub fn new(stw: &'static bool) -> Self {
    Self {
      pool: ThreadPool::new(),
      stw
    }
  }
  
  pub fn append(&mut self, func: fn(reg, reg, reg, reg, reg), r1: reg, r2: reg, r3: reg, r4: reg, r5: reg) {
    self.pool.append(func, r1, r2, r3, r4, r5);
  }
  
  pub fn go_native(&mut self, rbp: reg, rsp: reg) {
    self.pool.go_native(rbp, rsp);
  }
  
  pub fn go_back(&mut self) {
    while self.stw {}
    
    self.pool.go_back();
  }
}