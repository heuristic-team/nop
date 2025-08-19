mod markqueue;

use std::cmp::min;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::thread;
use std::thread::JoinHandle;
use crate::{alloca, threads, gc, utils};
use alloca::{ptr, Arena3, Object, Cfg};
use threads::ThreadPhase;
use gc::markqueue::{MarkQueue, MarkQueueElement};
use utils::reg;

pub struct Gc<'a, T: Object, U: Arena3> {
  pub alloca: alloca::HAllocator<T, U>,
  
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
  
  root: Vec<ptr>,
  mark_queue: MarkQueue<'a, U>,
}

impl<T: Object, U: Arena3> Gc<'_, T, U> {
  
  pub fn new(threads: Arc<threads::Threads>, config: Cfg) -> Self {
    Self {
      alloca: alloca::HAllocator::<T, U>::new(config),
      threads,
      stw_is_done: Mutex::new(false),
      stw_cv: Default::default(),
      
      heap_is_done: Mutex::new(false),
      heap_cv: Default::default(),
      
      mark_is_done: Mutex::new(false),
      mark_cv: Default::default(),
      
      root_is_done: Mutex::new(false),
      root_cv: Default::default(),
      
      work_is_done: Mutex::new(false),
      work_cv: Default::default(),
      
      workers: Vec::new(),
      count_active_workers: AtomicUsize::new(0),
      
      root: Vec::new(),
      mark_queue: MarkQueue::new(),
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
          let mut local_queue = vec![];
          
          let count_for_scan = self.root.len() / count + 1;
          for j in min(i*count_for_scan, self.root.len())..self.root.len() {
            self.mark_gray_el_from_ptr(self.root[j], &mut local_queue);
            
            if !(local_queue.len() & 15) { // TODO: make somethink like cfg
              self.mark_queue.pushn(&mut local_queue);
            }
          }
          
          'external: loop {
            
            while local_queue.len() > 0 {
              self.mark(local_queue.pop().unwrap());
            }
            
            local_queue = self.mark_queue.popn(16);
            if local_queue.len() == 0 {
              if self.count_active_workers.fetch_sub(1, Ordering::SeqCst) == 1 {
                self.mark_queue.pushn(&mut vec![MarkQueueElement::End]);
                
                {
                  let mut root_is_done_flag = self.stw_is_done.lock().unwrap();
                  self.root = Vec::new();
                  *root_is_done_flag = false;
                  
                  let mut work_is_done_flag = self.work_is_done.lock().unwrap();
                  assert!(!*work_is_done_flag);
                  *work_is_done_flag = true;
                  
                  self.work_cv.notify_all();
                }
                
                break 'external; // for better reading
              } else {
                loop {
                  if let last = self.mark_queue.check()
                      && last.is_some() {
                    if last.unwrap() == MarkQueueElement::End {
                      self.count_active_workers.fetch_add(1, Ordering::SeqCst);
                      
                      {
                        let mut work_is_done_flag = self.work_is_done.lock().unwrap();
                        while !*work_is_done_flag {
                          work_is_done_flag = self.work_cv.wait(work_is_done_flag).unwrap()
                        }
                      }
                      break 'external;
                    }
                    else {
                      local_queue = self.mark_queue.popn(16);
                      break;
                    }
                  }
                }
              }
            }
          }
          // TODO WORK
        }
      }))
    }
  }
  
  pub fn go_gc(&mut self, rbp: reg) {
    self.threads.go_immut(rbp);
    
    
    let mut stw_is_done_flag = self.stw_is_done.lock().unwrap();
    while !*stw_is_done_flag {
      stw_is_done_flag = self.stw_cv.wait(stw_is_done_flag).unwrap()
    }
    
    
    // TODO
  }
  
  pub fn master(&mut self) {
    loop {
      
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
      
      *(self.heap_is_done) = false;
      
      let mut work_is_done_flag = self.work_is_done.lock().unwrap();
      while !*work_is_done_flag {
        work_is_done_flag = self.work_cv.wait(work_is_done_flag).unwrap()
        
      }
    }
  }
  
  pub fn notify_master(&mut self) {
    *(self.heap_is_done) = true;
    self.heap_cv.notify_all();
  }
  
  fn add_to_root(&mut self, rbp: reg) {
    // TODO
  }
  
  fn mark(&mut self, el: MarkQueueElement<U>) {
    let mut local_queue = vec![];
    match el {
      MarkQueueElement::arena(arena) => {
        arena.make_live();
        let (black, size) = arena.black_map();
        let (gray, size) = arena.gray_map();
        let mut diff_bits = vec![];
        // byte-iter
        for i in (0..size).step_by(8) {
          unsafe {
            diff_bits.push(*((gray + i) as *const u64) ^ *((black + i) as *const u64));
          }
        }
        // bit-iter
        for i in 0..(size * 8) {
          if diff_bits[i / 64] & (1 << (i % 64)) {
            unsafe {
              *((black + (i / 64)) as *mut u64) |= (1 << (i % 64));
            }
            let ptr_to_obj = arena.span_start() + i * 8;
            let ptr_to_header = (ptr_to_obj - 8) as *const dyn Object;
            let size_of_object = ptr_to_header.size();
            for j in 0..size_of_object / 8 {
              if ptr_to_header.get_bitset_of_ref()[j / 8] & (1 << (j % 8)) {
                let field_ptr = ptr_to_obj + j * 8;
                self.mark_gray_el_from_ptr(field_ptr, &mut local_queue);
              }
            }
          }
        }
      }
      MarkQueueElement::object(ptr) => {
        todo!();
        // делать мне нехуй чтоли
        // TODO
      }
      MarkQueueElement::End => {
        panic!("something went wrong in mark");
      }
    }
  }
  
  fn mark_gray_el_from_ptr(&mut self, ptr: ptr, source: &mut Vec<MarkQueueElement<U>>) {
    match self.alloca.mark_gray(ptr) {
      None => {
        source.push(MarkQueueElement::object(ptr))
      }
      Some(a) => {
        if !a.fetch_and_add_in_queue() {
          source.push(MarkQueueElement::arena(a))
        }
      }
    }
  }
}