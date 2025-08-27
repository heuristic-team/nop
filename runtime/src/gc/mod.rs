mod markqueue;

use crate::{alloca, gc, threads, utils};
use alloca::{Arena3, ArenaAllocator3, Cfg, ptr};
use gc::markqueue::{MarkQueue, MarkQueueElement};
use std::cmp::min;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::thread::JoinHandle;
use threads::ThreadPhase;
use utils::object::Object;
use utils::reg;

// struct Arc<T>(Arc<RefCell<T>>);
//
// impl<T> Arc<T> {
//     pub fn new(value: T) -> Arc<T> {
//         Arc(Arc::new(RefCell::new(value)))
//     }
//
//     pub fn borrow_mut(&self) -> RefMut<'_, T> {
//         self.0.borrow_mut
//     }
//
//     pub fn clone(&self) -> Arc<T> {
//         Arc(self.0.clone())
//     }
// }

pub struct Gc<U: Arena3 + Send + Sync + 'static> {
    pub alloca: Arc<Mutex<alloca::HAllocator<U>>>,

    threads: Arc<Mutex<threads::Threads>>,

    stw_is_done: Arc<Mutex<bool>>,
    stw_cv: Arc<std::sync::Condvar>,

    heap_is_done: Arc<Mutex<bool>>,
    heap_cv: Arc<std::sync::Condvar>,

    mark_is_done: Arc<Mutex<bool>>,
    mark_cv: Arc<std::sync::Condvar>,

    root_is_done: Arc<Mutex<bool>>,
    root_cv: Arc<std::sync::Condvar>,

    workers: Vec<JoinHandle<()>>,
    count_active_workers: Arc<AtomicUsize>,

    root: Arc<Vec<ptr>>,
    mark_queue: Arc<MarkQueue>,
}

impl<U: Arena3 + Send + Sync> Gc<U> {
    pub fn new(threads: Arc<Mutex<threads::Threads>>, config: &Cfg) -> Self {
        Self {
            alloca: Arc::new(Mutex::new(alloca::HAllocator::<U>::new(config))),
            threads,
            stw_is_done: Arc::new(Mutex::new(false)),
            stw_cv: Arc::new(Default::default()),

            heap_is_done: Arc::new(Mutex::new(false)),
            heap_cv: Arc::new(Default::default()),

            mark_is_done: Arc::new(Mutex::new(false)),
            mark_cv: Arc::new(Default::default()),

            root_is_done: Arc::new(Mutex::new(false)),
            root_cv: Arc::new(Default::default()),

            workers: Vec::new(),
            count_active_workers: Arc::new(AtomicUsize::new(config.count_gc_workers)),

            root: Arc::new(Vec::new()),
            mark_queue: Arc::new(MarkQueue::new()),
        }
    }

    pub fn init_workers(&self, count: usize) {
        assert_ne!(self.workers.len(), 0);

        let mut workers = Vec::with_capacity(count);
        for i in 0..count {
            let thread_root_is_done = self.root_is_done.clone();
            let thread_root_cv = self.root_cv.clone();
            let mut thread_root = self.root.clone();
            let thread_alloca = self.alloca.clone();
            let thread_mark_queue = self.mark_queue.clone().clone();
            let thread_count_active_workers = self.count_active_workers.clone();
            let thread_mark_is_done = self.mark_is_done.clone();
            let thread_mark_cv = self.mark_cv.clone();
            workers.push(thread::spawn(move || {
                loop {
                    {
                        let mut root_is_done_flag = thread_root_is_done.lock().unwrap();

                        while !*root_is_done_flag {
                            root_is_done_flag = thread_root_cv.wait(root_is_done_flag).unwrap()
                        }
                    }
                    let mut local_queue = VecDeque::new();

                    let count_for_scan = thread_root.len() / count + 1;
                    for j in min(i * count_for_scan, thread_root.len())..thread_root.len() {
                        Self::mark_gray_el_from_ptr(
                            thread_alloca.clone(),
                            thread_root[j],
                            &mut local_queue,
                        );

                        if local_queue.len() > /* TODO: add to cfg*/ 15 {
                            (*thread_mark_queue).pushn(&mut local_queue);
                        }
                    }

                    'external: loop {
                        while local_queue.len() > 0 {
                            Self::mark(thread_alloca.clone(), local_queue.pop_front().unwrap());
                        }

                        local_queue = (*thread_mark_queue).popn(16);
                        if local_queue.len() == 0 {
                            if thread_count_active_workers.fetch_sub(1, Ordering::SeqCst) == 1 {
                                (*thread_mark_queue)
                                    .pushn(&mut VecDeque::from([MarkQueueElement::End]));
                                {
                                    let mut root_is_done_flag = thread_root_is_done.lock().unwrap();
                                    thread_root = Arc::new(Vec::new());
                                    *root_is_done_flag = false;

                                    let mut mark_is_done_flag = thread_mark_is_done.lock().unwrap();
                                    assert!(!*mark_is_done_flag);
                                    *mark_is_done_flag = true;

                                    thread_mark_cv.notify_all();
                                }

                                break 'external;
                            } else {
                                loop {
                                    match (*thread_mark_queue).last() {
                                        Some(MarkQueueElement::End) => {
                                            thread_count_active_workers
                                                .fetch_add(1, Ordering::SeqCst);
                                            {
                                                let mut mark_is_done_flag =
                                                    thread_mark_is_done.lock().unwrap();
                                                while !*mark_is_done_flag {
                                                    mark_is_done_flag = thread_mark_cv
                                                        .wait(mark_is_done_flag)
                                                        .unwrap()
                                                }
                                            }
                                            break 'external;
                                        }
                                        None => {}
                                        _ => {
                                            local_queue = (*thread_mark_queue).popn(16);
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }))
        }
    }

    pub fn go_gc(&mut self, rbp: reg) {
        if self.threads.lock().unwrap().go_immut(rbp) == 0 {
            self.notify_master();
        }

        let mut stw_is_done_flag = self.stw_is_done.lock().unwrap();
        while !*stw_is_done_flag {
            stw_is_done_flag = self.stw_cv.wait(stw_is_done_flag).unwrap()
        }

        // TODO
    }

    pub fn master(&mut self) {
        loop {
            {
                let mut heap_is_done_flag = self.heap_is_done.lock().unwrap();
                while !*heap_is_done_flag {
                    heap_is_done_flag = self.heap_cv.wait(heap_is_done_flag).unwrap()
                }
            }
            *self.heap_is_done.lock().unwrap() = false;

            let threads = self.threads.clone();
            let guard_threads = threads.lock().unwrap();
            for item in guard_threads.pool.thread_map.iter() {
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

            *self.root_is_done.lock().unwrap() = true;
            self.root_cv.notify_all();

            let mut mark_is_done_flag = self.mark_is_done.lock().unwrap();
            while !*mark_is_done_flag {
                mark_is_done_flag = self.mark_cv.wait(mark_is_done_flag).unwrap()
            }
            // TODO SWEEP
        }
    }

    pub fn notify_master(&self) {
        *self.heap_is_done.lock().unwrap() = true;
        self.heap_cv.notify_all();
    }

    fn add_to_root(&mut self, rbp: reg) {
        // TODO
    }

    fn mark(mut alloca: Arc<Mutex<alloca::HAllocator<U>>>, el: MarkQueueElement) {
        let mut local_queue = VecDeque::new();
        match el {
            MarkQueueElement::Arena(ptr_on_arena) => {
                let mut alloca_guard = alloca.lock().unwrap();
                let arena = alloca_guard.mut_arena_by_ptr(ptr_on_arena).unwrap();
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
                    if (diff_bits[i / 64] & (1 << (i % 64))) != 0 {
                        unsafe {
                            *((black + (i / 64)) as *mut u64) |= (1 << (i % 64));
                        }
                        let ptr_to_obj = arena.span_start() + i * 8;
                        let ptr_to_header = (ptr_to_obj - 8) as *const Object;
                        let size_of_object = unsafe { ptr_to_header.as_ref_unchecked().size };
                        for j in 0..size_of_object / 8 {
                            if (unsafe { ptr_to_header.as_ref_unchecked().bitset }[j / 8]
                                & (1 << (j % 8)))
                                != 0
                            {
                                let field_ptr = ptr_to_obj + j * 8;
                                Self::mark_gray_el_from_ptr(
                                    alloca.clone(),
                                    field_ptr,
                                    &mut local_queue,
                                );
                            }
                        }
                    }
                }
            }
            MarkQueueElement::Object(ptr) => {
                todo!();
                // делать мне нехуй чтоли
                // TODO
            }
            MarkQueueElement::End => {
                panic!("something went wrong in mark");
            }
        }
    }

    fn mark_gray_el_from_ptr(
        mut alloca: Arc<Mutex<alloca::HAllocator<U>>>,
        ptr: ptr,
        source: &mut VecDeque<MarkQueueElement>,
    ) {
        match alloca.lock().unwrap().mark_gray(ptr) {
            None => source.push_back(MarkQueueElement::Object(ptr)),
            Some(a) => {
                if !a.fetch_and_add_in_queue() {
                    source.push_back(MarkQueueElement::Arena(a.span_start()))
                }
            }
        }
    }
}
