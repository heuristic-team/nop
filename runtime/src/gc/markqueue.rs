pub use crate::alloca::*;
use std::cmp::min;
use std::collections::VecDeque;

#[derive(Copy, Clone)]
pub enum MarkQueueElement {
    Arena(ptr), // start of Arena lol
    Object(ptr),
    End,
}

pub struct MarkQueue {
    queue: std::sync::RwLock<VecDeque<MarkQueueElement>>,
}

impl MarkQueue {
    pub fn new() -> MarkQueue {
        Self {
            queue: std::sync::RwLock::new(VecDeque::new()),
        }
    }

    pub fn pushn(&self, elements: &mut VecDeque<MarkQueueElement>) {
        self.queue.write().expect("pushn1").append(elements);
    }

    pub fn popn(&self, n: usize) -> VecDeque<MarkQueueElement> {
        let mut lock = self.queue.write().expect("popn");
        let bound = min(lock.len(), n);
        let mut result = VecDeque::new();
        for _ in 0..bound {
            result.push_back(lock.pop_front().expect("popn copy"));
        }
        result
    }

    pub fn last(&self) -> Option<MarkQueueElement> {
        self.queue.read()
            .expect("last_is_end")
            .iter()
            .last()
            .copied()
    }
}
