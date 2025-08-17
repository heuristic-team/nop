use std::sync::{Arc, RwLock};
use crate::utils::reg;

pub struct NThread {
  pub routine: std::thread::JoinHandle<()>,
  pub state: Arc<RwLock<ThreadState>>, // i guess arc or rw or rw and arc id redundant
  pub phase: ThreadPhase
}

#[derive(Debug, Clone, Copy)]
pub enum ThreadPhase {
  Mutable,
  Immutable(ThreadCntxt)
}

impl ThreadPhase {
  pub fn new(rbp: reg) -> Self {
    ThreadPhase::Immutable(ThreadCntxt {rbp})
  }
}

#[derive(Debug, Clone, Copy)]
struct ThreadCntxt {
  pub rbp: reg,
}

pub(crate) enum ThreadState {
  Runnable,
  Waiting,
  Blocked,
  Terminated
}