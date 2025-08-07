use std::sync::{Arc, RwLock};
use crate::utils::reg;

pub(crate) struct NThread {
  pub(crate) routine: std::thread::JoinHandle<()>,
  pub(crate) state: Arc<RwLock<ThreadState>>,
  pub(crate) phase: ThreadPhase
}

pub(crate) enum ThreadPhase {
  Nop,
  Native(ThreadCntxt)
}

impl ThreadPhase {
  pub fn new(rbp: reg, rsp: reg) -> Self {
    ThreadPhase::Native(ThreadCntxt {rbp, rsp})
  }
}

struct ThreadCntxt {
  pub(crate) rbp: reg,
  pub(crate) rsp: reg
}

pub(crate) enum ThreadState {
  Runnable,
  Waiting,
  Blocked,
  Terminated
}