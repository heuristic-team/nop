use crate::alloca::arena::Arena3;
use crate::alloca::{Cfg, ptr};
use crate::utils::Object;

pub trait ArenaAllocator3<U: Arena3> {
    fn new(config: &Cfg) -> Self;

    fn alloc(&mut self, o: &Object) -> (ptr, bool);

    fn mark_white(&mut self);

    fn arena_by_ptr(&self, ptr: usize) -> Option<&U>;

    fn mut_arena_by_ptr(&mut self, ptr: usize) -> Option<&mut U>;

    fn mark_gray(&mut self, ptr: ptr) -> Option<&mut U>;

    fn mark_black(&mut self, ptr: ptr) -> Option<&mut U>;

    fn sweep(&mut self);
}
