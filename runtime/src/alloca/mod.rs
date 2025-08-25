mod allocator;
mod arena;
mod cfg;
mod hallocator;
mod hedgearena;

pub use allocator::ArenaAllocator3;
pub use arena::Arena3;
pub use cfg::Cfg;
pub use hallocator::{HAllocator, IndexArena};
pub use hedgearena::HedgeArena;

pub(crate) type ptr = usize;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::alloca::allocator::ArenaAllocator3;
    use crate::alloca::cfg::Cfg;
    use crate::alloca::hallocator::HAllocator;
    use crate::alloca::hedgearena::HedgeArena;
    use crate::utils::Object;

    fn config1() -> &'static Cfg {
        Box::leak(Box::new(Cfg::new(
            37,
            26,
            12,
            20,
            2,
            1 << 20,
            |size| size / 64,
            3,
        )))
    }

    #[test]
    fn create_allocator() {
        let aa: HAllocator<HedgeArena> = HAllocator::new(config1());
    }

    #[test]
    fn first_alloc() {
        let mut aa: HAllocator<HedgeArena> = HAllocator::new(config1());

        let inst_1 = Object {
            size: 24,
            bitset: &[0],
        };
        unsafe {
            let (ptr, pred) = aa.alloc(&inst_1);
            assert_eq!(pred, false);
            assert_ne!(ptr, 0);
            std::ptr::write_bytes(ptr as *mut u8, 126, inst_1.size / 8);
        }
    }

    #[test]
    fn alloc2() {
        let mut aa: HAllocator<HedgeArena> = HAllocator::new(config1());

        let inst_1 = Object {
            size: 1024,
            bitset: &[0],
        };
        let inst_2 = Object {
            size: 512,
            bitset: &[0],
        };
        unsafe {
            let (ptr, _) = aa.alloc(&inst_1);
            assert_ne!(ptr, 0);
            std::ptr::write_bytes(ptr as *mut u8, 126, inst_1.size / 8);

            let (ptr, _) = aa.alloc(&inst_2);
            assert_ne!(ptr, 0);
            std::ptr::write_bytes(ptr as *mut u8, 126, inst_2.size / 8);
        }
    }
}
