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
            8/*GB*/<< 30,
            |size| size / 64,
            3,
        )))
    }

    fn test_alloc(object: &Object, aa: &mut HAllocator<HedgeArena>) {
        unsafe {
            let (ptr, pred) = aa.alloc(object);
            assert_eq!(pred, false);
            assert_ne!(ptr, 0);
            std::ptr::write_bytes(ptr as *mut u8, 126, object.size / 8);
        }
    }

    #[test]
    fn create_allocator() {
        let aa: HAllocator<HedgeArena> = HAllocator::new(config1());
    }

    #[test]
    fn first_alloc() {
        let mut aa = HAllocator::new(config1());

        let inst_1 = Object {
            size: 24,
            bitset: &[0],
        };
        test_alloc(&inst_1, &mut aa);
    }

    #[test]
    fn alloc2() {
        let mut aa = HAllocator::new(config1());

        let inst_1 = Object {
            size: 1024,
            bitset: &[0],
        };
        test_alloc(&inst_1, &mut aa);
        let inst_2 = Object {
            size: 512,
            bitset: &[0],
        };
        test_alloc(&inst_2, &mut aa);
    }

    #[test]
    fn alloc3() {
        let mut aa = HAllocator::new(config1());
        let inst = Object {
            size: 32,
            bitset: &[0],
        };
        for i in 0..1_000_000 {
            test_alloc(&inst, &mut aa);
        }
    }

    #[test]
    fn alloc4() {
        let cfg = config1();
        let mut aa = HAllocator::new(cfg);

        for i in 0..cfg.count_of_tiers {
            for j in 0..(0x100000 >> (i * cfg.step_arena_size)) {
                let max_size_for_this_tier = (cfg.max_object_size_by_size)(
                    1 << (cfg.log_start_arena_size + i * cfg.step_arena_size),
                );
                let inst = Object {
                    size: max_size_for_this_tier - 8 - (j % 4) * max_size_for_this_tier / 8,
                    bitset: &[0],
                };
                test_alloc(&inst, &mut aa);
            }
        }
    }
}
