#![feature(let_chains)]

mod alloca;

// static mut ALLOCA: Option<alloca::HAllocator<T, U>> = None;

// pub extern "C" fn init(main: fn()) {
//   unsafe {
//     ALLOCA = Some(alloca::ArenaAllocator3::new(8 << 40));
//   }
//   main();
// }
//
// pub extern "C" fn alloc(t: &dyn alloca::Object) -> alloca::ptr {
//   unsafe {
//     match ALLOCA.as_mut() {
//       None => { panic!("ALLOCA is None")}
//       Some(mut alloca) => { alloca.alloc(t)}
//     }
//   }
// }

type reg = usize;

pub extern "C" fn go(func: fn(reg, reg, reg, reg, reg),
                     r1: reg, r2: reg, r3: reg, r4: reg, r5: reg) {
  func(r1, r2, r3, r4, r5);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert!(true);
    }
}
