#![allow(dead_code)]

use std::rc::Rc;

use super::function::Func;

/// Represents whole program that is to be optimized.
pub struct Program {
    pub fns: Vec<Rc<Func>>,
}

impl Program {
    /// Creates program instance with specified functions.
    pub fn new(fns: Vec<Rc<Func>>) -> Self {
        Program { fns }
    }

    /// Creates empty program instance.
    pub fn empty() -> Self {
        Program { fns: vec![] }
    }

    /// Adds function to this program.
    pub fn add_function(&mut self, func: Rc<Func>) -> &mut Self {
        self.fns.push(func);
        self
    }

    /// Adds owned function to this program and returns reference counted pointer to that function.
    ///
    /// Useful for consuming function into program and then using reference
    /// counted pointer for labels.
    pub fn add_owned_function(&mut self, func: Func) -> Rc<Func> {
        let func = Rc::new(func);
        self.fns.push(func.clone());
        func
    }
}
