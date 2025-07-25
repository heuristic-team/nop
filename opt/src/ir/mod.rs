use std::{cell::RefCell, rc::Rc};

pub mod basic_block;
pub mod function;
pub mod generated;
pub mod instr;
pub mod ir_display;
pub mod ir_type;
pub mod operand;
pub mod program;

type Control<Entity> = Rc<RefCell<Entity>>;
