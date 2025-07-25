use std::rc::Rc;

use crate::lexer::WithSpan;
use crate::typesystem::Type;

#[derive(Debug)]
pub struct TypeDecl {
    pub name: WithSpan<String>,
    pub value: WithSpan<Rc<Type>>,
}

impl TypeDecl {
    pub fn print(&self) {
        println!("type {} = {}", self.name.value, self.value.value);
    }
}
