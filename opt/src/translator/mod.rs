use std::collections::HashMap;

use frontend::ast::FnDecl;

use crate::ir::program::Program;

type AST = HashMap<String, FnDecl>;

pub fn translate(ast: AST) -> Program {
    todo!()
}
