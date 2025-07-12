use cli::get_input;
use lexer::lex;
use parser::Parser;

use ast::print::TreePrintable;

use crate::ast::FnDecl;

mod ast;
mod cli;
mod ir;
mod lexer;
mod parser;
mod typesystem;

fn main() {
    let input = get_input();
    let tokens = lex(&input);

    let parsed = Parser::new(tokens).parse();
    match parsed {
        Ok(decls) => {
            let print = <FnDecl as TreePrintable>::print_top_level;
            decls.iter().for_each(print);
        }
        Err(err) => todo!(), // print error nicely
    }
}
