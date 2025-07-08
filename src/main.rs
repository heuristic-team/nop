use cli::get_input;
use lexer::lex;
use parser::Parser;

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
            for decl in &decls {
                crate::ast::print::print_decl(decl)
            }
        }
        Err(err) => todo!(), // print error nicely
    }
}
