mod cli;

use cli::get_input;

use frontend::{
    ast::{FnDecl, print::TreePrintable},
    lexer::lex,
    parser::Parser,
};

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
