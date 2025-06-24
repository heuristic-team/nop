use cli::get_input;
use lexer::lex;

mod cli;
mod ir;
mod lexer;
mod parser;
mod typesystem;

fn main() {
    let input = get_input();
    let lexed = lex(input);
    let tokens: Vec<lexer::Token> = lexed.iter().map(|lexeme| lexeme.token.clone()).collect();
    println!("{:?}", tokens);
}
