use cli::get_input;
use lexer::lex;

mod cli;
mod lexer;

fn main() {
    let input = get_input();
    let lexed = lex(&input);
    println!("{:?}", lexed);
}
