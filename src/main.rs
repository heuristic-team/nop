use cli::get_input;
use lexer::lex;
use parser::Parser;

mod ast;
mod cli;
mod ir;
mod lexer;
mod parser;
mod sema;
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

            let ast_res = sema::run(decls);
            if let Some(diags) = ast_res.get_diagnostics() {
                for diag in diags {
                    // TODO: print diagnositcs
                }
            }
            if let Some(ast) = ast_res.extract_value() {
                // TODO: pass AST to translator
            }
        }
        Err(err) => {
            // TODO: print error nicely
            eprintln!(
                "parse error:\nexpected {:?}, but got {}",
                err.expected, err.actual
            );
        }
    }
}
