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
            println!("before sema:");
            for decl in &decls {
                crate::ast::print::print_decl(decl)
            }
            println!();

            let ast_res = sema::run(decls);
            if let Some(diags) = ast_res.get_diagnostics() {
                for diag in diags {
                    // TODO: print diagnositcs
                    eprintln!("{:?}", diag);
                }
            }
            if let Some(ast) = ast_res.extract_value() {
                println!("after sema:");
                for decl in ast.values() {
                    crate::ast::print::print_decl(decl)
                }

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
