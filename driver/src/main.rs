use frontend::ast;
use frontend::lexer::lex;
use frontend::parser::Parser;
use frontend::sema;

mod cli;
use cli::get_input;

mod error_print;
use error_print::print_error;

fn main() {
    let input = get_input();
    let tokens = lex(&input.contents);

    let parsed = Parser::new(tokens).parse();
    match parsed {
        Ok(decls) => {
            let ast_res = sema::run(decls);

            if let Some(diags) = ast_res.get_diagnostics() {
                diags.for_each(|d| print_error(&input, d));
            }

            if let Some(ast) = ast_res.extract_value() {
                println!("post-sema AST:");
                for decl in ast.values() {
                    ast::print::print_decl(decl)
                }

                // TODO: pass AST to translator
            }
        }
        Err(err) => print_error(&input, &err.into()),
    }
}
