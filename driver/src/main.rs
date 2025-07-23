use frontend::lexer::lex;
use frontend::parser::Parser;
use frontend::sema;

mod cli;
use cli::get_input;

mod error_print;
use error_print::print_error;
use opt::translator::{ASTTranslator, Translator};

fn main() {
    let input = match get_input() {
        Ok(input) => input,
        Err(err) => {
            eprintln!("error while reading input: {}", err);
            std::process::exit(1);
        }
    };
    let tokens = lex(&input.contents);

    let parsed = Parser::new(tokens).parse();
    match parsed {
        Ok((fn_decls, type_decls)) => {
            for decl in &type_decls {
                decl.print();
            }

            let sema_res = sema::run(fn_decls, type_decls);

            if let Some(diags) = sema_res.get_diagnostics() {
                diags.for_each(|d| print_error(&input, d));
            }

            if let Some(unit) = sema_res.extract_value() {
                println!("post-sema AST:");
                for decl in unit.0.values() {
                    decl.print();
                }

                let mut translator = ASTTranslator::new();
                let program = translator.translate(unit.0);
                println!("{}", program);
            }
        }
        Err(err) => print_error(&input, &err.into()),
    }
}
