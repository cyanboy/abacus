use abacus::{eval::Env, lexer::Lexer, parser::Parser};
use rustyline::DefaultEditor;

fn main() {
    println!("Abacus - Calculator REPL");
    println!("Type expressions or 'quit' to exit\n");

    let mut rl = DefaultEditor::new().unwrap();

    let mut env = Env::new();

    loop {
        match rl.readline("> ") {
            Ok(line) => {
                let input = line.trim();
                if input.is_empty() {
                    continue;
                }
                rl.add_history_entry(input).unwrap();

                if line == "quit" || line == "exit" {
                    println!("Goodbye!");
                    break;
                }

                let mut parser = Parser::new(Lexer::new(input));
                match parser.parse() {
                    Ok(stmt) => match env.eval_stmt(&stmt) {
                        Ok(Some(v)) => println!("{v}"),
                        Ok(None) => {}
                        Err(e) => eprintln!("eval error: {e}"),
                    },
                    Err(e) => eprintln!("parse error: {e}"),
                }
            }
            Err(_) => {
                println!("Goodbye!");
                break;
            }
        }
    }
}
