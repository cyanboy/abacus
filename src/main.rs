use std::path::PathBuf;

use clap::Parser;

use abacus::{RunConfig, run_expression, run_file, run_with_config};

#[derive(Parser, Debug)]
#[command(
    name = "abc",
    about = "Abacus - The mathemagical REPL",
    author,
    version,
    disable_help_subcommand = true
)]
struct Cli {
    /// Evaluate a single expression and exit
    #[arg(
        short = 'e',
        long = "expr",
        value_name = "EXPR",
        conflicts_with = "script"
    )]
    expr: Option<String>,

    /// Disable ANSI color output
    #[arg(long = "no-color")]
    no_color: bool,

    /// Execute the given Abacus source file
    #[arg(value_name = "FILE", conflicts_with = "expr")]
    script: Option<PathBuf>,
}

fn main() {
    let cli = Cli::parse();
    let config = RunConfig {
        color: !cli.no_color,
    };

    if let Some(expr) = cli.expr {
        if let Err(err) = run_expression(&expr, config) {
            eprintln!("error while executing expression: {err}");
            std::process::exit(1);
        }
        return;
    }

    if let Some(path) = cli.script {
        if let Err(err) = run_file(path, config) {
            eprintln!("error while executing file: {err}");
            std::process::exit(1);
        }
        return;
    }

    run_with_config(config);
}
