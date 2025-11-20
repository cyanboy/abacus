use std::path::PathBuf;

use clap::Parser;

use abacus::{RunConfig, run_expression, run_file, run_with_config};
use std::process;

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
    process::exit(run_cli(Cli::parse()));
}

fn run_cli(cli: Cli) -> i32 {
    let config = RunConfig {
        color: !cli.no_color,
    };

    if let Some(expr) = cli.expr {
        if let Err(err) = run_expression(&expr, config) {
            eprintln!("error while executing expression: {err}");
            return 1;
        }
        return 0;
    }

    if let Some(path) = cli.script {
        if let Err(err) = run_file(path, config) {
            eprintln!("error while executing file: {err}");
            return 1;
        }
        return 0;
    }

    run_with_config(config);
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        fs,
        time::{SystemTime, UNIX_EPOCH},
    };

    fn unique_path(name: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("{name}_{nonce}"))
    }

    #[test]
    fn expr_flag_executes_expression() {
        let exit = run_cli(Cli {
            expr: Some("1 + 1".into()),
            no_color: true,
            script: None,
        });
        assert_eq!(exit, 0);
    }

    #[test]
    fn script_flag_executes_file() {
        let path = unique_path("abacus_script");
        fs::write(&path, "a = 2\na + 3\n").expect("write script");

        let exit = run_cli(Cli {
            expr: None,
            no_color: true,
            script: Some(path.clone()),
        });
        fs::remove_file(path).ok();
        assert_eq!(exit, 0);
    }

    #[test]
    fn missing_file_reports_error() {
        let path = unique_path("abacus_missing");
        let exit = run_cli(Cli {
            expr: None,
            no_color: false,
            script: Some(path),
        });
        assert_eq!(exit, 1);
    }
}
