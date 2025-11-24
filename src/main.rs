use std::path::PathBuf;

use clap::Parser;

use abacus::{
    RunConfig, interpreter::DEFAULT_MAX_CALL_DEPTH, run_expression, run_file, run_with_config,
};
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

    /// Maximum recursion depth before aborting evaluation
    #[arg(long = "recursion-limit", value_name = "DEPTH")]
    recursion_limit: Option<usize>,

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
        recursion_limit: cli
            .recursion_limit
            .unwrap_or_else(|| RunConfig::default().recursion_limit),
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
    use std::{
        fs,
        path::PathBuf,
        process::{Command, Stdio},
    };

    fn abc_bin() -> Command {
        let mut path = std::env::current_exe().expect("current exe path");
        // current exe: target/debug/deps/<test_bin>; ascend to target/debug/abc
        path.pop(); // deps
        path.pop(); // debug
        path.push("abc");
        if cfg!(windows) {
            path.set_extension("exe");
        }

        let mut cmd = Command::new(path);
        cmd.stdin(Stdio::null());
        cmd
    }

    #[test]
    fn expr_flag_executes_expression() {
        let output = abc_bin()
            .arg("--no-color")
            .args(["-e", "1 + 1"])
            .output()
            .expect("run abc --expr");
        assert!(
            output.status.success(),
            "process failed: {:?}",
            output.status
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("2"),
            "stdout should contain result, got: {stdout}"
        );
    }

    #[test]
    fn script_flag_executes_file() {
        let mut path = std::env::temp_dir();
        path.push("abacus_script.abc");
        fs::write(&path, "a = 2\na + 3\n").expect("write script");

        let output = abc_bin()
            .arg("--no-color")
            .arg(&path)
            .output()
            .expect("run abc script file");
        fs::remove_file(&path).ok();

        assert!(
            output.status.success(),
            "process failed: {:?}",
            output.status
        );

        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("5"),
            "stdout should contain evaluation result, got: {stdout}"
        );
    }

    #[test]
    fn missing_file_reports_error() {
        let path = PathBuf::from("/nonexistent/abacus_missing.abc");
        let output = abc_bin()
            .arg("--no-color")
            .arg(&path)
            .output()
            .expect("run abc missing file");
        assert!(
            !output.status.success(),
            "process should fail for missing file"
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("error while executing file"),
            "stderr should report file error, got: {stderr}"
        );
    }
}
