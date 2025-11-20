use std::{
    io::Write,
    process::{Command, Stdio},
};
use tempfile::NamedTempFile;

#[test]
fn cli_runs_in_test_mode_with_piped_input() {
    let mut child = Command::new(env!("CARGO_BIN_EXE_abc"))
        .env("ABACUS_TEST_MODE", "1")
        .arg("--no-color")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("spawn abc");

    {
        let stdin = child.stdin.as_mut().expect("stdin available");
        stdin
            .write_all(b"2 + 3\nquit\n")
            .expect("write scripted input");
    }

    let output = child.wait_with_output().expect("wait for abc");
    assert!(
        output.status.success(),
        "process should exit successfully: {:?}",
        output.status
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("5"),
        "stdout should include evaluation result: {stdout}"
    );
    assert!(
        stdout.contains("Goodbye!"),
        "stdout should include goodbye: {stdout}"
    );
}

#[test]
fn cli_expr_flag_evaluates_expression() {
    let output = Command::new(env!("CARGO_BIN_EXE_abc"))
        .arg("--no-color")
        .arg("-e")
        .arg("2 + 3")
        .output()
        .expect("run abc --expr");

    assert!(
        output.status.success(),
        "process failed: {:?}",
        output.status
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("5"), "stdout missing result: {stdout}");
}

#[test]
fn cli_script_flag_executes_file() {
    let script = "a = 4\na * 2\n";
    let mut file = NamedTempFile::new().expect("create temp script");
    file.write_all(script.as_bytes())
        .expect("write script file");
    file.flush().expect("flush script");

    let output = Command::new(env!("CARGO_BIN_EXE_abc"))
        .arg("--no-color")
        .arg(file.path())
        .output()
        .expect("run abc file");

    assert!(
        output.status.success(),
        "process failed: {:?}",
        output.status
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("8"), "stdout missing evaluation: {stdout}");
}
