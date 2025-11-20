use std::io::Write;
use std::process::{Command, Stdio};

fn binary_path() -> &'static str {
    option_env!("CARGO_BIN_EXE_abc").expect("binary not built; run `cargo test --all`")
}

#[test]
fn no_color_flag_disables_ansi_sequences() {
    let binary = binary_path();
    let mut cmd = Command::new(binary);
    cmd.arg("--no-color")
        .env("ABACUS_TEST_MODE", "1")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().expect("failed to spawn abacus --no-color");
    child
        .stdin
        .as_mut()
        .expect("stdin")
        .write_all(b"quit\n")
        .expect("write quit");

    let output = child
        .wait_with_output()
        .expect("failed to capture output");

    assert!(output.status.success(), "process exited {:?}", output.status);
    assert!(
        output.stderr.is_empty(),
        "stderr not empty: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
    assert!(stdout.contains("Type expressions or 'quit' to exit"));
    assert!(stdout.contains("Goodbye!"));
    assert!(
        !stdout.contains('\u{1b}'),
        "expected output without ANSI escapes: {stdout:?}"
    );
}
