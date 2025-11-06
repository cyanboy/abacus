use std::{
    env, fs,
    process::Command,
    time::{SystemTime, UNIX_EPOCH},
};

fn binary_path() -> &'static str {
    option_env!("CARGO_BIN_EXE_abc").expect("binary not built; run `cargo test --all`")
}

#[test]
fn expression_flag_evaluates_expression() {
    let binary = binary_path();
    let output = Command::new(binary)
        .args(["-n", "-e", "2+2"])
        .output()
        .expect("failed to run abacus -e");

    assert!(
        output.status.success(),
        "command exited with {:?}",
        output.status
    );
    assert!(
        output.stderr.is_empty(),
        "expected empty stderr, got {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "4\n");
}

#[test]
fn script_execution_outputs_results() {
    let binary = binary_path();
    let mut path = env::temp_dir();
    let unique_suffix = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time went backwards")
        .as_nanos();
    let filename = format!("abacus_script_{}_{}.abc", std::process::id(), unique_suffix);
    path.push(filename);

    fs::write(&path, "answer = 2 + 2\nanswer\n").expect("failed to write script file");

    let output = Command::new(binary)
        .args(["-n", path.to_str().expect("path utf-8")])
        .output()
        .expect("failed to run abacus <file>");

    fs::remove_file(&path).ok();

    assert!(
        output.status.success(),
        "command exited with {:?}",
        output.status
    );
    assert!(
        output.stderr.is_empty(),
        "expected empty stderr, got {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(String::from_utf8(output.stdout).unwrap(), "4\n");
}
