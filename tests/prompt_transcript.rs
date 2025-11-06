use std::{
    io::Write,
    process::{Command, Stdio},
};

fn strip_ansi(input: &str) -> String {
    let mut output = String::with_capacity(input.len());
    let mut chars = input.chars();

    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' {
            match chars.next() {
                Some('[') => {
                    while let Some(c) = chars.next() {
                        if ('@'..='~').contains(&c) {
                            break;
                        }
                    }
                }
                Some(_) => {
                    // Skip single-character escape sequence.
                }
                None => break,
            }
        } else {
            output.push(ch);
        }
    }

    output
}

#[test]
fn transcript_matches_expected_format() {
    let binary = option_env!("CARGO_BIN_EXE_abc").expect("binary not built");
    let mut cmd = Command::new(binary);
    cmd.env("ABACUS_TEST_MODE", "1")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut child = cmd.spawn().expect("failed to spawn abacus");
    {
        let stdin = child.stdin.as_mut().expect("child stdin");
        stdin
            .write_all(b"2 + 2\n\na ==\nquit\n")
            .expect("failed to send input");
    }

    let output = child.wait_with_output().expect("failed to read output");
    assert!(
        output.status.success(),
        "process exited with {:?}",
        output.status
    );
    assert!(
        output.stderr.is_empty(),
        "expected empty stderr, got {:?}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8(output.stdout).expect("stdout not utf-8");
    let cleaned = strip_ansi(&stdout);

    let expected = concat!(
        "[ABACUS - Calculator REPL]\n",
        "Type expressions or 'quit' to exit\n",
        "\n",
        "[0x00]: 2 + 2\n",
        "[0x00]: 4\n",
        "\n",
        "[0x01]: \n",
        "\n",
        "[0x01]: \n",
        "[0x01]: a ==\n",
        "  x unexpected token: expected expression, found '=='\n",
        "   ,-[<repl>:1:3]\n",
        " 1 | a ==\n",
        "   :   ^|\n",
        "   :    `-- unexpected token\n",
        "   `----\n",
        "\n",
        "[0x02]: \n",
        "Goodbye!\n",
    );

    assert_eq!(cleaned, expected);
}
