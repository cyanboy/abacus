# Abacus

![build](https://github.com/cyanboy/abacus/actions/workflows/build.yml/badge.svg)

Abacus is a small, expressive calculator language for quick experiments and teaching functional programming ideas. It pairs a friendly REPL with pattern-matched functions so you can sketch algorithms without bootstrapping a full Rust project.

---

## Install

### Cargo

```bash
cargo install abacus
```

Need a bleeding-edge build? Install directly from the repository:

```bash
cargo install --locked --git https://github.com/cyanboy/abacus
```

---

## Try It

Run the REPL with `abc`:

```
Abacus - Calculator REPL
Type expressions or 'quit' to exit

> rate = 0.0825
> with_tax(amount) = amount * (1 + rate)
> with_tax(24.99)
27.05475
> fib(0) = 0
> fib(1) = 1
> fib(n) = fib(n-1) + fib(n-2)
> fib(10)
55
> odd(n) = n % 2 == 1
> odd(41)
true
```

---

## Highlights

- Persistent REPL environment with history and an ANSI-colored prompt.
- 64-bit integers, double-precision floats, and booleans with straightforward formatting.
- Arithmetic, comparison, logical, and bitwise operators with predictable precedence.
- Pattern-matched function arms: mix literals and identifiers and the most specific match runs.
- Helpful diagnostics that underline exactly where evaluation failed.

---

## Build From Source

```bash
cargo run      # start the REPL (runs the `abc` binary)
cargo test     # run the test suite
cargo fmt      # format the codebase
```

To build the binary without launching the REPL:

```bash
cargo build
./target/debug/abc
```

---

## License

This project is licensed under the [MIT License](./LICENSE.md).
