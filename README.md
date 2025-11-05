# Abacus

Abacus is a small, expressive calculator language with a focus on readable diagnostics. It offers an interactive REPL, arithmetic with integers/floats/booleans, user-defined functions (including simple pattern matching), and clear error reporting powered by [`miette`](https://github.com/zkat/miette).

> “Abacus lets you prototype little math snippets and functional helpers without the ceremony of a full Rust project.”

---

## Quick Start

```bash
git clone https://github.com/yourname/abacus.git
cd abacus
cargo run
```

Example REPL session:

```
Abacus - Calculator REPL
Type expressions or 'quit' to exit

> square(x) = x * x
> square(12)
144
> fib(0) = 0
> fib(1) = 1
> fib(n) = fib(n-1) + fib(n-2)
> fib(10)
55
```

Type `quit` or `exit` to leave the REPL.

---

## Features

- **Interactive REPL** with persistent definitions.
- **Basic types**: integers (`i64`), floats (`f64`), booleans.
- **Arithmetic & logical operators** (`+`, `-`, `*`, `/`, `%`, `&&`, `||`, comparisons, bitwise ops).
- **Pattern-matched functions**: define multiple arms with literal/identifier patterns.
- **Helpful diagnostics** with source highlighting; fallback caret rendering for span-less parses.
- **Unit tests** covering the lexer, parser, and evaluator.

---

## Building & Running

### Requirements

- Rust 1.75+ (2024 edition). Install via [rustup](https://rustup.rs).

### Commands

```bash
cargo run      # launch the REPL
cargo test     # run unit tests
cargo fmt      # format code
```

To rebuild the debug binary:

```bash
cargo build
./target/debug/abacus
```

---

## Language Overview

### Expressions

- Integers (`42`), floats (`1.5`, `1e-3`), booleans (`true`, `false`)
- Grouping with parentheses, unary `-` and `!`
- Binary operators with standard precedence (logical → bitwise → comparison → arithmetic → multiplicative)

### Variables

```
> x = 10
> x * 3
30
```

Assignments store values in the current environment. Reassignments overwrite global bindings.

### Functions & Pattern Matching

```
> negate(x) = -x
> negate(4)
-4

> fib(0) = 0
> fib(1) = 1
> fib(n) = fib(n-1) + fib(n-2)
> fib(6)
8
```

Patterns may be literals (`fib(0)`) or identifiers (`fib(n)`). Arms are sorted by specificity—literal-only patterns win ties.

---

## Diagnostics

Abacus uses `miette` for rich error messages. Example:

```
> adder(1)
Error: undefined function: adder

   ,-[<repl>:1:1]
 1 | adder(1)
   : ^^|^^
   :   `-- not defined here
   `----
```

Parse errors without spans fall back to a minimal caret display:

```
> 1/
Error: Unexpected token: expected expression, found end of input

  1 | 1/
    |   ^
```

---

## Roadmap & Ideas

- Strings and richer pattern matching (`_` wildcards, guards)
- Persistent REPL commands (`:vars`, `:funcs`, `:clear`)
- Multiline input/editing helpers
- Standard-library utilities shipped with the REPL

See [GitHub Issues](https://github.com/yourname/abacus/issues) for current discussions.

---

## Contributing

Contributions are welcome!

1. Fork and clone the repo
2. Create a feature branch (`git checkout -b feature/idea`)
3. Add tests and keep `cargo fmt`/`cargo clippy` clean
4. Open a pull request

Please review the [LICENSE](./LICENSE.md) (MIT) before submitting code.

---

## License

This project is licensed under the [MIT License](./LICENSE.md).
