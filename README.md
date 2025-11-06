# Abacus

![build](https://github.com/cyanboy/abacus/actions/workflows/build.yml/badge.svg)

Abacus is a small, interpreted, functional language for quick experiments

---

## Highlights

- Persistent REPL environment with history and an ANSI-colored prompt.
- 64-bit integers, double-precision floats, and booleans with straightforward formatting.
- Arithmetic, comparison, logical, and bitwise operators with predictable precedence.
- Pattern-matched function arms: mix literals and identifiers and the most specific match runs.
- Helpful diagnostics that underline exactly where evaluation failed.

---

## Install

### Cargo

```bash
cargo install abacus
```

---

## Try It

Run the REPL with `abc`:

```abacus
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

## Build From Source

```bash
cargo build
```

---

## License

This project is licensed under the [MIT License](./LICENSE.md).
