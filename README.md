# blueprint

A tiny Scheme interpreter written in Rust.

Blueprint intends to be fully R7RS-small compliant. Concise, readable, and
idiomatic code is more important for this project than performance, but I
hope to get it no more than 2-3 times slower than cpython.

## Installation

### Building from Source

Install the [Rust toolchain](https://www.rust-lang.org/tools/install).
Run `cargo install --git https://github.com/Stonks3141/blueprint` to build
the program and copy it into your `$PATH`.

## Usage

The CLI takes one positional argument: a path to the Scheme file to run.
If you want to use the REPL, run with no arguments. A full list of command-line
arguments can be obtained by running with the `--help` flag.

### Example

`main.scm`:

```scheme
(define (fib n)
  (if (or (= n 1) (= n 2))
    1
    (+ (fib (- n 1)) (fib (- n 2)))))

(display (fib 10))
```

```sh
$ blueprint ./main.scm
55
```

## Features

- [x] Parser
- [x] Lexical scoping and closures
- [x] Recursion
- [x] Proper tail-call optimization
- [x] Reference-counting garbage collection
- [x] REPL
- [ ] Proper number type with ints, rationals, and complex numbers
- [ ] All R7RS-small basic expressions
- [ ] All R7RS-small standard procedures
- [ ] Macros
