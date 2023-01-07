# blueprint

[![CI](https://github.com/Stonks3141/blueprint/actions/workflows/ci.yml/badge.svg)](https://github.com/Stonks3141/blueprint/actions/workflows/ci.yml)
[![license](https://img.shields.io/github/license/Stonks3141/blueprint)](https://www.mozilla.org/en-US/MPL/2.0/)

A tiny Scheme interpreter written in Rust.

Blueprint intends to be fully R7RS-small compliant. Concise, readable, and
idiomatic code is more important for this project than performance, but I
hope to get it no more than 2-3 times slower than cpython.

## Installation

### With Cargo

Install the [Rust toolchain](https://www.rust-lang.org/tools/install).
Run `cargo install --git https://github.com/Stonks3141/blueprint blueprint` to
download, build, and install the program.

### Building from Source

Install the [Rust toolchain](https://www.rust-lang.org/tools/install) and clone
the repository. Run `cargo install --path .` in the base directory to build and
install the program.

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
- [x] Reference-counted garbage collection
- [x] REPL
- [ ] Descriptive syntax errors
- [ ] Error locations
- [x] Proper number type with ints, rationals, and complex numbers
- [ ] All basic expressions
- [ ] All standard procedures
- [ ] Quasiquotation
- [ ] Promises
- [ ] Exceptions
- [ ] Libraries
- [ ] Macros

## Contributing

Clone the repository and install the [Rust toolchain](https://www.rust-lang.org/tools/install).
Run `cargo run` in the root directory to build and run the program, or `cargo test` to run tests.
Check out the above list of features that need implementing. Write tests for any new functionality.
