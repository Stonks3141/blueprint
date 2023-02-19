default:
  @just --list

check:
  cargo fmt --check
  cargo clippy -- -D warnings

test:
  cargo test -- -Z unstable-options --report-time

build:
  cargo build \
    -Z build-std=std,panic_abort \
    -Z build-std-features=panic_immediate_abort \
    --target $(rustc -vV | grep -oP '^host:\K\s.*$') \
    --release
