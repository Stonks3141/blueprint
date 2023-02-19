default:
  @just --list

build:
  cargo build \
    -Z build-std=std,panic_abort \
    -Z build-std-features=panic_immediate_abort \
    --target $(rustc -vV | grep -oP '^host:\K\s.*$') \
    --release
