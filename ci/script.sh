#!/bin/bash

# Run clippy checks
if [ "$CLIPPY" == "true" ]; then
    cargo clippy --all-targets
    exit
fi

# Run clippy rustfmt
if [ "$RUSTFMT" == "true" ]; then
    cargo fmt -- --check
    exit
fi

# Build separately so we generate an 'alacritty' binary without -HASH appended
cargo build --release
cargo test --release
