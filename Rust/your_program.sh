#!/bin/sh

set -e 

(
  cd "$(dirname "$0")" 
  cargo build \
      --quiet \
      --release \
      --target-dir=/tmp/codecrafters-interpreter-target \
      --manifest-path Cargo.toml
)

exec /tmp/codecrafters-interpreter-target/release/interpreter-starter-rust "$@"
