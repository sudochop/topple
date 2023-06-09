#!/bin/bash
tests=$(find test -type f -name "*.top")
cargo build
for i in $tests; do
    ./target/debug/topple "$i" -j | git diff --no-index -- - "$i.stdout"
done