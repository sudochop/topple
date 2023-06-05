#!/bin/bash
cargo build
diff <(./target/debug/topple top/putchar.top -j) <(echo "Hello, world!")
diff <(./target/debug/topple top/compare.top -j) <(echo "101010101010")
diff <(./target/debug/topple top/conditional.top -j) <(echo "Hello, sailor!")
diff <(./target/debug/topple top/math.top -j) <(echo "ABCD")
