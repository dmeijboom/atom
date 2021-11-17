#!/bin/sh

set -e

cargo build --release

echo "Wyhash"
hyperfine --warmup 10 "target/release/atom run ./benchmark/wyhash.atom"

echo "Recursive fibonacci"
hyperfine --warmup 10 "target/release/atom run ./benchmark/fib.atom"

echo "Recursive fibonacci (with closures)"
hyperfine --warmup 10 "target/release/atom run ./benchmark/fib_closure.atom"

echo "Binary trees"
hyperfine --warmup 10 "target/release/atom run ./benchmark/binary_trees.atom"

echo "Method call"
hyperfine --warmup 10 "target/release/atom run ./benchmark/method_call.atom"
