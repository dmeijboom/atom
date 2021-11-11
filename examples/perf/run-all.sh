#!/bin/sh

set -e

cargo build --release

echo "Wyhash"
hyperfine --warmup 10 "target/release/atom run examples/perf/wyhash.atom"

echo "Recursive fibonacci"
hyperfine --warmup 10 "target/release/atom run examples/perf/fib.atom"

echo "Binary trees"
hyperfine --warmup 10 "target/release/atom run examples/perf/binary_trees.atom"

echo "Method call"
hyperfine --warmup 10 "target/release/atom run examples/perf/method_call.atom"
