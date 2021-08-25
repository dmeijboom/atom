#!/bin/sh

set -e

cargo build --release

echo "Recursive fibonacci"
hyperfine --warmup 5 "../target/release/atom --module-path=./src/std/atom run examples/perf/fib.atom"

echo "Binary trees"
hyperfine --warmup 5 "../target/release/atom --module-path=./src/std/atom run examples/perf/binary_trees.atom"

echo "Method call"
hyperfine --warmup 5 "../target/release/atom --module-path=./src/std/atom run examples/perf/method_call.atom"
