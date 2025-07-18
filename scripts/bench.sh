#!/bin/bash

set -e

echo ">> building atom"
cargo build --release 2> /dev/null

PROGRAM="$1"
PROGRAMS="fib fib_method factorial calc fizzbuzz nested"

for program in $PROGRAMS
do
    if [ ! -z "$PROGRAM" ] && [ "$PROGRAM" != "$program" ]; then
        continue
    fi

    echo ">> running $program"

    hyperfine --shell=none --warmup 20 "./target/release/atom run examples/${program}.atom"
    hyperfine --shell=none --warmup 20 "python3 examples/${program}.py"

    echo
done

echo ">> all benchmarks finished"
