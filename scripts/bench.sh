#!/bin/bash

echo ">> building atom"
cargo build --release 2> /dev/null

PROGRAM="$1"
PROGRAMS="fib factorial calc fizzbuzz fib_method"

for program in $PROGRAMS
do
    if [ ! -z "$PROGRAM" ] && [ "$PROGRAM" != "$program" ]; then
        continue
    fi

    echo ">> running $program"

    hyperfine --shell=none --warmup 10 "./target/release/atom run examples/${program}.atom"
    hyperfine --shell=none --warmup 10 "python3 examples/${program}.py"

    echo
done

echo ">> all benchmarks finished"
