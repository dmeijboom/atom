#!/bin/sh

cargo build --release

PROGRAMS="fib factorial calc fizzbuzz fib_method nested"

for program in $PROGRAMS
do
    echo ">> running $program"

    PY_RESULT=$(python3 "examples/${program}.py")
    ATOM_RESULT=$(./target/release/atom run "examples/${program}.atom")

    if [ "$PY_RESULT" != "$ATOM_RESULT" ]; then
        echo ">> $program failed"
        echo "python: $PY_RESULT"
        echo "atom: $ATOM_RESULT"
        exit 1
    fi
done

echo ">> all tests passed"
