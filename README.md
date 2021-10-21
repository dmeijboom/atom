# Atom

The documentation can be found at: [GitBook](https://app.gitbook.com/@gitbook-services/s/atom-lang/).

## What is atom?

Atom is a general purpose dynamically typed programming language combining features from Rust, Go, Python and others.
It's written in Rust, uses a bytecode compiler and is designed to be performant, safe and easy to use.

## Features

- Tail calls
- Dynamically typed
- No null or uninitialized fields
- Classes as first-class citizens
- Supports 64-bit floats
- Supports 8/16/32/64/128-bit signed/unsigned integers (defaults to 32-bit signed integer)

### Integers

Integer literals default to 32-bit signed integers unless that doesn't fit.
On integer operations both sides are cast to the upper bound.
This means in following program: `(Uint8)10 + (Uint64)20000` the `Uint8` value of `10` will be cast to a `Uint64` before the add operation.

## Project Status

The project is in a very early stage. Expect most things to work in most cases, but you shouldn't be surprised if the
performance is terrible in some cases or strange bugs occur.
