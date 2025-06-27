# atom

## Introduction

atom is a dynamically typed interpreted language with a bytecode compiler and stack based VM.
My goal is to build a performant language which is fun to build with a clean and minimalistic design.

## Example

```atom
import std;
import std/map;

let data = map.Map();
data.set("name", "Atom");

std.println(data.get("name"));
```

## Features

- NaN tagging
- Tail call optimization
- Arbitrary precision integers
- Arrays with (cheap) slices
- Mark and sweep garbage collector
- Basic types: Int, BigInt, Float, Array, Str, Class, Atom
- Hand-written lexer/recursive descent parser with precedence climbing
