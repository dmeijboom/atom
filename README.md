# atom

## Introduction

atom is a dynamically typed interpreted language with a bytecode compiler.
The goal is to create a language which is fun to build with a clean and minimalistic design.

## Features

- Basic types: Int, Float, Str, Array
- Tail call optimization
- Hand-written lexer/recursive descent parser with precedence climbing
- Simple mark and sweep garbage collector
- NaN tagging (heavily based on [nanoval](https://github.com/phkeese/nanoval))
