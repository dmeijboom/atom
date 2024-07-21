# atom

## Introduction

atom is a dynamically typed interpreted language with a bytecode compiler.
The goal is to create a language which is fun to build with a clean and minimalistic design.

## Features

- Tail call optimization
- Simple mark and sweep garbage collector
- Basic types: Int, Float, Array, Str, Bool, Class
- Hand-written lexer/recursive descent parser with precedence climbing
- NaN tagging (heavily based on [nanoval](https://github.com/phkeese/nanoval))
