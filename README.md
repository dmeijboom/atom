# atom

## What is atom?

Atom is a general purpose dynamically typed programming language combining features from Rust, Go, Python and others.
It's far from ready but already contains quite some features.

## Features

- Dynamically typed
- No null or uninitialized fields
- Simple, usually only one way to do things

## Data Types

- Int (64-bit only)
- Float (64-bit only)
- String
- Char
- Bool
- Ref
- Array
- Map
- Fn
- Class
- Range
- Option
- Object

## References and values

Most values are copied to and from the stack (also known as: passed by value) but some of the data types are passed by
reference. When a value is passed by reference there will be multiple references to the same value.

## External functions

A function (or method) can either be native (defined in atom) or external (defined in Rust). If the function is defined
in Rust it’s embedded as a function pointer. The function signature will be checked at runtime. As atom is an dynamic
and interpreted language each value can have any type.

## Safety measures

There are no nullable values or uninitialized fields in atom. The only way to deal with optional values is to explicitly
handle these values using the “Option” type. This is a special type of object which has two states: 1) some (where it
contains a value) and 2) none (where it doesn’t have a value).

Users are encouraged to use the “safe” methods such as “std.core.Array.nth(…)” as opposed to the index operator. This is
because these safe methods will check if the index is out of bounds. Note that using non-safe code requires an “unsafe”
block statement.
