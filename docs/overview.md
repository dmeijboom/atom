# Overview

Atom is a high-level general purpose programming language with a goal to be performant, small and correct. Some concepts should feel similar as a lot of concepts are taken from other languages.

### Hello World

Create a new file called `hello.atom` and use the following code to get started:

```
import std;

std.println("hello world");
```

We're importing the core interface of the standard library while using the exported `println` function to write output to the screen. Each statement has to end with a semicolon (`;`). Run the code by using the `atom` binary:

```
atom run hello.atom
```

This should output `"hello world"` to the screen.

### Syntax

#### Primitives

```
100000 // 64-bit integer
.45 // 64-bit float
:true
:false
```

#### Atoms

An atom is a globally unique name that can be used as a primitive value. The compiler uses atoms extensively, and they also power booleans. Atoms are distinguished by starting with a colon (`:`).

#### Strings

A string is an array of bytes. It's the only typed array in atom and has to be UTF-8 encoded.

**Example:**

```
"hello world"
"first\nsecond"
```

#### Array

An array is a dynamically sized data structure that references memory allocated on the heap. Arrays support efficient slicing operations, where new slices share references to the same underlying data.

**Example:**

```
[1, 2, 3, 4]
[1, 2, 3][1:] // outputs: [2, 3]
```

#### Functions

You can define a function using the `fn` keyword and specifying the argument names. Functions automatically return the value of their last expression. While the `return` statement is supported, it's unnecessary when returning the final expression.

**Example:**

```
fn example(one, two) {
    10
}
```

#### Variables

The `let` statement declares variables, and you can re-declare variables using the same name. Each variable must be used at least once. While you can assign a value to a variable after declaration, it must be initialized before use.

**Example:**

```
let name = "hello";
let foo;

foo = 10;
```
