# Atom

## Basic syntax

### Basic types

```atom
ival = 100_000 :: Int
fval = 10.294 :: Float
bval = true :: Bool
sval = "hello world" :: String
```

### Functions

```atom
fn say(input, amount) :: String Int {
    println("hi #{input.repeat(amount)}");
}
```

### Structures

```atom
struct User {
    id :: Int32,
    name :: String,
}

fn email(user) :: User -> String {
    "#{user.name}@example.com"
}

fn email(user) => "#{user.name}@example.com"

fn main {
    let user = User {
        id = 1,
        name = "John",
    };

    println(user.email);
}
```