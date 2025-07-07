# Resumables

Resumables are functions that implement cooperative suspension, allowing execution to pause at `yield` points and resume later. This provides a cleaner alternative to explicit iterator state machines, drawing from coroutine theory to enable lazy evaluation and on-demand computation. The syntax mirrors regular functions, but `yield` replaces `return` for suspension points.

### Syntax

```
pub fn* each(array) {
    for let i = 0; i < array.len(); i += 1 {
        yield array[i];
    }
}
```

### Composable

Composable resumables enable functional composition patterns where resumable computations can be chained, transformed, and combined while preserving their lazy evaluation properties. This design allows resumables to act as first-class values that can be passed to higher-order functions.

#### Example

```
pub fn* enum(inner) {
    let i = 0;
    
    for elem in inner {
        yield [i, elem];
        i += 1;
    } 
}
```
