# Classes

A class is simply a structure that combines fields and values together. Unlike most interpreted languages, classes have no special behavior or inheritance. However, they do support constructors through the `init` method. Classes are initialized by calling them as a function.

Note that methods have to accept `self` as the first argument which will contain the instance of the class that calls the method.

### Syntax

```
import std;

class User {
    fn init(self) {
        self.email = "hello@world.example";
    }
}

let user = User();
std.println(user.email);
```
