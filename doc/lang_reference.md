# Kulfon language reference
## Intro

### The most common C and C++ errors that Kulfon is trying to address
* Uninitialized variables. Kulfon: all variables must be initialized before accessing.
* Boolean expression errors. Kulfon: all boolean expression must be explicit, there is no hidden type casting.
* Undefined behaviours. Kulfon: there are no undefinded behaviours.
* Returning local objects by reference: Kulfon: lifetime checker prevents from this scenario.
* Dangling pointers: Kulfon - for stack based allocation this is not possible, for heap allocated object this is vastly reduced.
* Invalidated iterators: Kulfon:?
* Type conversions. Kuflon: there are not implicit type conversions.

## Functions
### Variadic functions
Example:
```rust
/// Function to print a formatted string with variadic arguments
/// Supports i32, bool, and str types. Raises an error for unsupported types.
fn print(s: &str, ...) {
    //...
    // Iterate over each variadic argument
    for a in @fn.variadic_args {
        match a {
            // Handle 32-bit integer argument
            @typeid(i32) => { 
                // ... 
            }
            // Handle boolean argument
            @typeid(bool) => { 
                // ...
            }
            // Handle string argument
            @typeid(str) => { 
                // ...
            }
            // Handle unsupported argument types
            _ => @error("Unsupported type")
        } 
    }
    // ...
    @inline_c("printf(....)");
}

...
print("Point: x={}, y={}", p.x, p.y);
```
Internally it might be represented as following:
```rust
enum Type {
    I32(i32),
    Bool(bool),
    Str(str),
    // ...
}

fn print(s: &str, args: &[Type]) {
    for a in args {
        match a {
            I32(v) => { ... }
            Bool(v) => { ... }
            Str(v) => { ... }
            _ => magically_generate_compile_error("Unsupported type")
        }
    }
    // compiler will pass this value to the transpiller
    // and it will appear in C generated code
    @inline_c("printf(....)");
}
```
