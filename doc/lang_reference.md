# Kulfon language reference

## Status

This document describes the currently implemented features of Kulfon. Features marked with `(planned)` or `(future design)` are not yet implemented.

## Intro

### The most common C and C++ errors that Kulfon is trying to address
* Uninitialized variables. Kulfon: all variables must be initialized before accessing.
* Boolean expression errors. Kulfon: all boolean expression must be explicit, there is no hidden type casting.
* Undefined behaviours. Kulfon: there are no undefined behaviours.
* Returning local objects by reference: Kulfon: lifetime checker prevents from this scenario.
* Dangling pointers: Kulfon - for stack based allocation this is not possible, for heap allocated object this is vastly reduced.
* Invalidated iterators: Kulfon:?
* Type conversions. Kulfon: there are not implicit type conversions.

## Types

### Primitive types (implemented)

| Type | Description |
|------|-------------|
| `bool` | Boolean (`true` or `false`) |
| `i8`, `i16`, `i32`, `i64` | Signed integers |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers |
| `usize`, `isize` | Machine-dependent integers |
| `f32`, `f64` | Floating-point |
| `char` | Single character |
| `rune` | Unicode code point (mapped to `i32`) |
| `str` | String literal |

### Complex types (implemented)

**Structs:**
```kulfon
struct Point { x: i32, y: i32 }
```

**Enums:**
```kulfon
// Simple enum
enum Color { Red, Green, Blue }

// Enum with payloads (generates C tagged union)
enum Result { Ok(i32), Err(i32) }
```

### Planned types
- References (`&T`, `&mut T`)
- Arrays (`[T; N]`)
- Slices (`&[T]`)
- Tuples
- Function pointers
- Closures

## Variables

### Immutable variables
```kulfon
let x = 42;           // type inferred
let y: i32 = 10;      // explicit type
```

### Mutable variables
```kulfon
let mut counter = 0;
counter = counter + 1;
```

## Functions

### Basic functions
```kulfon
fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

### Functions without return value
```kulfon
fn greet(name: &str) {
    println("Hello, {}!", name);
}
```

### Main function
```kulfon
fn main() {
    // program entry point
}
```

### Variadic functions (future design)

Example of how variadic functions might work:
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

Internal representation might be:
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

## Expressions

### Binary operators
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `==`, `!=`, `>`, `>=`, `<`, `<=`
- Logical: `&&`, `||` (planned)

### Unary operators
- Negation: `-`
- Logical not: `!`

### Parenthesized expressions
```kulfon
let result = (a + b) * c;
```

### Field access
```kulfon
struct Point { x: i32, y: i32 }

let p = Point { x: 1, y: 2 };
let x = p.x;        // access field x
let sum = p.x + p.y; // use in expressions
```

### Function calls
```kulfon
let result = add(3, 4);
println("{}", result);
```

## Control flow

### If/else
```kulfon
if condition {
    // ...
} else if other_condition {
    // ...
} else {
    // ...
}
```

### While loop
```kulfon
let mut i = 0;
while i < 10 {
    println("{}", i);
    i = i + 1;
}
```

### Loop with break
```kulfon
let mut count = 0;
loop {
    if count >= 5 {
        break;
    }
    count = count + 1;
}
```

### For loop (planned — syntax parsed but limited)
```kulfon
for item in collection {
    // ...
}
```

## Structs

### Definition
```kulfon
struct Point { x: i32, y: i32 }
struct Person { name: str, age: i32 }
```

### Initialization
```kulfon
let p = Point { x: 1, y: 2 };
let person = Person { name: "Alice", age: 30 };
```

### Field access
```kulfon
println("{}", p.x);
let sum = p.x + p.y;
```

### Planned features
- Methods via `impl` blocks
- Struct update syntax (`Point { x: 1, ..other }`)
- Tuple structs
- Unit structs

## Enums

### Simple enums
```kulfon
enum Direction { North, South, East, West }

let dir = Direction :: North;
```

### Enums with payloads (tagged unions)
```kulfon
enum Result { Ok(i32), Err(i32) }

let success = Result :: Ok(42);
let failure = Result :: Err(1);
```

C code generation:
- Simple enums → C `enum`
- Enums with payloads → C `struct` with tag + union

### Planned features
- Pattern matching (`match` expressions)
- Methods via `impl` blocks
- Associated functions

## Built-in functions

### print
```kulfon
print("Hello");           // no newline
print("x = {}", 42);      // with format argument
```

### println
```kulfon
println("Hello");         // with newline
println("x = {}", 42);    // with format argument
```

### Format string syntax
- `{}` — placeholder for the next argument
- Supported argument types: `i32`, `bool`, `str`, `char`, floats, unsigned ints

Examples:
```kulfon
println("{}", 42);           // prints: 42
println("{}", true);         // prints: true
println("{}", "hello");      // prints: hello
println("x = {}, y = {}", 1, 2);  // prints: x = 1, y = 2
```

## C code generation

Kulfon generates C89-compatible code. The generated code is compiled with strict flags:
```
gcc -std=c89 -pedantic-errors -Wall -Wextra -Werror
```

### Type mapping

| Kulfon | C |
|--------|-----|
| `bool` | `kf_boolean` (C enum) |
| `i8`-`i64` | `int8_t`-`int64_t` |
| `u8`-`u64` | `uint8_t`-`uint64_t` |
| `usize` | `size_t` |
| `isize` | `ssize_t` |
| `f32` | `float` |
| `f64` | `double` |
| `char` | `char` |
| `rune` | `int32_t` |
| `str` | `const char*` |

### Struct mapping
```kulfon
struct Point { x: i32, y: i32 }
```
→
```c
struct Point {
    int32_t x;
    int32_t y;
};
```

### Enum mapping
```kulfon
enum Color { Red, Green, Blue }
```
→
```c
enum Color { Color_Red, Color_Green, Color_Blue };
```

### Tagged union mapping
```kulfon
enum Result { Ok(i32), Err(i32) }
```
→
```c
enum Result_tag {
    Result_Ok,
    Result_Err
};

struct Result {
    enum Result_tag tag;
    union {
        int32_t Ok;
        int32_t Err;
    } payload;
};
```

## Examples

### Hello world
```kulfon
fn main() {
    println("Hello world!");
}
```

### Fibonacci
```kulfon
fn main() {
    let mut a = 0;
    let mut b = 1;
    let mut i = 0;
    while i < 10 {
        println("{}", a);
        let temp = a + b;
        a = b;
        b = temp;
        i = i + 1;
    }
}
```

### Struct with field access
```kulfon
struct Point { x: i32, y: i32 }

fn distance_squared(p: Point, q: Point) -> i32 {
    let dx = p.x - q.x;
    let dy = p.y - q.y;
    dx * dx + dy * dy
}

fn main() {
    let a = Point { x: 0, y: 0 };
    let b = Point { x: 3, y: 4 };
    println("{}", distance_squared(a, b));  // prints: 25
}
```
