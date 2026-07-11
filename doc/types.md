# Kulfon types and memory model

## Status

This document describes the type system and memory model. Items marked with `(planned)` are not yet implemented.

## Memory model

The Kulfon language currently supports stack-based memory allocation for local variables and function arguments.

### Planned memory features
- Dynamic memory allocation (heap)
- Move semantics
- Borrow checker and lifetime tracking
- Smart pointers

## Primitive types (implemented)

### Boolean
`bool` — values: `true` or `false`

### Integers
Signed: `i8`, `i16`, `i32`, `i64`
Unsigned: `u8`, `u16`, `u32`, `u64`

Note: `i128` and `u128` are not yet supported.

### Machine-dependent integers
`usize`, `isize`

### Floating-point
`f32`, `f64`

### Textual
`char` — single character
`rune` — Unicode code point (mapped to `i32`)
`str` — string literal (mapped to `const char*` in C)

## Complex types (implemented)

### Structs
```kulfon
struct Point { x: i32, y: i32 }
```

Properties:
- Stack-allocated
- Copy by value semantics
- Fields accessed with dot notation (`p.x`)

### Enums
```kulfon
// Simple enum
enum Color { Red, Green, Blue }

// Enum with payloads
enum Result { Ok(i32), Err(i32) }
```

Properties:
- Simple enums → C `enum` (all variants are unit variants)
- Enums with payloads → C tagged union (struct with tag + union)
- Variants accessed with `::` syntax (`Color :: Red`)

## Planned types

### References
```kulfon
let r: &i32 = &x;      // immutable reference
let r: &mut i32 = &mut x;  // mutable reference
```

### Arrays
```kulfon
let arr: [i32; 3] = [1, 2, 3];
let first = arr[0];
```

### Slices
```kulfon
let slice: &[i32] = &arr[0..2];
```

### Tuples
```kulfon
let pair: (i32, bool) = (42, true);
let (x, y) = pair;
```

### Function pointers
```kulfon
let f: fn(i32, i32) -> i32 = add;
```

### Closures (planned)
```kulfon
let add_one = |x: i32| -> i32 { x + 1 };
```

## Type system

### Type inference
Kulfon infers types when not explicitly specified:
```kulfon
let x = 42;        // inferred as i32
let y = 3.14;      // inferred as f64
let s = "hello";   // inferred as str
```

### Explicit types
Type annotations are optional but recommended for clarity:
```kulfon
let x: i32 = 42;
let y: f64 = 3.14;
```

### Type checking
- All variables must be initialized before use
- Function arguments must match declared types
- Return types must match declared types
- No implicit type conversions

## C type mapping

| Kulfon | C type | Notes |
|--------|--------|-------|
| `bool` | `kf_boolean` | C enum type |
| `i8` | `int8_t` | |
| `i16` | `int16_t` | |
| `i32` | `int32_t` | |
| `i64` | `int64_t` | |
| `u8` | `uint8_t` | |
| `u16` | `uint16_t` | |
| `u32` | `uint32_t` | |
| `u64` | `uint64_t` | |
| `usize` | `size_t` | |
| `isize` | `ssize_t` | |
| `f32` | `float` | |
| `f64` | `double` | |
| `char` | `char` | |
| `rune` | `int32_t` | |
| `str` | `const char*` | |

Note: Kulfon targets C89 but uses `<stdint.h>` types which are C99. The compiler generates the necessary includes automatically. See [this discussion](https://stackoverflow.com/questions/62937049/stdint-h-in-ansi-c-c89) for details on using `stdint.h` with C89 compilers.

## Struct C mapping

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

## Enum C mapping

### Simple enums
```kulfon
enum Color { Red, Green, Blue }
```
→
```c
enum Color { Color_Red, Color_Green, Color_Blue };
```

### Enums with payloads
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
