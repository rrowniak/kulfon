# kulfon lang

This is a research project.

Experiments with context free grammars (CFG) and attempts to create a new language.

More to read:
- [Types and memory model](./doc/types.md)
- [BNF-like grammar for Kulfon](./doc/bnf.md)
- [Project roadmap](./doc/roadmap.md)
- [Language reference](./doc/lang_reference.md)

## Assumptions
- This is a transpiler, that is, the Kulfon code is compiled to pure `C` language (C89).
- The goal is to create a simple and very practical language.
- The language is inspired by others like Rust, Zig, C, C++, Python, Go.

## High level goals for Kulfon language
- Minimal syntax inspired by Zig, Rust, C and Go. No complex and weird constructs. No _innovations_. Just what you've probably seen before. Less is more.
- This is a strongly typed language. 
- Zero cost abstractions. You write what you want, Kulfon is trying hard to make the safest and most efficient code.
- Platform independent, general purpose language. The only thing Kulfon needs is a C compiler.
- Memory safe to some rational extend. E.g. the is no possibility to create a dangling pointer. Lifetimes is something that Kulfon takes care of and notifies you in case of issues, you don't need to specify anything like in Rust.
- No undefined behaviours.
- Designed for easy development, prototyping, frequent iterations.
- Easy integration with `C` libraries - instead of rewriting whole world just use what's already proven.

## Benefits of being a C transpiler
- Hard to imagine a platform that does not have a dedicated C compiler. Now you can use Kulfon there.
- Regulatory and compliance - you can still meet your standards as finally you will be using your certified C compiler.
- C compilers are trusted as they're with us since 1970. Let's take advantage of that.
- We don't need to reinvent the wheel by implementing heavy optimization techniques on assembly/machine level.
- Instead of trusting your shiny new XYZ compiler that generates machine code directly, you can immediately understand what Kulfon compiler is doing by looking at C generated code. Moreover, you can run your favorite static analysis tools against generated C code.
- You can integrate Kulfon with existing C or C++ code base, there is no need to create a new project. Bring some fun and safety to your legacy stuff!
- You're afraid that Kulfon is not what you're looking for? Remember that a half-product is C code. If you start with Kulfon and (unlikely scenario) abandon it at some point, your effort doesn't need to be thrown away, the development might be continued on C codebase.

## Current status

Kulfon is in early development stage. The following features are implemented:

- Functions with arguments and return types
- Primitive types: `bool`, `i8`-`i64`, `u8`-`u64`, `usize`, `isize`, `f32`, `f64`, `char`, `rune`, `str`
- Variables: immutable (`let`) and mutable (`let mut`)
- Type annotations and type inference
- Control flow: `if`/`else if`/`else`, `while`, `loop` with `break`, `for`
- Binary operators: `+`, `-`, `*`, `/`, `==`, `!=`, `>`, `>=`, `<`, `<=`
- Unary operators: `!`, `-`
- Structs: definition, initialization, field access
- Enums: definition (simple and with payloads)
- `print()` and `println()` with format strings

## Examples

### Hello world
```kulfon
fn main() {
    println("Hello world!");
}
```

### Variables and type inference
```kulfon
fn main() {
    let x: i32 = 42;       // explicit type
    let y = 10;            // type inferred as i32
    let mut counter = 0;   // mutable variable
    counter = counter + 1;
}
```

### Functions
```kulfon
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let result = add(3, 4);
    println("{}", result);
}
```

### Control flow
```kulfon
fn main() {
    let x = 10;

    if x > 5 {
        println("big");
    } else if x > 0 {
        println("small positive");
    } else {
        println("non-positive");
    }

    let mut i = 0;
    while i < 5 {
        println("{}", i);
        i = i + 1;
    }

    let mut count = 0;
    loop {
        if count >= 3 {
            break;
        }
        count = count + 1;
    }
}
```

### Structs and field access
```kulfon
struct Point { x: i32, y: i32 }

fn main() {
    let p = Point { x: 3, y: 7 };
    println("x = {}", p.x);
    println("y = {}", p.y);
    println("sum = {}", p.x + p.y);
}
```

### Enums
```kulfon
enum Direction { North, South, East, West }

enum Result { Ok(i32), Err(i32) }

fn main() {
    let dir = Direction :: North;
    let r = Result :: Ok(42);
}
```

### Format strings
```kulfon
fn main() {
    let name = "World";
    let value = 42;
    println("Hello, {}!", name);
    print("The answer is {}", value);
}
```

## Building

### Prerequisites
- Rust toolchain (for building the compiler)
- GCC (for compiling generated C code)

### Build the compiler
```bash
cd kf
cargo build
```

### Run tests
```bash
bash tests/test_all.sh
```

### Compile a Kulfon file
```bash
kf/target/debug/kf compile -i input.kf -o output.c
gcc -std=c89 -pedantic-errors output.c -o output
./output
```

## Project structure
```
kulfon/
├── kf/                    # Compiler source (Rust)
│   └── src/
│       ├── main.rs        # CLI entry point
│       ├── ast.rs         # Abstract syntax tree definitions
│       ├── parser.rs      # Parser (tokens → AST)
│       ├── kf_core.rs     # Type checker and semantic analysis
│       ├── type_system.rs # Type definitions and utilities
│       └── cbackend/      # C code generation
│           ├── cbackend.rs
│           └── generators.rs
├── tests/                 # Test files
│   ├── test_all.sh        # Test runner
│   └── test*.kf           # Kulfon test programs
└── doc/                   # Documentation
```
