# kulfon lang
Experiments with context free grammars (CFG) and attempts to create a new language.

## Assumptions
- This is going to be a transpiler, that is, the code will be generated to pure `C` language.
- The goal is to create a simple but very usable language.
- The language is inspired by others like Rust, C, C++, Python, Go.

## High level goals for Kulfon language
- Minimal Rust-like syntax
    - Not compatible with Rust
    - Concepts like lifetime syntax not present
- Strongly typed
- Platform independent, general purpose language
- Memory safe to some rational extend
- No undefined behaviours
- Designed for easy development, prototyping, frequent iterations
- Easy integration with `C` libraries - instead of rewriting whole world just use what's already proven

## Hello world
Hello world example in Kulfon:
```rust
fn main() {
    print("Hello world!");
}
```
