# Development plan for Kulfon project

## Current status

Kulfon is in early development. The compiler can parse, type-check, and generate C89 code for a subset of the language. All generated code is tested with `gcc -std=c89 -pedantic-errors -Werror`.

## Implemented features

### Parsing
- [done] Function definitions with arguments and return types
- [done] Variable definitions (`let`, `let mut`) with optional type annotations
- [done] Type names (`i32`, `bool`, `str`, etc.)
- [done] Expressions (binary ops, unary ops, parentheses)
- [done] Control flow (`if`/`else if`/`else`, `while`, `loop` with `break`, `for`)
- [done] Function calls
- [done] Struct definitions
- [done] Enum definitions (simple and with payloads)
- [done] Struct initialization (`Point { x: 1, y: 2 }`)
- [done] Enum initialization (`Color :: Red`, `Result :: Ok(42)`)
- [done] Field access (`p.x`)
- [done] Boolean literals (`true`, `false`)
- [done] String and character literals
- [done] Numeric literals (integer, floating-point)

### Type system
- [done] Basic type checking for expressions
- [done] Type inference for variables
- [done] Function call argument type checking
- [done] Variable definition type checking
- [done] Struct field type resolution
- [done] Enum variant type resolution
- [done] Field access type resolution

### C code generation
- [done] Function definitions
- [done] Variable definitions (const/mutable)
- [done] Control flow
- [done] Binary and unary operators
- [done] Function calls
- [done] Struct definitions and initialization
- [done] Enum definitions (simple and tagged unions)
- [done] Field access
- [done] Format strings (`print`/`println` with `{}` placeholders)
- [done] Standard library includes (auto-detected)

### Testing
- [done] Test framework (`tests/test_all.sh`)
- [done] 7 automated tests covering all implemented features
- [done] C89 strict compilation with `-Werror`

## Not yet implemented

### Language features
- [ ] References (`&`, `&mut`)
- [ ] Arrays and slices
- [ ] Pattern matching (`match` expressions)
- [ ] Impl blocks and methods
- [ ] Multiple return values / tuples
- [ ] Range expressions (`0..10`)
- [ ] String concatenation and operations
- [ ] Type aliases
- [ ] Generics
- [ ] Traits / interfaces
- [ ] Move semantics
- [ ] Borrow checker
- [ ] Closures
- [ ] Error handling (`?` operator)

### Infrastructure
- [ ] Multiple files and module system
- [ ] Standard library (collections, I/O, utilities)
- [ ] C API (importing/exporting C functions and types)
- [ ] Better error messages with source context
- [ ] Warnings and diagnostics
- [ ] Optimization passes

### Nice to have
- [ ] Package manager
- [ ] Linter and formatter
- [ ] Language server protocol (LSP)
- [ ] Documentation generator

## Memory safety goals
- [ ] No dangling pointers (stack-based allocation)
- [ ] No use-after-free
- [ ] No memory leaks (for stack-allocated data)
- [ ] No out-of-bounds access
- [ ] No null pointer dereferences
