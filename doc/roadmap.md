# Development plan for Kulfon project

## Highest priority

### Finishing language parsing
- [done] structs (struct + impl)
- [done] enums (enum)
- [done] arrays & slices ([])
- [done] basic format string parsing
- [done] variable length function parsing
- [done] type parsing (literal names, references, arrays, slices, generic types, function pointers)
- parsing struct and enum initialization
- parsing array initialization

### Type system
- [done] basic checking types
- checking type conversion
- [done] functions calls
- [done] variable definitions and mutability checks
- struct type checking
- array type checking
- enum type checking
- references and slices
- borrow checker, lifetime guard
- move semantics 

### Other
- [done] refactored compile errors handling
- [done] predefined std functions like `print` and `println`

### C binding system CAPI-IN
- importing primitive types and functions
- pointers
- importing enums and defines
- importing structures

## Next in the queue

- struct, enum and function annotations (@annotation)
- missing operators
- parsing matching patterns
- range struct (0..20 => Range{start:0, end:20})

### generics
- parsing generics
- type deducing & checking

### traits aka interfaces
- design the concept
- implement support
- dynamic traits/interfaces (vtable)

### matching
- implement matching system
- C code generator

### compiler API
- enforce compile time
- sizeof etc
- suppress warnings
- compile time flow control

### heap allocation
- concept
- smart prts

### types
- struct deconstructing

### STD
- collections
- io (files, sockets)
- miscelanous (rand)
- string utils
- primitive types utils
- advanced string parsing format

### C binding system CAPI-OUT
- exporting types and functions

## Nice to have so far

### STD
- high level libraries
    - net
    - http server
    - crypto

### Kulfon project
- multifile compilation
- different targets

### Kulfon package manager

### Kulfon tools
- linter
- formatter

# Memory issues to be solved
- after free problem
- memory leaks
- out of bound access
- NULL pointer deref
