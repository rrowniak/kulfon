# Development plan for Kulfon project

## Highest priority

### Finishing language parsing
- structs (def + impl)
- enums
- arrays & slices
- parsing matching
- missing operators
- format string parsing

### Type system
- checking types
- checking type conversion and correctness
- struct type checking
- array type checking
- enum type checking
- borrow checker, lifetime guard
- move semantics 

## Next in the queue

### generics
- parsing generics
- type deducing & checking

### traits aka interfaces
- design the concept

### matching
- implement matching system
- C code generator

### compiler API

### heap allocation
- concept
- smart prts

### STD
- collections
- io (files, sockets)
- miscelanous (rand)
- string utils
- primitive types utils

### C binding system
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
