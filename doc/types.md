# Kulfon memory model
## Plain old data (POD)
Variables are allocated on the stack, copy by value semantics is employed. POD variables are not moveable.

# Kulfon built-in primitive types
## Boolean

`bool` 

## Integer

`u8`, `u16`, `u32`, `u64`, `u128`, `i8`, `i16`, `i32`, `i64`, `i128`

## Machine-dependent integer types

`usize`, `isize`

## Floating-point

`f32`, `f64`

## Textual

`char`, `rune`

# Kulfon complex types

## Never type
`!`

## Arrays
ArrayType: `[` Type `;` Expression `]`
`let array: [f32; 3] = [0.5, 0.8, 1.0]`

## Slices

## Structs

## Enums

## Unions

## Aliases
`alias TempT = f32;`

## Type derivation
`type TempT = f32;`

## Inferred types
`let spins: Vec<_> = [0.5, -0.5, 0.5];`

# C mapping

This mapping suggests conformance with C99 while Kulfon promises to be compliant with C89. Depending on the environment, Kulfon might pregenerate these types if they're missing to be compliant with C89, or, you need to provide correct mapping in the platform configuration.

[See here](https://stackoverflow.com/questions/62937049/stdint-h-in-ansi-c-c89) and [here](https://stackoverflow.com/questions/44590043/why-is-generic-keyword-supported-in-c99-or-c90-modes/44590122#44590122) for reference.


| Kulfon type | C-type      | Comments |
|-------------|-------------|----------|
|`bool`       |`kf_boolean` |C-enum type|
| `u8`        | `uint8_t`||
|`u16`        | `uint16_t`||
|`u32`        | `uint32_t`||
|`u64`        | `uint64_t`||
|`u128`       | |Not supported yet|
|`i8`         | `int8_t`|| 
|`i16`        | `int16_t`||
|`i32`        | `int32_t`||
|`i64`        | `int64_t`||
|`i128`       | |Not supported yet|
|`usize`      | `size_t`||
|`isize`      | `ssize_t`||
|`f32`        | `float` ||
|`f64`        | `double` ||
|`char`       | `char` ||
|`rune`       | `int32_t` ||
