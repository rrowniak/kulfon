# kulfon lang

This is a research project.

Experiments with context free grammars (CFG) and attempts to create a new language.

## Assumptions
- This is going to be a transpiler, that is, the code will be generated to pure `C` language.
- The goal is to create a simple but very practical language.
- The language is inspired by others like Rust, C, C++, Python, Go.

## High level goals for Kulfon language
- Minimal syntax inspired by Rust, C and Go. No complex and weird constructs. No _innovations_. Just what you've probably seen before. Less is more.
- This is a strongly typed language. 
- Zero cost abstractions. You write what you want, Kulfon is trying hard to make the safest and most efficient code.
- Platform independent, general purpose language. The only thing Kulfon needs is a C compiler. 
- Memory safe to some rational extend. E.g. the is no possibility to create a dangling pointer. Lifetimes is something that Kulfon takes care of and notifies you in case of issues, you don't need to specify anything like in Rust.
- No undefined behaviours.
- Designed for easy development, prototyping, frequent iterations. 
- Easy integration with `C` libraries - instead of rewriting whole world just use what's already proven.
- Many features that are common for modern languages like built-in unit tests or modules.

## Examples
### Hello world
Hello world example in Kulfon:
```rust
fn main() {
    println("Hello world!");
}
```

### Control flow

`if` statent
```rust
let isCrazyMurderingRobot = false;

fn interact_with_human() {
    // no, isCrazyMurderingRobot = true won't compile :)
    if isCrazyMurderingRobot == true {
        kill(humans);
    } else {
        be_nice_to(human);
    }
}
```

Loops
```rust
fn shoot_for_the_moon() -> bool {
    for countdown in [10..1] {
        say(countdown);
    }

    while in_athmosphere() {
        keep_accelerating();
        if max_performance() {
            break;
        }
    }

    let mission_accomplished = loop {
        if look_around() == Target::Moon {
            break true;
        }

        do_daily_routines();
        landed_among_start() || break false;
    };

    mission_accomplished
