# kulfon lang

This is a research project.

Experiments with context free grammars (CFG) and attempts to create a new language.

More to read:
- [Types and memory model](./doc/types.md)
- [BNF-like grammar for Kulfon](./doc/bnf.md)
- [Project roadmap](./doc/roadmap.md)

## Assumptions
- This is a transpiler, that is, the Kuflon code is compiled to pure `C` language (C89).
- The goal is to create a simple and very practical language.
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

## Benefits of being a C transpiler
- Hard to imagine a platform that does not have a dedicated C compiler. Now you can use Kulfon there.
- Regulatory and compliance - you can still meet your standards as finally you will be using your certified C compiler.
- C compilers are trusted as they're with us since 1970. Let's take advantage of that.
- We don't need to reivent the wheel by implementing heavy optimization techniques on assembly/machine level.
- Instead of trusing your shiny new XYZ compiler that generates machine code directly, you can immediately understand what Kulfon compiler is doing by looking at C generated code. Moreover, you can run your favorite static analysis tools against generated C code.
- You can integrate Kulfon with existing C or C++ code base, there is no need to create a new project. Bring some fun and safety to your legacy stuff!
- You're afraid that Kulfon is not what you're looking for? Remember that a half-product is C code. If you start with Kulfon and (unlikely scenario) abandon it at some point, your effort doesn't need to be thrown away, the development might be continued on C codebase.

## Examples
### Hello world
Hello world example in Kulfon:
```rust
fn main() {
    println("Hello world!");
}
```

### Control flow

`if` statement
```rust
let isCrazyMurderingRobot = false;
let humans = ["Alice", "Bob", "Ewa"];

fn kill(humans: [str]) {
    for human in humans {
        print("Killing {human}!");
    }
}

fn be_nice_to(people: [str]) {
    print("Welcome all: {people}!");
}

fn interact_with_human() {
    // no, isCrazyMurderingRobot = true won't compile :)
    if isCrazyMurderingRobot == true {
        kill(humans);
    } else {
        be_nice_to(humans);
    }
}
```

Loops
```rust
fn shoot_for_the_moon() -> bool {
    for countdown in 10..=1 {
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
        landed_among_stars() || break false;
    };

    mission_accomplished
}
```
