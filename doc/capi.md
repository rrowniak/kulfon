# CAPI design document

## Intro

Seamless integration with the C ecosystem is essential. To utilize existing C libraries and export functions and types to the C environment, specific steps must be taken.

Currently, several manual steps are required, such as implementing C definitions in a special Kulfon meta language. In the future, these steps will be automated, and you will only need to 'include' a C header like `@capi from "person.h" as cperson;`.

### Introducing C libraries into Kulfon world

Consider the following C interface defined in `person.h` header file:

```C
typedef struct {
    const char* name;
    int age;
} Person;

void print_person(struct Person* p);
```
In Kulfon, we need to declare C stuff that we will be using:

```Rust
@capi from "person.h" as cperson {
    struct Person {
        name: c_str,
        age: c_int,
    }

    fn print_point(p: &mut Person);
}
```
