# What are typestates?

In a nutshell, typestates are finite state machines described at the type-level.
They aim to tame stateful computations by allowing the compiler to reason about the state of the program.

Consider the following Java example:

```java
public class ScannerFail {
    void main(String... args) {
        Scanner s = new Scanner(System.in);
        s.close();
        s.nextLine();
    }
}
```

The example will compile and run, however it will crash during runtime, throwing an `IllegalStateException`, 
this happens because we tried to read a line after closing the `Scanner`.

If you thought: "*The compiler should have told me!*" - then, typestates are for you!

In a typestated language, `Scanner` would have its state be a first-class citizen of the code.
Consider the following example in *typestated-Java*:

```java
public class ScannerFail {
    void main(String... args) {
        Scanner[Open] s = new Scanner(System.in);
        // s now has type Scanner[Closed]
        s = s.close();
        // compilation error: Scanner[Closed] does not have a nextLine method
        s.nextLine();
    }
}
```

As made evident by the comments, the example would not compile because the `Scanner` 
transitions to the `Closed` state after the `.close()` call.

## Typestates in Rust

Typestates are not a new concept to Rust.
There are several blog posts on the subject
[[1](https://yoric.github.io/post/rust-typestate/),
[2](http://cliffle.com/blog/rust-typestate/),
[3](https://rustype.github.io/notes/notes/rust-typestate-series/rust-typestate-index)]
as well as a [chapter](https://docs.rust-embedded.org/book/static-guarantees/typestate-programming.html) in *The Embedded Rust Book*.

In short, we can write typestates by hand, we add some generics here and there,
declare them as a "*state*" and in the end we can keep living our lives with our new state machine.

This approach however is *error-prone* and *verbose* (especially with bigger automata).
It also provides *no* guarantees about the automata, unless of course, you designed and tested the design previously.

As programmers, we want to automate this cumbersome job and to do so, we use Rust's powerful procedural macros!