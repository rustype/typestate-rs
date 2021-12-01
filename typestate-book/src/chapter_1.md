# Introduction

## First steps

Before you start your typestate development journey you need to declare your dependencies,
you can start using the `typestate` crate by adding the following line to your `Cargo.toml` file.

```toml
typestate = "0.8.0"
```

## What are typestates?

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