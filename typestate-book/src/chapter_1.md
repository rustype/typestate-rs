# Introduction

Typestates allow you to define *safe* usage protocols for your objects.
The compiler will help you on your journey and disallow errors on given states.
You will no longer be able to try and read from closed streams.

`#[typestate]` builds on ideas from the [`state_machine_future`](https://github.com/fitzgen/state_machine_future) crate.

## First steps

Before you start your typestate development journey you need to declare your dependencies,
you can start using the `typestate` crate by adding the following line to your `Cargo.toml` file.

```toml
typestate = "0.8.0"
```

## Citing `typestate`

If you find `typestate` useful in your work, we kindly request you cite the following paper:

```bibtex
@inproceedings{10.1145/3475061.3475082,
    author = {Duarte, Jos\'{e} and Ravara, Ant\'{o}nio},
    title = {Retrofitting Typestates into Rust},
    year = {2021},
    url = {https://doi.org/10.1145/3475061.3475082},
    doi = {10.1145/3475061.3475082},
    booktitle = {25th Brazilian Symposium on Programming Languages},
    pages = {83–91},
    numpages = {9},
    series = {SBLP'21}
}
```