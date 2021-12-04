# `#[typestate]`

[<img alt="github" src="https://img.shields.io/badge/github-rustype/typestate-8da0cb?style=flat-square&logo=github">](https://github.com/rustype/typestate-rs)
[<img alt="" src="https://img.shields.io/badge/docs.rs-typestate-success?style=flat-square">](https://docs.rs/typestate)
[<img alt="" src="https://img.shields.io/crates/v/typestate?style=flat-square">](https://crates.io/crates/typestate)

This library provides developers with a macro to design typestated objects.

```toml
[dependencies]
typestate = "0.8.0"
```

*Compiler support: requires rustc 1.42+*

## Documentation

If you're only interested in getting up and running with `typestate`, 
the documentation might be more useful for you. 
You can consult it in <https://docs.rs/typestate/0.8.0/typestate/>

If you're interested in learning more about the `typestate` crate, or typestates in Rust, 
you can read *The Typestate Book* in <https://rustype.github.io/typestate-rs/>.

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

## Publications

- [Retrofitting Typestates into Rust (SBLP'21)](paper/sblp21.pdf)