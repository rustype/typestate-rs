# Contributing

This project welcomes contributions under (but not limited to) the following categories:

- Code
    - `typestate` macro codebase
    - The actions pipeline
    - Other code present in the repository
    (e.g. `justfile` or in the future, scripts to "compile" diagrams)
- Documentation
    - Examples
    - Documentation for the macro
    - *The Typestate Book*
- Reports
    - Bugs
    - Missing functionality (e.g.
        [#5](https://github.com/rustype/typestate-rs/issues/5) &
        [#8](https://github.com/rustype/typestate-rs/issues/8))
    - Suggestions (e.g.
        [#2](https://github.com/rustype/typestate-rs/issues/2))
    - Relevant discussions (e.g.
        [#3](https://github.com/rustype/typestate-rs/issues/3))
    - Questions

> A single contribution may fall under one or more of the previous categories.

## Git Message Style

The repository git message style is based on Karma's style (read more in <http://karma-runner.github.io/6.3/dev/git-commit-msg.html>),
to reduce confusion, we use the following subset:

- `feat` for a new feature.
- `fix` for a bug fix or other minor code modifications.
  This includes the following labels from Karma's style:
    - `perf`
    - `style`
    - `refactor`
    - `test`
    - `build`
    - `chore`
- `docs` for documentation modifications.
- `book` for *The Typestate Book* modifications. (Note: these may be included under `docs`)

### Addressing Issues

Commits addressing specific issues *must* contain the addressed issue.

As an example, consider you were fixing issue `#10`, the message should look as follows:

```
feat(#10): message goes here
```

> Addressing multiple issues in a single commit is discouraged,
> in such cases, a bigger issue which tracks the progress on the smaller tasks should be created.

### Breaking Changes

Commits implementing breaking changes should contain a `!` in the end of the commit label (e.g. `feat!(#10):...`).

## Final Note

The objective of this document is to *reduce friction*,
as such the contents in this document constitute a *guide* rather than a set of rules.
