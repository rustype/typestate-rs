default:
    @just --list

build:
    cargo build

preinstall:
    rustup toolchain install nightly
    cargo install cargo-expand

clean-diagrams:
    rm *.uml *.dot

clean-build:
    cargo clean

@clean:
    just clean-diagrams
    just clean-build

test:
    cargo test --all

fmt:
    cargo fmt --all

# pre-commit only

fmt-check:
    git diff --name-only --cached | grep ".rs" | xargs rustfmt --check --edition 2018 -l

clippy:
    cargo clippy --tests -- -Dclippy::all

# book

serve:
    mdbook serve typestate-book/ --open
