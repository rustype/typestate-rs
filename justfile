preinstall:
    rustup toolchain install nightly
    cargo install cargo-expand

clean-diagrams:
    rm *.uml *.dot

test:
    cargo test --all

fmt:
    cargo fmt --all

build:
    cargo build

# pre-commit only
fmt-check:
    git diff --name-only --cached | grep ".rs" | xargs rustfmt --check --edition 2018 -l

clippy:
    cargo clippy --tests -- -Dclippy::all