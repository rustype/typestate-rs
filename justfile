clean-diagrams:
    rm *.uml *.dot

test:
    cargo test --all

clippy:
    cargo clippy --tests -- -Dclippy::all

fmt:
    cargo fmt --all

build:
    cargo build