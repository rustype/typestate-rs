# Compilation Flags

The `typestate` macro provides several `cargo` features,
mostly focused on the visualization of your typestate's automata.

## Mermaid Diagrams

`docs-mermaid` will embed [Mermaid.js](https://mermaid-js.github.io/mermaid/#/) diagrams in your documentation.

This feature is activated by default, regarless, see below how you can explicitly activate it.

In the terminal, for each run:
```bash
cargo doc --features docs-mermaid
```

Or by declaring it in `Cargo.toml`:
```toml
typestate = { version = "0.8.0", features = [ "docs-mermaid" ] }
```

## DOT Diagrams

`export-dot` - will generate a `.dot` file, describing your typestate's state machine.
You can customize certain `.dot` parameters through the following environment variables:

- `DOT_PAD` - specifies how much, in inches, to extend the drawing area around the minimal area needed to draw the graph.
- `DOT_NODESEP` - `nodesep` specifies the minimum space between two adjacent nodes in the same rank, in inches.
- `DOT_RANKSEP` - sets the desired rank separation, in inches.
- `EXPORT_FOLDER` - declare the target folder for exported files.

This feature is not activated by default, see below how you can activate it.

In the terminal, for each run:
```bash
cargo doc --features export-dot
```

Or by declaring it in `Cargo.toml`:
```toml
typestate = { version = "0.8.0", features = [ "export-dot" ] }
```

> For more information on DOT configuration, I recommend you read through [DOT documentation](https://graphviz.org/doc/info/attrs.html).

### Examples

These examples are present in the `examples/` folder in the repository's root.

| `LightBulb`                                          | `SmartBulb`                                          |
| ---------------------------------------------------- | ---------------------------------------------------- |
| ![`examples/light_bulb.rs`](../static/DotLightBulb.svg) | ![`examples/smart_bulb.rs`](../static/DotSmartBulb.svg) |

## PlantUML Diagrams

`export-plantuml` will generate a PlantUML state diagram (`.uml` file) of your state machine.
Like the previous feature, you can also customize this one through the following environment variables:

- `PLANTUML_NODESEP` - `nodesep` specifies the minimum space between two adjacent nodes in the same rank.
- `PLANTUML_RANKSEP` - Sets the desired rank separation.
- `EXPORT_FOLDER` - Declare the target folder for exported files.

This feature is not activated by default, see below how you can activate it.

In the terminal, for each run:
```bash
cargo doc --features export-plantuml
```

Or by declaring it in `Cargo.toml`:
```toml
typestate = { version = "0.8.0", features = [ "export-plantuml" ] }
```

> For more information on PlantUML configuration, I recommend you read through [PlantUML Hitchhiker's Guide](https://crashedmind.github.io/PlantUMLHitchhikersGuide/layout/layout.html#nodesep-and-ranksep).

### Examples

These examples are present in the `examples/` folder in the repository's root.

| `LightBulb`                                          | `SmartBulb`                                          |
| ---------------------------------------------------- | ---------------------------------------------------- |
| ![`examples/light_bulb.rs`](../static/UmlLightBulb.svg) | ![`examples/smart_bulb.rs`](../static/UmlSmartBulb.svg) |