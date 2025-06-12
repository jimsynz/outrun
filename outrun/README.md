# Outrun CLI

Beautiful command-line interface for the Outrun programming language with syntax-highlighted error reporting.

## Building

The CLI uses embedded syntax highlighting for fast startup. The syntax definition is automatically generated from the YAML source during build:

```bash
# Build CLI with embedded syntax highlighting (default)
# Automatically generates outrun_syntax.dump from outrun.sublime-syntax
cargo build

# Build CLI with file-based syntax loading (development)
cargo build --no-default-features --features file-syntax

# Manual syntax generation (optional, for development)
cargo run --bin build_syntax
```

## Features

- **Fast startup**: ~0.17s with embedded syntax highlighting
- **Beautiful errors**: Syntax-highlighted error messages with miette
- **Professional output**: Colored keywords, strings, and comments
- **Zero dependencies**: No external files needed (embedded mode)

## Usage

```bash
# Parse Outrun files
./target/debug/outrun parse file.outrun

# Parse from stdin
echo 'def hello(): String { "world" }' | ./target/debug/outrun parse -

# Show detailed AST with spans
./target/debug/outrun parse --spans file.outrun
```