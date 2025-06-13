# Working with the Outrun Project

## Project Overview

Outrun is a statically-typed, functional programming language built around the concept of traits. The language emphasises immutability, named parameters, and a powerful guard system for control flow.

## Project Structure

```
outrun/
├── README.md                    # Project discussion and background
├── LANGUAGE_SPEC.md             # Complete language syntax specification
├── GRAMMAR.bnf                 # Formal BNF grammar
├── outrun.toml                 # Package manifest (when created)
└── outrun-parser/              # Parser implementation (Rust)
    ├── Cargo.toml
    └── src/
        └── lib.rs
```

## Core Design Principles

- **Everything is traits** - All types implement traits, which define behaviour
- **Named arguments only** - No positional arguments in function calls  
- **Static typing** with trait constraints and guards
- **Immutable and functional** - No mutation, rebinding allowed
- **Actor model runtime** - Built for concurrent, distributed systems

## Key Language Features

### Trait-Based Everything
All operators, control flow, and behaviour is defined through traits:
- `+` calls `BinaryAddition.add()`
- `|>` calls `Pipe.pipe_into()`
- `|?` calls `Maybe.maybe_pipe()`
- `@Attribute()` calls `Attribute.apply()`
- `~Sigil""` calls `Sigil.parse()`

### Module System
- Types (structs/traits) ARE modules
- File structure maps to module hierarchy: `src/http/client.outrun` → `Http.Client`
- Multiple types per file all belong to the same module namespace
- Constants are always private; use functions to expose values

### Guard System
Functions ending in `?` must be side-effect-free and return Boolean:
```outrun
def divide(a: Integer, b: Integer): Float 
when Integer.non_zero?(b) {
    Float.from_integer(a) / Float.from_integer(b)
}
```

### Hygienic Macros
Same syntax as the language, using `^` for argument injection:
```outrun
macro unless(condition, do_block) {
    if !^condition {
        ^do_block
    }
}
```

## Development Workflow

### Adding New Syntax

1. **Update LANGUAGE_SPEC.md** with examples and explanation
2. **Update GRAMMAR.bnf** with formal grammar rules
3. **Add test cases** in `outrun-parser/tests/`
4. **Update Pest grammar** if needed

### Parser Development

The Rust parser is in `outrun-parser/`. This will eventually become the main compiler frontend.

```bash
cd outrun-parser
cargo build
cargo test
```

## File Extensions

- Source files: `.outrun`
- Package manifest: `outrun.toml`

## Standard Library

Core traits live in the `Outrun.` namespace to avoid conflicts:
- `Outrun.Option<T>`
- `Outrun.Result<T, E>`
- `Outrun.Iterator<T>`
- `Outrun.BinaryAddition<T>`
- etc.

Users must explicitly alias them:
```outrun
alias Outrun.Option as Option
alias Outrun.Result as Result
```

## Testing Syntax Ideas

Create test files in `outrun-parser/tests/` to validate syntax using Rust unit tests.

## Current Status

- ✅ Core syntax specification complete
- ✅ BNF grammar written
- ✅ **All LANGUAGE_SPEC.md features implemented and tested**
- ✅ **Pest parser implementation**: Complete string interpolation with expression parsing
- ✅ **CLI tool**: Parse command with pretty-printed s-expressions and stdin support
- ✅ **Recursive destructuring patterns**: Fully implemented with unified pattern system across let bindings, case statements, and function parameters
- ⭐ Type checker needed  
- ⭐ Interpreter/compiler needed

## Contributing

When working on the language:

1. **Syntax changes** require updates to both LANGUAGE_SPEC.md and GRAMMAR.bnf
2. **Test thoroughly** with Rust unit tests
3. **Keep "everything is traits"** philosophy consistent
4. **Maintain immutability** and functional approach
5. **Document design decisions** in commit messages

## Useful Commands

```bash
# Format all code across all rust subprojects.
cargo fmt

# Run tests across all rust subprojects.
cargo test

# Run lints across all rust subprojects
cargo clippy --all-targets --all-features -- -D warnings

# Create pull request with fj CLI tool
fj pr create "PR title" --body "PR description"

# Push branch and create PR in one go
git push -u origin branch-name && fj pr create "Title" --body "Description"
```
