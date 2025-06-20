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
├── outrun-parser/              # Parser implementation (Rust)
│   ├── CLAUDE.md               # Parser-specific development guide
│   ├── Cargo.toml
│   ├── src/
│   └── tests/                  # Parser integration tests
└── outrun-typechecker/         # Type checker implementation (Rust)
    ├── CLAUDE.md               # Type checker development guide
    ├── Cargo.toml
    ├── src/
    └── tests/                  # Type checker integration tests
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

### Notes to reduce confusion:
- Outrun is functional, not OOP. There are no methods, instances, objects or classes.
- All functions take keyword arguments.
- All function calls must provide keyword arguments (althought shorthand is available).
- There are no Enum or union types. Instead define structs and implement the same trait for them.
- The only constructors are struct constructors.

## Development Workflow

### Adding New Syntax

1. **Update LANGUAGE_SPEC.md** with examples and explanation
2. **Update GRAMMAR.bnf** with formal grammar rules
3. **Add parser support** following `outrun-parser/CLAUDE.md`
4. **Add type checking support** following `outrun-typechecker/CLAUDE.md`

### Component Development

- **Parser**: See `outrun-parser/CLAUDE.md` for Pest grammar and AST development
- **Type Checker**: See `outrun-typechecker/CLAUDE.md` for type system implementation

## File Extensions

- Source files: `.outrun`
- Package manifest: `outrun.toml`

## Standard Library

Core traits that will be used every day by developers are in the root namespace:
- `Option<T>`
- `Result<T, E>`
- `Iterator<T>`
- etc.

Traits which are for syntax support, or not commonly needed devs live in the `Outrun` namespace:
- `Outrun.BinaryAddition<T>`
- etc.

Users must explicitly alias them:
```outrun
alias Outrun.BinaryAddition<T> as BinAdd<T>
```

## Contributing

When working on the language:

1. **Syntax changes** require updates to both LANGUAGE_SPEC.md and GRAMMAR.bnf
2. **Test thoroughly** following component-specific testing guidelines
3. **Keep "everything is traits"** philosophy consistent
4. **Maintain immutability** and functional approach
5. **Document design decisions** in commit messages

### Rust Test Organization Rules

**IMPORTANT**: All Rust tests MUST follow these conventions:

1. **Test files MUST be in separate test directories**, not inline with implementation code
2. **Test files MUST start with `test_` prefix**
3. **Tests must not contain any debug prints** unless actively debugging a test failure
4. **Less comments** there's no need to add spurious comments that describe easily understandable assertions or setup
5. **Assertions over panics** use assertions to pass/fail tests rather than panics.

```
✅ Correct (Current Parser Pattern):
outrun-parser/tests/test_string_literals.rs
outrun-parser/tests/test_arithmetic_operators.rs

✅ Alternative (src/tests/ subdirectory):
outrun-typechecker/src/tests/test_trait_definitions.rs
outrun-typechecker/src/tests/test_expression_checking.rs

❌ Incorrect:
outrun-parser/tests/string_literals.rs        # Missing test_ prefix
outrun-parser/src/parser/literals.rs          # Inline #[cfg(test)] modules
outrun-typechecker/src/types/mod.rs           # Inline tests
outrun-typechecker/tests/trait_definitions.rs # Missing test_ prefix
```

**Recommended Structure**:
```
outrun-component/
├── src/
│   ├── lib.rs
│   ├── module/
│   │   ├── implementation.rs    # Implementation only
│   │   └── other_logic.rs       # Implementation only
│   └── tests/                   # Tests in src/tests/ subdirectory
│       ├── test_integration.rs
│       ├── test_basic_features.rs
│       └── test_edge_cases.rs
```

**Rationale**: 
- Keeps source code clean and focused on implementation
- `test_` prefix makes test files immediately identifiable
- Enables comprehensive integration testing  
- Improves compilation times for production builds
- Separates unit logic from test logic clearly
- Tests can access internal modules more easily when in src/tests/

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
