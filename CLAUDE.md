# Working with the Outrun Project

## Project Overview

Outrun is a statically-typed, functional programming language built around the concept of protocols. The language emphasises immutability, named parameters, and a powerful guard system for control flow.

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

- **Everything is protocols** - All types implement protocols, which define behaviour
- **Named arguments only** - No positional arguments in function calls
- **Static typing** with protocol constraints and guards
- **Immutable and functional** - No mutation, rebinding allowed
- **Actor model runtime** - Built for concurrent, distributed systems

## Key Language Features

### protocol-Based Everything

All operators, control flow, and behaviour is defined through protocols:

- `+` calls `BinaryAddition.add()`
- `|>` calls `Pipe.pipe_into()`
- `|?` calls `Maybe.maybe_pipe()`
- `@Attribute()` calls `Attribute.apply()`
- `~Sigil""` calls `Sigil.parse()`

### Module System

- Types (structs/protocols) ARE modules
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

### Critical Language Differences

**⚠️ Outrun is NOT Like Other Languages** - These differences are crucial for all development:

#### 1. **NO METHODS OR INSTANCE FUNCTIONS**

```outrun
// ❌ WRONG - This is NOT how Outrun works
let list = [1, 2, 3];
let head = list.head();  // Methods don't exist in Outrun

// ✅ CORRECT - Outrun uses static protocol functions
let list = [1, 2, 3];
let head = List.head(value: list);  // Static function call
```

#### 2. **NO ENUM VARIANT CONSTRUCTORS**

```outrun
// ❌ WRONG - Outrun has no enum constructors
let opt = Some(42);        // This syntax doesn't exist
let result = Ok("success"); // This syntax doesn't exist

// ✅ CORRECT - Use static protocol functions
let opt = Option.some(value: 42);        // Static function creates struct
let result = Result.ok(value: "success"); // Static function creates struct
```

#### 3. **NO EMPTY TUPLE OR UNIT TYPES**

```outrun
// ❌ WRONG - No unit type in Outrun
def do_something() {  // No return type = void (doesn't exist)
    // ...
}

// ✅ CORRECT - All functions return meaningful values
def do_something(): Boolean {  // Must return actual value
    // ...
    true  // Always return a value
}
```

#### 4. **ONLY NAMED PARAMETERS (with shorthand)**

```outrun
// ❌ WRONG - No positional arguments
List.prepend(list, element);  // Positional args don't exist

// ✅ CORRECT - All parameters must be named
List.prepend(list: my_list, elem: element);  // Named parameters required

// ✅ SHORTHAND - When variable name matches parameter name
let list = [1, 2, 3];
let elem = 0;
List.prepend(list, elem);  // Shorthand: list means list: list, elem means elem: elem
```

#### 5. **STRUCTS CREATED BY FUNCTIONS, NOT CONSTRUCTORS**

```outrun
// ❌ WRONG - Direct struct construction doesn't exist for users
let opt = Some { value: 42 };  // Direct construction not available

// ✅ CORRECT - Use static protocol functions (which create structs internally)
let opt = Option.some(value: 42);  // Creates Outrun.Option.Some<T> struct internally
```

### Common Programming Mistakes to Avoid

#### ❌ Thinking Object-Oriented

```rust
// WRONG - Treating values as objects with methods
let list = [1, 2, 3];
let head = list.head();  // Methods don't exist!

// CORRECT - Everything is function calls
let head = List.head(value: list);
```

#### ❌ Using Direct Enum/Variant Construction

```rust
// WRONG - Direct variant construction
let opt = Some(value);
let result = Ok(data);

// CORRECT - Via protocol functions
let opt = Option.some(value: value);
let result = Result.ok(value: data);
```

#### ❌ Expecting Unit/Void Returns

```rust
// WRONG - Functions returning nothing
def print_line(text: String) {  // Void return
    // ...
}

// CORRECT - Everything returns a value
def print_line(text: String): Boolean {  // Success indicator
    // ...
    true
}
```

#### ❌ Using Positional Arguments

```rust
// WRONG - Positional function calls
List.append(list1, list2);
Map.get(map, key);

// CORRECT - Named arguments (with optional shorthand)
List.append(first: list1, second: list2);
Map.get(map: map, key: key);
// Or shorthand when variable names match:
Map.get(map, key);  // Equivalent to Map.get(map: map, key: key)
```

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

Core protocols that will be used every day by developers are in the root namespace:

- `Option<T>`
- `Result<T, E>`
- `Iterator<T>`
- etc.

Protocols which are for syntax support, or not commonly needed devs live in the `Outrun` namespace:

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
3. **Keep "everything is protocols"** philosophy consistent
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
outrun-typechecker/src/tests/test_protocol_definitions.rs
outrun-typechecker/src/tests/test_expression_checking.rs

❌ Incorrect:
outrun-parser/tests/string_literals.rs        # Missing test_ prefix
outrun-parser/src/parser/literals.rs          # Inline #[cfg(test)] modules
outrun-typechecker/src/types/mod.rs           # Inline tests
outrun-typechecker/tests/protocol_definitions.rs # Missing test_ prefix
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

## Package Composition System

The Outrun compiler supports composing multiple compilation results together, enabling:

1. **Package Systems** - Import pre-compiled packages
2. **Incremental Compilation** - Reuse previously compiled modules
3. **Build Systems** - Compose multiple compilation units

### Core Components

#### CompilationResult::merge()

Combines multiple compilation results into a single coherent unit:

```rust
let core_lib = compile_core_library();
let user_package = compile_user_package();
let third_party = compile_third_party_package();

let combined = CompilationResult::merge(
    core_lib,
    vec![user_package, third_party]
)?;
```

#### PackageSummary

Lightweight interface extraction for fast imports:

```rust
// Create package summary from compilation
let summary = compilation.create_package_summary("my_package");

// Package summary contains only public interface:
// - Exported protocols and their signatures
// - Exported structs and their fields
// - Exported function signatures
// - Protocol implementations

// Convert back to compilation result for merging
let import_compilation = summary.to_compilation_result();
```

#### MultiProgramCompiler composition methods

```rust
// Compose multiple packages
let composed = MultiProgramCompiler::compose_packages(vec![
    core_library_compilation,
    math_library_compilation,
    user_code_compilation,
])?;

// Import a package into existing compiler
let mut compiler = MultiProgramCompiler::from_compilation_result(core_lib);
compiler.import_package(&math_package_summary)?;
```

### Conflict Resolution

The system handles conflicts through:

1. **Protocol Compatibility** - Same protocol must have identical signatures
2. **Struct Compatibility** - Same struct must have identical fields
3. **Function Overrides** - Later packages can override earlier functions (with warnings)
4. **Orphan Rules** - Prevent conflicting protocol implementations

### Performance Benefits

1. **No Recompilation** - Core library compiled once, reused everywhere
2. **Fast Imports** - Package summaries contain only interface, not implementation
3. **Incremental Builds** - Only recompile changed modules
4. **Parallel Compilation** - Independent packages can compile in parallel

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
