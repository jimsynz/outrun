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
├── outrun-parser/              # Parser implementation (Rust) ✅ PRODUCTION READY
│   ├── CLAUDE.md               # Parser-specific development guide
│   ├── Cargo.toml
│   ├── src/
│   └── tests/                  # Parser integration tests (400+ tests)
├── outrun-typechecker/         # Type checker v3 with reusable compilation ✅ PRODUCTION READY
│   ├── CLAUDE.md               # Type checker development guide
│   ├── Cargo.toml
│   ├── src/
│   └── tests/                  # Type checker integration tests (247+ tests)
└── outrun-interpreter/         # Interpreter with CompilationResult integration ✅ FOUNDATION COMPLETE
    ├── CLAUDE.md               # Interpreter development guide
    ├── Cargo.toml
    ├── src/
    └── tests/                  # Interpreter integration tests
```

## 🚀 **NEW: Major Capabilities Added Today**

### ✅ **Reusable Compilation System**
- **CompilationResult architecture** - Packages compile once, reuse everywhere
- **Dependency composition** - Pre-compiled packages compose without recompilation
- **3x REPL performance** - Core library cached for fast expression evaluation (2.58s → 871ms)

### ✅ **Module Conflict Prevention**  
- **Cross-package protection** - Prevents different packages from module redefinition
- **Comprehensive conflict detection** - Struct vs struct, protocol vs protocol, struct vs protocol
- **Package self-redefinition** - Supports hot reloading with intelligent change detection

### ✅ **Registry Composition**
- **Protocol registry merging** - Sophisticated composition with orphan rule preservation
- **Function registry composition** - Unified function dispatch across package boundaries
- **Dispatch table building** - Complete runtime dispatch from composed registries

### ✅ **Hot Reloading Support**
- **Content-aware change detection** - Warnings only when module content actually changes
- **Package recompilation API** - `recompile_package()` for plugin systems and hot reloading
- **Hash-based change tracking** - Efficient detection of actual content modifications

## Core Design Principles

- **Everything is protocols** - All types implement protocols, which define behaviour
- **Named arguments only** - No positional arguments in function calls
- **Static typing** with protocol constraints and guards
- **Immutable and functional** - No mutation, rebinding allowed
- **Actor model runtime** - Built for concurrent, distributed systems

## ⚡ CRITICAL: Minimalist Development Philosophy

**ABSOLUTE PRIORITY: Write as little code as humanly possible to achieve any feature.**

### Code Minimization Rules

**🚫 NEVER create new code when you can:**
1. **Refactor existing code** to handle the new case
2. **Extend existing abstractions** rather than create parallel ones  
3. **Reuse existing data structures** with minor modifications
4. **Generalize existing functions** to handle broader cases
5. **Compose existing primitives** in new ways

**✅ ONLY create new code when:**
- **Absolutely no existing code can be refactored** to handle the requirement
- **The new code enables significant reuse** in multiple future features
- **Creating shared abstractions** that eliminate duplicate patterns

### Refactoring-First Development Process

**Before writing ANY new code:**

1. **Survey existing codebase** - Search thoroughly for similar patterns, data structures, or logic
2. **Identify refactoring opportunities** - Can existing code be generalized to handle your case?
3. **Consider composition** - Can you build the feature by combining existing pieces?
4. **Evaluate abstractions** - Would a small change to existing abstractions unlock your feature?
5. **Only then create new code** - And design it for maximum reusability

### Examples of Minimalist Thinking

**❌ BAD: Creating parallel systems**
```rust
// Creating separate error handling for typechecker
enum TypecheckerError { ... }
impl TypecheckerError { ... }

// When parser already has comprehensive error handling
enum ParserError { ... }
impl ParserError { ... }
```

**✅ GOOD: Extending existing systems**
```rust
// Extend existing error system to handle typechecker cases
enum CompilerError {
    Parser(ParserError),
    Typechecker(TypecheckerError),  // Add new variant
    // Reuse existing error infrastructure
}
```

**❌ BAD: Duplicating data structures**
```rust
// Creating separate AST for typechecker
struct TypecheckerAst { ... }

// When parser AST could be extended
struct ParserAst { ... }
```

**✅ GOOD: Extending existing structures**
```rust
// Add type information to existing AST
struct Ast {
    // Existing parser fields...
    type_info: Option<TypeInfo>,  // Add what you need
}
```

### Reuse-Oriented Design

**Design every piece of code for maximum reuse:**

- **Generic over specific** - Make functions work with traits/protocols, not concrete types
- **Composable primitives** - Small, focused functions that combine well
- **Data-driven behaviour** - Use configuration/parameters instead of separate code paths
- **Orthogonal concerns** - Separate functionality that can be mixed and matched

### Code Review Questions

Before any code change, ask:

1. **"Can I refactor existing code instead?"**
2. **"What existing patterns am I duplicating?"**
3. **"How can this be reused by future features?"**
4. **"What's the smallest possible change?"**
5. **"Am I creating abstractions that eliminate duplication?"**

**Remember: The best code is code that doesn't exist. The second-best code is code that serves multiple purposes.**

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

### ✅ Task Completion Protocol

**CRITICAL: Never start new tasks until the previous task is explicitly confirmed as complete by the user.**

When completing development tasks:

1. **Implement the task fully** - Complete all implementation, tests, and documentation
2. **Mark todo items as completed** - Update TodoWrite with completed status
3. **Wait for user confirmation** - DO NOT proceed to next task automatically
4. **Request explicit approval** - Ask user to confirm task completion before moving on

This ensures:
- **Quality control** - User can review and validate work before proceeding
- **Priority alignment** - User can redirect to different tasks if priorities change
- **Context preservation** - User maintains control over development flow
- **Documentation accuracy** - Task completion status stays synchronized

**Example workflow:**
```
Assistant: Task #1234 implementation is complete. All tests passing.
User: "You can mark that task as done and proceed with task #1235"
Assistant: [Updates task status and begins next task]
```

**Never assume task completion without explicit user confirmation.**

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

1. **Package Systems** - Import pre-compiled packages with dependency composition
2. **Incremental Compilation** - Reuse previously compiled modules without recompilation  
3. **REPL Optimization** - Pre-compile core library once for fast expression evaluation
4. **Hot Reloading** - Package self-redefinition with intelligent change detection

### Core Components

#### CompilationResult::compile_with_dependencies()

Compiles a package with pre-compiled dependencies:

```rust
// Pre-compile core library once
let core_lib = CompilationResult::precompile_core_library()?;

// Compile user package with core library as dependency
let mut user_package = Package::new("my_package".to_string());
user_package.add_program(parsed_program);

let compilation_result = CompilationResult::compile_with_dependencies(
    &mut user_package,
    vec![core_lib]  // Dependencies
)?;
```

#### REPL Optimization

Fast expression compilation using pre-compiled core library:

```rust
// Pre-compile core library once for REPL session
let core_compilation = CompilationResult::precompile_core_library()?;

// Fast expression compilation (3x performance improvement: 2.58s → 871ms)
let expression_result = CompilationResult::compile_repl_expression(
    "1 + 2 * 3",
    &core_compilation
)?;
```

#### Hot Reloading Support

Package self-redefinition with content-aware change detection:

```rust
// Initial compilation
let initial_result = CompilationResult::compile_package(&mut package)?;

// Update package content
update_package_content(&mut package);

// Recompile with change detection (warns only if content actually changed)
let updated_result = CompilationResult::recompile_package(
    &mut package,
    Some(&initial_result),  // Previous compilation for change detection
    vec![]  // Dependencies
)?;
// Output: "⚠️  package myapp redefined module User" (only if content changed)
```

### Module Conflict Prevention

The system prevents cross-package conflicts through comprehensive detection:

1. **Cross-Package Protection** - Different packages cannot redefine each other's modules
2. **Struct vs Struct Conflicts** - Prevents multiple packages defining the same struct name
3. **Protocol vs Protocol Conflicts** - Prevents multiple packages defining the same protocol name  
4. **Struct vs Protocol Conflicts** - Prevents packages mixing struct/protocol with same name
5. **Package Self-Redefinition** - Packages CAN redefine their own modules (hot reloading support)
6. **Content-Aware Warnings** - Warnings only when module content actually changes

### Registry Composition

Sophisticated registry merging with orphan rule preservation:

1. **Protocol Registry Merging** - Combines protocol definitions with conflict detection
2. **Function Registry Merging** - Merges function registries from dependencies
3. **Orphan Rule Preservation** - Maintains locality information across package boundaries
4. **Dispatch Table Composition** - Builds unified dispatch tables from all packages

### Performance Benefits

1. **No Recompilation** - Core library compiled once, reused everywhere (3x REPL improvement)
2. **Registry Reuse** - Pre-compiled registries eliminate redundant processing
3. **Incremental Builds** - Only recompile changed packages
4. **Smart Change Detection** - Content hashing prevents unnecessary warnings

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
