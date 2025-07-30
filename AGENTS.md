# AGENTS.md - Development Guide for Outrun

## Build/Test Commands
- `cargo fmt` - Format all Rust code across workspace
- `cargo test` - Run all tests across workspace  
- `cargo test test_name` - Run specific test by name
- `cargo clippy --all-targets --all-features -- -D warnings` - Run lints

## Code Style Guidelines
- **Test Organization**: Tests MUST be in separate `test_*.rs` files or `src/tests/` subdirectories, never inline
- **Imports**: Use explicit imports (`use crate::ast::*`), group by source (std, external, internal)
- **Error Handling**: Use `Result<T, E>` types, propagate with `?` operator
- **Naming**: snake_case for functions/variables, PascalCase for types, SCREAMING_SNAKE_CASE for constants
- **Documentation**: Use `///` for public APIs, `//!` for module-level docs
- **Formatting**: Always run `cargo fmt` before committing

## Language-Specific Rules
- **No hardcoded function mappings** - All dispatch goes through CompilationResult mechanism
- **Minimalist development** - Refactor existing code before writing new code
- **Protocol-based everything** - All operators/behaviour defined through protocols
- **Named parameters only** - No positional arguments in function calls
- **Static typing** with protocol constraints and guards

## Critical Outrun Language Differences
- **NO METHODS** - Use static protocol functions: `List.head(value: list)` not `list.head()`
- **NO ENUM CONSTRUCTORS** - Use protocol functions: `Option.some(value: 42)` not `Some(42)`
- **NO UNIT TYPES** - All functions return meaningful values, never void
- **NAMED PARAMETERS ONLY** - `Map.get(map, key)` shorthand for `Map.get(map: map, key: key)`
- **STRUCTS VIA FUNCTIONS** - Use `Option.some(value: 42)` not direct construction

## Absolute Prohibitions
- **NEVER hardcode protocol-to-intrinsic mappings** - breaks extensibility and architecture
- **NEVER create parallel systems** - extend existing abstractions instead
- **NEVER write new code** when existing code can be refactored to handle the case
- **NEVER assume task completion** - wait for explicit user confirmation before proceeding

## Development Philosophy
- **Write as little code as possible** - refactor existing code before creating new code
- **Extend existing systems** rather than create duplicate ones
- **Reuse existing data structures** with minor modifications
- **Design for maximum reusability** across features

## Language Specification Reference
- **@LANGUAGE_SPEC.md** contains comprehensive information about how Outrun should work
- **Core principles**: Everything is protocols, named parameters only, static typing, immutable/functional
- **Type system**: Concrete types vs protocols, Self type semantics, generic constraints
- **Module system**: Types ARE modules, file structure is conventional
- **Function semantics**: All functions must return values, explicit return types required
- **Pattern matching**: Unified destructuring patterns across all contexts