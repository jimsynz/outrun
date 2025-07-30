# AGENTS.md - Typechecker Development Guide

## Build/Test Commands
- `cargo test --package outrun-typechecker` - Run 247+ comprehensive typechecker tests
- `cargo clippy --package outrun-typechecker --all-targets --all-features -- -D warnings` - Run lints
- `cargo fmt --package outrun-typechecker` - Format typechecker code

## Core Architecture Rules
- **CompilationResult system**: Use `CompilationResult::compile_package()` for reusable compilation
- **Package-level processing**: Process multiple `.outrun` files together for complete type understanding
- **6-phase architecture**: Collect definitions → Build registry → Type inference → Dependency composition → Module conflict detection → Result packaging
- **Registry composition**: Merge protocol/function registries from dependencies with orphan rule preservation

## Type Inference Critical Rules
- **Hindley-Milner foundation**: Use constraint generation and unification for type inference
- **Self type resolution**: Handle `Self` in multiple positions (arguments, return types, generic containers)
- **Type variable unification**: All occurrences of same type variable must unify to same concrete type
- **Protocol constraint solving**: Use constraint-based compatibility for protocol requirements
- **Monomorphisation**: Generate concrete implementations for generic functions

## Protocol System Requirements
- **Everything is protocols**: All operators, behaviour defined through protocol dispatch
- **Static functions only**: No methods - use `List.head(value: list)` not `list.head()`
- **Named parameters only**: All function calls use named arguments (with shorthand support)
- **Protocol requirements**: Track transitive requirements like "Integer requires BinaryAddition"
- **Impl block validation**: Constrained type variables must appear in type specifications

## CompilationResult Integration
- **Reusable compilation**: `compile_with_dependencies()` for pre-compiled dependencies
- **REPL optimization**: `precompile_core_library()` for 3x performance improvement
- **Hot reloading**: `recompile_package()` with content-aware change detection
- **Module conflict prevention**: Prevent cross-package module redefinition
- **Registry merging**: Sophisticated composition with conflict detection

## Test Organization Rules
- **Test files MUST start with `test_` prefix**: `test_integration_comprehensive.rs`
- **Separate test directories**: Use `src/tests/`, never inline `#[cfg(test)]` modules
- **247 tests passing**: Comprehensive coverage including dependency composition
- **Property-based testing**: Use proptest for type system invariant verification

## Type System Semantics
- **Concrete vs Protocol types**: Distinguish between `Integer64` (concrete) and `Integer` (protocol)
- **Generic type instantiation**: Use `Type::Concrete { args: Vec<Type> }` for generics
- **Constraint validation**: Validate impl block constraints against available type variables
- **Type compatibility**: Handle protocol-concrete relationships through constraint solving
- **Substitution consistency**: Apply same substitution to all occurrences of type variable

## Error Reporting Standards
- **Enhanced error types**: Rich error structures with context and suggestions
- **Smart suggestions**: Levenshtein distance-based similarity detection for typos
- **Type conversion hints**: Automatic suggestions for common conversion patterns
- **Multi-span errors**: Context-aware error reporting with source spans
- **Protocol guidance**: Suggest missing protocol implementations

## Integration Points
- **Parser AST extension**: Add `type_info: Option<TypeInfo>` to Expression nodes
- **Miette error integration**: Reuse existing error infrastructure with source highlighting
- **CLI tool integration**: Support `outrun typecheck` command with beautiful output
- **Interpreter integration**: Provide CompilationResult for runtime dispatch

## Absolute Prohibitions
- **NEVER hardcode protocol-to-intrinsic mappings** - breaks extensibility and architecture
- **NEVER create parallel type systems** - extend existing parser AST instead
- **NEVER assume positional arguments** - all Outrun functions use named parameters
- **NEVER bypass protocol dispatch** - all operations must go through protocol system
- **NEVER ignore orphan rules** - maintain locality constraints in registry composition

## Quality Standards
- **247 tests passing**: All tests must pass including dependency composition
- **Zero clippy warnings**: Strict Rust best practices
- **Production-ready**: Handles 100+ core library files with complete type checking
- **Performance optimized**: REPL expressions compile in <1s with pre-compiled core library