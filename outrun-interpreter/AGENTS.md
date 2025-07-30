# AGENTS.md - Interpreter Development Guide

## Build/Test Commands
- `cargo test --package outrun-interpreter` - Run 16+ interpreter foundation tests
- `cargo clippy --package outrun-interpreter --all-targets --all-features -- -D warnings` - Run lints
- `cargo fmt --package outrun-interpreter` - Format interpreter code

## Core Architecture Rules
- **CompilationResult integration**: Use `InterpreterContext::load_compilation_result()` for runtime setup
- **Parser AST compatibility**: Work directly with `outrun_parser::Expression` nodes
- **Value system**: Use functional programming patterns with `Rc<T>` for memory efficiency
- **Context management**: Lexical scoping with push/pop operations for variable environments
- **Registry access**: Direct access to protocol/function registries from CompilationResult

## Value System Requirements
- **Functional lists**: Use linked lists with O(1) head/tail access via `Rc<List>`
- **Type integration**: Include `ParsedTypeInfo` fields for type-aware operations
- **Reference counting**: Efficient memory sharing with `Rc<T>` for immutable values
- **REPL-friendly display**: Implement clear value display for interactive usage
- **All Outrun types**: Support Integer64, Float64, Boolean, String, Atom, List, Map, Struct, Function

## Expression Evaluation Patterns
- **Parser AST direct**: Work with `Expression` and `ExpressionKind` from parser
- **Context-based evaluation**: All evaluation requires `InterpreterContext` for variable/function lookup
- **Error propagation**: Preserve source spans throughout evaluation pipeline
- **Extensible design**: Framework for adding new expression types systematically
- **Type-aware evaluation**: Use attached `ParsedTypeInfo` for runtime type checking

## Intrinsics System Rules
- **Complete function registry**: Map all intrinsic functions to implementation functions
- **Proper argument evaluation**: Full type checking and arity validation
- **Comprehensive error handling**: Division by zero, type mismatches, invalid operations
- **All core operations**: Integer/float arithmetic, list operations, comparisons, boolean logic, string operations
- **Registry integration**: Access intrinsics through CompilationResult dispatch tables

## Context Management Critical Rules
- **Variable scoping**: Proper lexical scoping with nested environments
- **Call stack management**: Track function calls for error reporting and recursion detection
- **CompilationResult loading**: Load protocol/function registries from compilation results
- **Scope isolation**: Push/pop scopes for functions, let bindings, and control flow
- **Error context**: Maintain source spans and call stack for debugging

## Package Composition Support
- **Dependency composition**: Work with pre-compiled dependencies from CompilationResult
- **Registry merging**: Access composed protocol/function registries seamlessly
- **REPL optimization**: Support pre-compiled core library for fast expression evaluation
- **Hot reloading**: Handle package recompilation with context updates
- **Module conflict handling**: Respect module boundaries from compilation results

## Test Organization Rules
- **Test files MUST start with `test_` prefix**: `test_value_operations.rs`
- **Separate test directories**: Use `src/tests/`, never inline `#[cfg(test)]` modules
- **16 tests passing**: Foundation complete with comprehensive coverage
- **Integration testing**: Test complete parser → typechecker → interpreter pipeline
- **Component isolation**: Test each component (Value, Context, Evaluator, Intrinsics) separately

## Integration Points
- **Parser integration**: Direct consumption of `outrun_parser::Expression` AST
- **Typechecker integration**: Load CompilationResult with all type information
- **CLI/REPL integration**: Foundation ready for interactive usage
- **Error reporting**: Source span preservation for miette integration
- **Performance optimization**: Support for pre-compiled core library caching

## Runtime Execution Rules
- **No methods**: All operations through static protocol functions via dispatch
- **Named parameters**: All function calls use named arguments (with shorthand)
- **Immutable values**: No mutation, only rebinding in new scopes
- **Functional programming**: Lists, maps use persistent data structures
- **Protocol dispatch**: All operations go through CompilationResult dispatch tables

## Absolute Prohibitions
- **NEVER assume methods exist** - use static protocol functions only
- **NEVER use positional arguments** - all function calls require named parameters
- **NEVER mutate values** - create new values, use rebinding for changes
- **NEVER bypass CompilationResult** - all type/function info comes from compilation results
- **NEVER ignore source spans** - preserve location information for error reporting

## Quality Standards
- **16 tests passing**: Foundation complete with 100% pass rate
- **CompilationResult integration**: Full support for reusable compilation results
- **Memory efficiency**: Functional data structures with reference counting
- **Error preservation**: Source spans maintained throughout evaluation pipeline
- **Performance ready**: Foundation supports REPL optimization and hot reloading