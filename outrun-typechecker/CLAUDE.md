# Working with the Outrun Type Checker

## Project Overview

The Outrun type checker is a comprehensive static type analysis system for the Outrun programming language. It validates trait constraints, function signatures, and expressions at compile time, generating efficient dispatch tables for the interpreter.

## Project Structure

```
outrun-typechecker/
├── src/
│   ├── lib.rs                    # Main library interface and public API
│   ├── error.rs                  # Type error definitions with miette integration
│   ├── checker/
│   │   ├── mod.rs               # Main TypeChecker struct and typed AST
│   │   ├── context.rs           # Type context and scope management
│   │   ├── expressions.rs       # Expression type checking (core logic)
│   │   ├── functions.rs         # Function definition and call validation
│   │   └── patterns.rs          # Pattern type checking for destructuring
│   ├── types/
│   │   ├── mod.rs              # Type system module exports
│   │   ├── interning.rs        # TypeId/AtomId interning for performance
│   │   ├── concrete.rs         # Concrete type definitions and compatibility
│   │   └── traits.rs           # Trait system and implementation tracking
│   └── dispatch/
│       ├── mod.rs              # Dispatch system exports
│       ├── lookup.rs           # Trait method dispatch table construction
│       └── resolution.rs      # Runtime trait method resolution
├── src/
│   └── tests/                  # Integration tests for type checking
└── Cargo.toml                 # Dependencies and configuration
```

## Key Design Principles

### Type Interning for Performance
All types, atoms, and traits use interned IDs for fast equality comparisons:
```rust
let string_type = context.interner.intern_type("Outrun.Core.String");
let name_atom = context.interner.intern_atom("name");
let display_trait = context.interner.intern_trait("Display");
```

### Scope-Based Context Management
The type checker maintains a scope stack for proper variable shadowing:
```rust
context.push_scope(false);  // Create new scope
context.register_variable(var)?;  // Register in current scope
// ... type check expressions in scope
context.pop_scope();  // Clean up scope
```

### Comprehensive Error Reporting
All errors use miette for beautiful source highlighting:
```rust
return Err(TypeError::type_mismatch(
    "Outrun.Core.Boolean".to_string(),
    "Outrun.Core.Integer64".to_string(),
    crate::error::span_to_source_span(expr.span),
));
```

## Core Type Checking Flow

### Expression Type Checking
The main entry point is `ExpressionChecker::check_expression()` in `expressions.rs`:

1. **Literals**: Direct type mapping (`42` → `Outrun.Core.Integer64`)
2. **Binary Operations**: Trait-based dispatch validation
3. **Function Calls**: Parameter validation with named argument checking
4. **Collections**: Homogeneous lists, heterogeneous tuples, typed maps
5. **Control Flow**: If/case expressions with branch type compatibility
6. **Pattern Matching**: Destructuring validation with variable binding

### Case Expression Variants
Two distinct case expression types:

**Concrete Case**: Pattern-based matching with exhaustiveness checking
```rust
case value {
    when guard1 -> result1
    when guard2 -> result2
}
```

**Trait Case**: Trait implementation dispatch with orphan rule checking
```rust
case value as Display {
    String {} -> "string"
    Integer {} -> "number"
}
```

## Trait System Architecture

### Trait Definition Validation
- Function signature validation (parameters, return types)
- Guard function validation (must return Boolean)
- Generic parameter constraint checking
- Type name resolution (`Boolean` → `Outrun.Core.Boolean`)

### Trait Implementation Tracking
```rust
// Register trait implementations
let impl_id = context.trait_registry.register_implementation(trait_impl);

// Check trait implementation
if context.trait_registry.implements_trait(type_id, trait_id) {
    // Type implements trait
}

// Exhaustiveness checking
let result = context.trait_registry.check_trait_case_exhaustiveness(
    trait_id, 
    covered_types
);
```

### Orphan Rule Analysis
The trait system enforces orphan rules for trait implementations:
- Can only implement traits you own OR for types you own
- Prevents conflicting implementations
- Enables exhaustiveness checking for trait case expressions

## Pattern Type Checking

### Unified Pattern System
All patterns work consistently across:
- Let bindings: `let User { name, age } = user`
- Case expressions: `case user { when User { age } when age > 18 -> ... }`
- Function parameters: `def process(User { name, address: Address { city } })`

### Variable Binding Collection
Pattern checker returns bound variables for scope registration:
```rust
let bound_variables = PatternChecker::check_pattern(
    context, 
    pattern, 
    target_type
)?;

for variable in bound_variables {
    context.register_variable(variable)?;
}
```

## Testing Strategy

### Comprehensive Test Coverage
- **Unit tests**: Individual component validation
- **Integration tests**: Real type checking scenarios  
- **Error case tests**: Comprehensive error condition coverage
- **Pattern tests**: All pattern types and nesting combinations

### Test Organization
Tests are organized by feature area in `src/tests/`:
- `test_basic_type_checking.rs`: Core expression type checking integration tests
- `test_trait_case_exhaustiveness.rs`: Trait dispatch validation integration tests
- `test_trait_case_patterns.rs`: Pattern integration with trait cases integration tests
- `test_type_checker_core.rs`: TypeChecker core functionality unit tests
- `test_type_interning.rs`: Type interning system unit tests  
- `test_traits_unit.rs`: Trait system component unit tests

## Development Workflow

### Adding New Type Checking Features

1. **Define error types** in `error.rs` with miette integration
2. **Add type checking logic** in appropriate checker module
3. **Update typed AST** structures in `checker/mod.rs` if needed
4. **Write comprehensive tests** covering success and error cases
5. **Test with real Outrun programs** for integration validation

### Type System Extensions

1. **Add concrete types** in `types/concrete.rs`
2. **Update type compatibility** checking if needed
3. **Add trait definitions** in `types/traits.rs` for new behaviour
4. **Update dispatch tables** if new trait methods are added

### Error Message Guidelines

- Use fully qualified type names: `Outrun.Core.String` not `String`
- Include helpful context: "Expected struct type for pattern destructuring"
- Provide specific spans: Point to exact source location causing error
- Suggest fixes when possible: "Consider using Option.some(value: ...)"

## Performance Considerations

### Type Interning
- All type comparisons are O(1) via TypeId equality
- String interning prevents repeated allocations
- Thread-safe design for future parallel type checking

### Scope Management
- Efficient scope stack with variable shadowing support
- O(1) variable lookup within scope
- Automatic cleanup on scope exit

### Trait Implementation Lookup
- Hash-based trait implementation storage
- Fast trait membership checking
- Efficient exhaustiveness analysis

## Integration Points

### Parser Integration
The type checker consumes AST from `outrun-parser`:
- All source spans preserved for error reporting
- Pattern structures shared between parser and type checker
- Expression kinds mapped to typed expression variants

### Future Interpreter Integration
Type checker produces:
- Typed AST with complete type information
- Trait dispatch tables for runtime method calls
- Static function lookup tables
- Pre-validated function signatures

## Common Patterns

### Type Context Usage
```rust
// Always check if type exists before using
let type_id = context.interner.get_type(type_name).ok_or_else(|| {
    TypeError::UndefinedType { 
        span: span_to_source_span(span),
        name: type_name.clone() 
    }
})?;

// Use concrete type for validation
let concrete_type = context.get_concrete_type(type_id)
    .ok_or_else(|| TypeError::internal("Type not in registry".to_string()))?;
```

### Error Creation
```rust
// Type mismatch with helpful messages
TypeError::type_mismatch(
    expected_type_name,
    found_type_name, 
    span_to_source_span(span)
)

// Internal errors for debugging
TypeError::internal(format!("Unexpected state: {}", details))

// Trait-specific errors
TypeError::TraitNotImplemented {
    span: span_to_source_span(span),
    trait_name: trait_name.clone(),
    type_name: type_name.clone(),
}
```

### Scope Management
```rust
// Function scopes need special handling
context.push_scope(true);  // Function scope
// ... register parameters and check body
context.pop_scope();

// Expression scopes for let bindings
context.push_scope(false);  // Expression scope  
// ... register pattern variables
// ... check expression in scope
context.pop_scope();
```

## Current Status (June 2025)

✅ **Completed Features**:
- Complete expression type checking with all Outrun expressions
- Comprehensive pattern type checking with recursive validation
- Trait definition processing with constraint validation
- Enhanced case statements with trait dispatch
- Pattern validation integration for case expressions
- Exhaustiveness checking for trait case expressions
- Beautiful error reporting with miette integration
- 94+ comprehensive tests with 100% pass rate

🔄 **Next Priorities**:
- Trait implementation validation (impl blocks)
- Dispatch table construction for runtime
- Function signature validation
- Function call resolution with overloading

## Useful Commands

```bash
# Run type checker tests
cargo test --package outrun-typechecker

# Run specific test category
cargo test test_trait_case_exhaustiveness

# Run with output for debugging
cargo test -- --nocapture

# Check type checker specifically
cargo check --package outrun-typechecker

# Run clippy on type checker
cargo clippy --package outrun-typechecker --all-targets --all-features -- -D warnings

# Format type checker code
cargo fmt --package outrun-typechecker
```

## Future Extensions

### Type Inference
- Hindley-Milner style type inference
- Reduce explicit type annotations
- Maintain trait dispatch efficiency

### Advanced Error Recovery
- Multiple error reporting in single pass
- Error recovery strategies for parser integration
- IDE integration with real-time feedback

### Performance Optimization
- Incremental type checking for large codebases
- Parallel type checking for independent modules
- Advanced dispatch table optimization

This type checker provides the foundation for Outrun's static type system while maintaining the flexibility needed for trait-based programming and comprehensive error reporting.