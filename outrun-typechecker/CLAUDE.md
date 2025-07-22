# Working with the Outrun Type Checker

## Project Overview

The Outrun type checker is a comprehensive static type analysis system for the Outrun programming language. It validates protocol constraints, function signatures, and expressions at compile time, generating efficient dispatch tables for the interpreter.

**Current Status**: ✅ **PRODUCTION READY** - Full type checking, protocol system, generics, and package composition support

### Recent Architectural Improvements

**Thread-Safe Function Registry**: Implemented `Arc<RwLock<>>` interior mutability pattern for:

- True sharing of function registries between components
- Preservation of typed function definitions across package composition
- Thread-safe access for concurrent type checking scenarios
- Elimination of registry cloning/merging data loss

**TypeInterner Singleton Architecture**: Enhanced type ID consistency with:

- Global singleton pattern for production builds ensuring consistent type IDs
- Test isolation via conditional compilation preventing cross-test contamination
- Proper memory management eliminating Box::leak patterns
- Consistent type resolution across all typechecker components

#### Recursive Type Unification Algorithm

Instead of string comparison, the system uses recursive unification:

```rust
// Option<Self> vs Option<Outrun.Core.String> when Self = Outrun.Core.String
fn unify_structured_types(type1: &StructuredType, type2: &StructuredType) -> bool {
    match (type1, type2) {
        // Generic type unification: Generic<A1, A2, ...> vs Generic<B1, B2, ...>
        (StructuredType::Generic { base: base1, args: args1 },
         StructuredType::Generic { base: base2, args: args2 }) => {
            // 1. Base types must unify (Option = Option) ✓
            unify_simple_types(base1, base2) &&
            // 2. Argument arity must match (1 = 1) ✓
            args1.len() == args2.len() &&
            // 3. All arguments must unify recursively (Self = Outrun.Core.String) ✓
            args1.iter().zip(args2.iter()).all(|(a1, a2)| unify_structured_types(a1, a2))
        }
        // ... other cases
    }
}
```

#### Function Type Handling with Named Parameters

Outrun requires named parameters, so function types track both names and types:

```rust
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParam {
    pub name: AtomId,           // Parameter name (required for Outrun)
    pub param_type: StructuredType,
}

// Function type: (name: String, age: Integer) -> Boolean
StructuredType::Function {
    params: vec![
        FunctionParam { name: "name".into(), param_type: StructuredType::Simple("String".into()) },
        FunctionParam { name: "age".into(), param_type: StructuredType::Simple("Integer".into()) },
    ],
    return_type: Box::new(StructuredType::Simple("Boolean".into())),
}
```

#### Type Resolution Pipeline

1. **Parser** → `TypeAnnotation` with `GenericArgs` (structured representation preserved)
2. **Type Resolver** → `StructuredType::from_type_annotation()` (recursive resolution with Self support)
3. **Type Checker** → `unify_structured_types()` (recursive unification with protocol compatibility)
4. **Error Reporting** → `StructuredType::to_string_representation()` (readable error messages)

#### Key Benefits

- **Correct Generic Unification**: `Option<Self>` properly unifies with `Option<ConcreteType>`
- **Protocol-Based Compatibility**: `Boolean` protocol unifies with `Outrun.Core.Boolean` implementation
- **Precise Error Messages**: Show exact generic type mismatches with proper structure
- **Performance**: Avoid string parsing/reconstruction during type checking
- **Extensibility**: Easy to add new type constructs (unions, intersections, etc.)

### Implementation Strategy for Structured Types

#### Critical Implementation Rules

1. **Never flatten to strings during type checking** - only for error reporting
2. **Always use recursive unification** for generic type compatibility
3. **Preserve Self binding context** when resolving type annotations in impl blocks
4. **Handle protocol vs concrete type relationships** in unification algorithm
5. **Maintain TypeId compatibility** only where absolutely necessary for legacy code

#### Common Pitfalls to Avoid

- ❌ Converting StructuredType → string → TypeId → StructuredType (loses structure)
- ❌ Using string comparison for generic types (`"Option<T>"` == `"Option<U>"`)
- ❌ Forgetting Self binding in impl block function signatures
- ❌ Not handling protocol implementation in unification (Boolean vs Outrun.Core.Boolean)
- ❌ Mixing TypeId and StructuredType incompatibly in the same code path

## Comprehensive Type Unification Algorithm

### Overview

The type unification algorithm is the core of Outrun's type system. It determines when two types are compatible, handling generic types recursively and protocol-based relationships. This replaces the flawed string-based comparison with proper structural analysis.

### Unification Rules

The algorithm follows these fundamental rules in order:

#### 1. Exact Match (Fast Path)

```rust
if type1 == type2 { return true; }
```

If both types are structurally identical, they unify immediately.

#### 2. Generic Type Unification

For types like `Option<T>` vs `Option<U>`:

```rust
(StructuredType::Generic { base: base1, args: args1 },
 StructuredType::Generic { base: base2, args: args2 }) => {
    // Base types must unify (Option = Option)
    unify_simple_types(base1, base2) &&
    // Argument count must match (arity check)
    args1.len() == args2.len() &&
    // All arguments must unify recursively
    args1.iter().zip(args2.iter()).all(|(a1, a2)| unify_structured_types(a1, a2))
}
```

**Examples:**

- ✅ `Option<String>` ∪ `Option<String>` → unified (exact match)
- ✅ `Option<Self>` ∪ `Option<Outrun.Core.String>` → unified (when Self = Outrun.Core.String)
- ✅ `Map<String, Integer>` ∪ `Map<String, Integer>` → unified (exact match)
- ❌ `Option<String>` ∪ `Option<Integer>` → not unified (different argument types)
- ❌ `Option<T>` ∪ `Map<T, U>` → not unified (different base types)
- ❌ `Option<T>` ∪ `Map<T>` → not unified (different arity: 1 vs 2)

#### 3. Tuple Type Unification

For tuple types like `(String, Integer)`:

```rust
(StructuredType::Tuple(elems1), StructuredType::Tuple(elems2)) => {
    // Element count must match
    elems1.len() == elems2.len() &&
    // All elements must unify positionally
    elems1.iter().zip(elems2.iter()).all(|(e1, e2)| unify_structured_types(e1, e2))
}
```

**Examples:**

- ✅ `(String, Integer)` ∪ `(String, Integer)` → unified
- ✅ `(Self, Boolean)` ∪ `(Outrun.Core.String, Outrun.Core.Boolean)` → unified (when Self = Outrun.Core.String, Boolean protocol implemented by Outrun.Core.Boolean)
- ❌ `(String, Integer)` ∪ `(Integer, String)` → not unified (order matters)
- ❌ `(String)` ∪ `(String, Integer)` → not unified (different arity)

#### 4. Function Type Unification

For function types with named parameters:

```rust
(StructuredType::Function { params: params1, return_type: ret1 },
 StructuredType::Function { params: params2, return_type: ret2 }) => {
    // Parameter count must match
    params1.len() == params2.len() &&
    // All parameters must have matching names AND types (Outrun requirement)
    params1.iter().zip(params2.iter()).all(|(p1, p2)| {
        p1.name == p2.name && unify_structured_types(&p1.param_type, &p2.param_type)
    }) &&
    // Return types must unify
    unify_structured_types(ret1, ret2)
}
```

**Examples:**

- ✅ `(name: String) -> Boolean` ∪ `(name: String) -> Boolean` → unified
- ✅ `(value: Self) -> Option<Self>` ∪ `(value: Outrun.Core.String) -> Option<Outrun.Core.String>` → unified (when Self = Outrun.Core.String)
- ❌ `(name: String) -> Boolean` ∪ `(title: String) -> Boolean` → not unified (different parameter names)
- ❌ `(name: String) -> Boolean` ∪ `(name: String, age: Integer) -> Boolean` → not unified (different arity)

#### 5. Simple Type Unification (Protocols + Concrete)

For non-generic types, the algorithm handles protocol-based compatibility:

```rust
fn unify_simple_types(type1: TypeId, type2: TypeId) -> bool {
    // Fast path: exact match
    if type1 == type2 { return true; }

    // Determine if types are protocols or concrete
    let type1_is_protocol = interner.get_protocol(&type1_name).is_some();
    let type2_is_protocol = interner.get_protocol(&type2_name).is_some();

    match (type1_is_protocol, type2_is_protocol) {
        // Both protocols: must be exactly equal
        (true, true) => false,

        // Type1 is protocol, type2 is concrete: type2 must implement type1
        (true, false) => protocol_registry.implements_protocol(type2, type1_protocol_id),

        // Type1 is concrete, type2 is protocol: type1 must implement type2
        (false, true) => protocol_registry.implements_protocol(type1, type2_protocol_id),

        // Both concrete: must be exactly equal
        (false, false) => false,
    }
}
```

**Examples:**

- ✅ `Boolean` ∪ `Outrun.Core.Boolean` → unified (concrete type implements protocol)
- ✅ `Outrun.Core.Boolean` ∪ `Boolean` → unified (protocol compatibility is symmetric)
- ✅ `Integer` ∪ `Outrun.Core.Integer64` → unified (if Integer64 implements Integer)
- ❌ `String` ∪ `Integer` → not unified (different concrete types)
- ❌ `Display` ∪ `Debug` → not unified (different protocols)

#### 6. Cross-Type Unification

Different type categories never unify:

```rust
// These combinations always return false:
(StructuredType::Simple(_), StructuredType::Generic { .. }) => false,
(StructuredType::Generic { .. }, StructuredType::Simple(_)) => false,
(StructuredType::Function { .. }, StructuredType::Tuple(_)) => false,
// ... etc
```

### Self Type Resolution

The algorithm handles `Self` type resolution in impl blocks:

```rust
// In impl Display for String { def show(value: Self): Self { ... } }
StructuredType::from_type_annotation(
    type_annotation,
    &mut interner,
    generic_params,
    Some(string_type_id), // Self resolves to String
)
```

**Self Resolution Rules:**

1. `Self` in protocol definitions → remains as generic parameter
2. `Self` in impl blocks → resolves to implementing type
3. `Self` in standalone functions → error (no context)
4. `Self` in generic arguments → recursive resolution (`Option<Self>` → `Option<ConcreteType>`)

### Generic Parameter Substitution

The algorithm supports generic parameter substitution:

```rust
// protocol Iterator<T> { def next(iter: Self): Option<T> }
// impl Iterator<String> for StringIterator { ... }

// During impl validation:
// Generic context: [("T", string_type_id)]
// Self context: string_iterator_type_id

// Function signature: (iter: Self) -> Option<T>
// Resolves to: (iter: StringIterator) -> Option<String>
```

### Error Cases and Debugging

#### Common Unification Failures

1. **Arity Mismatch**: `Option<T>` vs `Map<K, V>` (1 ≠ 2 arguments)
2. **Base Type Mismatch**: `Option<String>` vs `List<String>` (Option ≠ List)
3. **Argument Type Mismatch**: `Option<String>` vs `Option<Integer>` (String ≠ Integer)
4. **Parameter Name Mismatch**: `(name: String)` vs `(title: String)` (name ≠ title)
5. **Protocol Implementation Missing**: `Boolean` vs `String` (String doesn't implement Boolean)

#### Debugging Unification

```rust
// Enable debug output for unification failures
if cfg!(debug_assertions) {
    eprintln!("Unification failed: {} vs {}",
        type1.to_string_representation(&interner),
        type2.to_string_representation(&interner));
}
```

### Performance Considerations

#### Fast Paths

1. **Structural equality check first** (avoids protocol lookups)
2. **Early arity mismatch detection** (avoids recursive calls)
3. **TypeId interning** (fast equality for simple types)

#### Optimization Opportunities

1. **Memoization** for expensive protocol implementation checks
2. **Type canonicalization** for equivalent types
3. **Parallel unification** for independent generic arguments

### Integration with Type Checking

The unification algorithm integrates with key type checking phases:

1. **Function Return Type Checking**: Unify body type with declared return type
2. **Function Call Validation**: Unify argument types with parameter types
3. **Variable Assignment**: Unify expression type with variable type
4. **Pattern Matching**: Unify pattern type with matched expression type
5. **Protocol Implementation**: Unify impl function signatures with protocol signatures

This comprehensive unification system ensures that Outrun's type system correctly handles all combinations of generic types, protocols, and concrete types while providing clear error messages when unification fails.

## Key Design Principles

### Type Interning for Performance

All types, atoms, and protocols use interned IDs for fast equality comparisons:

```rust
let string_type = context.interner.intern_type("Outrun.Core.String");
let name_atom = context.interner.intern_atom("name");
let display_protocol = context.interner.intern_protocol("Display");
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
2. **Binary Operations**: Protocol-based dispatch validation
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

**Protocol Case**: Protocol implementation dispatch with orphan rule checking

```rust
case value as Display {
    String {} -> "string"
    Integer {} -> "number"
}
```

## Protocol System Architecture

### Protocol Definition Validation

- Function signature validation (parameters, return types)
- Guard function validation (must return Boolean)
- Generic parameter constraint checking
- Type name resolution (`Boolean` → `Outrun.Core.Boolean`)

### Protocol Implementation Tracking

```rust
// Register protocol implementations
let impl_id = context.protocol_registry.register_implementation(protocol_impl);

// Check protocol implementation
if context.protocol_registry.implements_protocol(type_id, protocol_id) {
    // Type implements protocol
}

// Exhaustiveness checking
let result = context.protocol_registry.check_protocol_case_exhaustiveness(
    protocol_id,
    covered_types
);
```

### Orphan Rule Analysis

The protocol system enforces orphan rules for protocol implementations:

- Can only implement protocols you own OR for types you own
- Prevents conflicting implementations
- Enables exhaustiveness checking for protocol case expressions

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
- `test_protocol_case_exhaustiveness.rs`: Protocol dispatch validation integration tests
- `test_protocol_case_patterns.rs`: Pattern integration with protocol cases integration tests
- `test_type_checker_core.rs`: TypeChecker core functionality unit tests
- `test_type_interning.rs`: Type interning system unit tests
- `test_protocols_unit.rs`: Protocol system component unit tests

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
3. **Add protocol definitions** in `types/protocols.rs` for new behaviour
4. **Update dispatch tables** if new protocol methods are added

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

### Protocol Implementation Lookup

- Hash-based protocol implementation storage
- Fast protocol membership checking
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
- Protocol dispatch tables for runtime method calls
- Static function lookup tables
- Pre-validated function signatures

## Common Patterns

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

// Protocol-specific errors
TypeError::ProtocolNotImplemented {
    span: span_to_source_span(span),
    protocol_name: protocol_name.clone(),
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

## Useful Commands

```bash
# Run type checker tests
cargo test --package outrun-typechecker

# Run specific test category
cargo test test_protocol_case_exhaustiveness

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
- Maintain protocol dispatch efficiency

### Advanced Error Recovery

- Multiple error reporting in single pass
- Error recovery strategies for parser integration
- IDE integration with real-time feedback

### Performance Optimization

- Incremental type checking for large codebases
- Parallel type checking for independent modules
- Advanced dispatch table optimization

This type checker provides the foundation for Outrun's static type system while maintaining the flexibility needed for protocol-based programming and comprehensive error reporting.
