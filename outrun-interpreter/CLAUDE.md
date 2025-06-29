# Working with the Outrun Interpreter

## Project Overview

The Outrun interpreter is an expression-mode interpreter that executes typed expressions from the Outrun typechecker. It's designed primarily for REPL usage and focuses on functional programming patterns with immutable data structures.

**Current Status**: ✅ **FUNCTIONAL** - Core interpreter with REPL support, expression evaluation, and function dispatch

**📖 Language Reference**: See the main `/CLAUDE.md` for critical Outrun language differences and common mistakes to avoid.

## Outrun Language Architecture

### Core Concepts

#### Traits vs Concrete Types
- **Traits**: Define behaviour interfaces (like `List<T>`, `Option<T>`)
- **Concrete Types**: Actual runtime types (like `Outrun.Core.List<T>`, `Outrun.Option.Some<T>`)
- **Static Functions**: Functions defined on traits that create or operate on concrete types

#### Constructor Pattern
```outrun
# Traits define static constructor functions
trait Option<T> {
    defs some(value: T): Outrun.Option.Some<T> {
        Outrun.Option.Some { value: value }  # Struct construction
    }
    
    defs none(): Outrun.Option.None {
        Outrun.Option.None { }  # Empty struct construction
    }
}

# Usage: static function calls, not constructors
let opt = Option.some(value: 42)  # Creates Outrun.Option.Some<Integer> struct
let empty = Option.none()         # Creates Outrun.Option.None struct
```

#### Intrinsics System
Most operations are implemented as intrinsics that the interpreter must handle:

```outrun
# Core library implementation
impl<T> List<T> for Outrun.Core.List<T> {
    def head(value: Self): Option<T> {
        Outrun.Intrinsic.list_head(value: value)  # Intrinsic call
    }
    
    def prepend(list: Self, elem: T): Self {
        Outrun.Intrinsic.list_prepend(list: list, elem: elem)  # Intrinsic call
    }
}
```

### Data Structures

#### Lists are Linked Lists (NOT Vectors)
```rust
// ✅ CORRECT - Lists are linked lists for functional programming
#[derive(Debug, Clone, PartialEq)]
pub enum List {
    Empty,
    Cons { head: Value, tail: Box<List> },
}

// Operations are optimized for head/tail access:
// - list.head()  -> O(1)
// - list.tail()  -> O(1) 
// - list.cons()  -> O(1)
// - list[index] -> O(n) - avoid this!
```

#### No Direct Enum Access
Values are created through function calls, not direct construction:

```rust
// ❌ WRONG - Direct enum construction
Value::Some(Box::new(value))  // Don't create enums directly

// ✅ CORRECT - Through function evaluation
// Outrun: Option.some(value: 42)
// Interpreter: evaluate_function_call("Option", "some", [42]) -> creates Value::Some(...)
```

## Interpreter Architecture

### Recent Improvements

**TypeInterner Singleton Pattern**: Fixed critical memory leaks and type ID consistency issues by:
- Eliminated `Box::leak` memory leaks in `InterpreterContext`
- Implemented proper singleton sharing for `TypeInterner` in production code
- Added test isolation via conditional compilation to prevent cross-test pollution
- Ensured consistent type IDs across all interpreter components

**Thread-Safe Function Registry**: Implemented `Arc<RwLock<>>` pattern for:
- Shared function registries between typechecker and interpreter
- Preserved typed function definitions across component boundaries
- Thread-safe access for concurrent scenarios

### Core Components

#### 1. Value System (`src/value.rs`)
- **Purpose**: Runtime representation of all Outrun values
- **Key Point**: Uses Rust enums internally but created via function calls
- **Integration**: Maps to typechecker's `StructuredType` system

#### 2. Expression Evaluator (`src/evaluator.rs`)
- **Purpose**: Execute `TypedExpression` from typechecker
- **Key Point**: All expressions return `Value` (never unit)
- **Integration**: Uses typechecker's dispatch resolution

#### 3. Intrinsics Handler (`src/intrinsics.rs`)
- **Purpose**: Execute `Outrun.Intrinsic.*` function calls
- **Key Point**: These do the actual work (list ops, arithmetic, etc.)
- **Examples**: `list_head`, `list_prepend`, `add_integer64`

#### 4. Function Dispatch (`src/function_calls.rs`)
- **Purpose**: Route function calls to correct implementations
- **Types**: Static trait functions, intrinsics, user-defined functions
- **Key Point**: No method calls - everything is function calls

#### 5. REPL Integration (`src/repl.rs`)
- **Purpose**: Interactive expression evaluation
- **Pipeline**: Parse → Type Check → Interpret
- **Persistence**: Variable bindings maintained across expressions

### Type Integration

#### StructuredType ↔ Value Mapping
```rust
// Typechecker provides StructuredType, interpreter creates Value
StructuredType::Simple(Integer64_TypeId) -> Value::Integer64(42)
StructuredType::Generic { 
    base: List_TypeId, 
    args: [Integer64_TypeId] 
} -> Value::List(List::Cons { ... })

StructuredType::Generic { 
    base: Option_TypeId, 
    args: [String_TypeId] 
} -> Value::Struct { 
    type_id: Some_TypeId, 
    fields: {"value": Value::String("hello")} 
}
```

#### No Type Inference
The interpreter doesn't do type inference - all type information comes from the typechecker:
- Expression types: `TypedExpression.structured_type`
- Function signatures: Pre-resolved in `DispatchMethod`
- Pattern types: Validated by typechecker

## Implementation Guidelines

### Default Value Creation
**Important**: The interpreter does NOT implement default value creation directly. Default values are handled through Outrun's `Default` trait via normal function dispatch:

```rust
// ❌ WRONG - Don't hardcode defaults in interpreter
fn create_default_boolean() -> Value {
    Value::boolean(false)  // Don't do this!
}

// ✅ CORRECT - Use trait function dispatch with type casting
// Outrun: Default.default() as Boolean
// Interpreter: evaluate_trait_function("Default", "default", [], target_type: Boolean) -> Value::boolean(false)
```

**Key Point**: Trait functions are called on the trait (`Default.default()`), not on implementing types (`Boolean.default()`). The type system handles dispatch to the correct implementation.

### Function Call Resolution
1. **Check if it's an intrinsic**: `Outrun.Intrinsic.*` -> execute directly
2. **Check dispatch strategy**: Use typechecker's resolution
3. **Execute with named parameters**: Extract argument values by name
4. **Return Value**: Every function call produces a `Value`

### Pattern Matching
```rust
// Pattern matching works on Value enums created by function calls
match value {
    Value::Struct { type_id, fields, .. } if is_some_type(type_id) => {
        // Option.some case - extract "value" field
        let inner = fields.get("value").unwrap();
        // ... bind variables
    }
    Value::Struct { type_id, .. } if is_none_type(type_id) => {
        // Option.none case - no fields to extract
        // ... continue matching
    }
    // ... other patterns
}
```

### Control Flow
```rust
// If expressions require Boolean condition (no truthiness)
match condition_value {
    Value::Boolean(true) => evaluate_then_branch(),
    Value::Boolean(false) => evaluate_else_branch(),
    _ => Err(RuntimeError::InvalidConditionType),  // Not a boolean!
}
```

### Error Handling
```rust
// All runtime errors should preserve source spans from TypedExpression
RuntimeError::TypeMismatch {
    expected: String,
    found: String,
    span: expr.span,  // Preserve original source location
}
```

## Interpreter-Specific Implementation Notes

### Value Creation Patterns
```rust
// ✅ CORRECT - Values created through function evaluation, not direct construction
// Outrun: Option.some(value: 42)
// Interpreter: evaluate_function_call("Option", "some", [42]) -> creates Value::Struct { ... }

// ✅ CORRECT - Lists use linked structure, not vectors
let list = List::Cons { head: Value::Integer64(1), tail: Box::new(List::Empty) };
let head = list.head();  // O(1) access

// ✅ CORRECT - All interpreter functions return Value, never unit
fn evaluate_expression(expr: &TypedExpression) -> Result<Value, RuntimeError> { ... }
```

## Testing Approach

### Unit Tests
- **Value operations**: Creation, type mapping, display
- **List operations**: Head, tail, cons, append (functional style)
- **Function dispatch**: Static vs trait vs intrinsic routing
- **Pattern matching**: Struct pattern binding, list destructuring

### Integration Tests
- **REPL simulation**: Multi-expression sessions with variable persistence
- **Core library integration**: Option/List/Result operations
- **Error handling**: Source span preservation, meaningful messages

### REPL Testing
```rust
#[test]
fn test_repl_session() {
    let mut repl = ReplSession::new().unwrap();
    
    // Test variable binding
    let result1 = repl.evaluate_line("let x = 42").unwrap();
    assert_eq!(result1, Value::Integer64(42));
    
    // Test variable access
    let result2 = repl.evaluate_line("x").unwrap();
    assert_eq!(result2, Value::Integer64(42));
    
    // Test function calls
    let result3 = repl.evaluate_line("Option.some(value: x)").unwrap();
    assert!(matches!(result3, Value::Struct { .. }));
}
```

## Performance Considerations

### Linked List Efficiency
- **Optimal**: Head/tail access, cons operations, pattern matching
- **Acceptable**: Map, filter, fold operations
- **Avoid**: Random access by index (O(n))

### Memory Management
- **Immutable values**: Clone for modification
- **Reference counting**: For large shared structures
- **Tail sharing**: Efficient linked list sharing

### Function Call Overhead
- **Static dispatch**: Direct registry lookup (fast)
- **Trait dispatch**: Hash table lookup (fast)
- **Intrinsics**: Direct execution (fastest)

## Future Extensions

### Advanced Pattern Matching
- **Guard expressions**: Pattern matching with conditions
- **Nested patterns**: Deep destructuring support
- **Exhaustiveness**: Compile-time completeness checking

### Performance Optimization
- **Tail call optimization**: For recursive functions
- **Value interning**: For commonly used values
- **Lazy evaluation**: For large collections

### Debugging Support
- **Stack traces**: Function call history with spans
- **Variable inspection**: REPL debugging commands
- **Step-through**: Expression-level debugging

This interpreter provides the foundation for Outrun's functional programming model while maintaining the language's unique characteristics around trait-based programming and immutable data structures.