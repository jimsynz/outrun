# Outrun Typechecker User Guide

## Overview

The Outrun typechecker provides static type checking for Outrun programs using Hindley-Milner type inference. It automatically infers types for expressions and validates protocol implementations without requiring explicit type annotations in most cases.

## How Type Inference Works

### Automatic Type Inference

The typechecker infers types for most expressions automatically:

```outrun
let x = 42          // Inferred as Integer64
let y = "hello"     // Inferred as String
let z = [1, 2, 3]   // Inferred as List<Integer64>
```

### Collection Types

The typechecker handles collection literals with proper type inference:

```outrun
// Lists - homogeneous types required
let numbers = [1, 2, 3]           // List<Integer64>
let names = ["alice", "bob"]      // List<String>

// Tuples - heterogeneous types allowed  
let person = ("alice", 25)        // Tuple<String, Integer64>
let coords = (10.5, 20.3, 5.0)   // Tuple<Float64, Float64, Float64>

// Maps - consistent key-value types
let ages = {"alice": 25, "bob": 30}   // Map<String, Integer64>
let flags = {"debug": true, "verbose": false}  // Map<String, Boolean>
```

### Operator Desugaring

All operators are transformed into protocol function calls:

```outrun
// These expressions...
let result = a + b
let negative = -x
let equal = x == y
let not_equal = x != y

// ...are automatically transformed to:
let result = BinaryAddition.add(left: a, right: b)
let negative = UnaryMinus.minus(value: x)
let equal = Equality.equal?(left: x, right: y)
let not_equal = LogicalNot.not?(value: Equality.equal?(left: x, right: y))
```

### Function Calls

The typechecker validates function calls and infers return types:

```outrun
// Function definition
def add_numbers(a: Integer64, b: Integer64): Integer64 {
    a + b  // Desugared to BinaryAddition.add(left: a, right: b)
}

// Function call - return type inferred as Integer64
let sum = add_numbers(a: 10, b: 20)
```

## Type Error Messages

### Common Type Errors

**Type Mismatch:**
```
Error: Type mismatch in function call
  ┌─ example.outrun:3:15
  │
3 │ let result = add_numbers(a: "hello", b: 42)
  │                         ^^^^^^^^^^^
  │
  = Expected Integer64 but found String for parameter 'a'
```

**Missing Protocol Implementation:**
```
Error: No implementation found for protocol
  ┌─ example.outrun:2:9
  │
2 │ let x = a + b
  │         ^^^^^
  │
  = No implementation of BinaryAddition found for types CustomType and Integer64
```

### Understanding Error Messages

Type errors include:
- **Precise location** using source spans
- **Expected vs actual types** for easy debugging  
- **Helpful suggestions** when possible
- **Context information** about where the error occurred

## Debugging Type Issues

### Enable Verbose Output

Use the typechecker in verbose mode to see type inference steps:

```rust
use outrun_typechecker::{typecheck_program, TypeInferenceEngine};

let mut engine = TypeInferenceEngine::new();
// engine.enable_debug_output();  // Future feature
```

### Common Patterns

**Empty Collections Need Type Hints:**
```outrun
// This might need a type annotation:
let empty_list = []  // Type cannot be inferred

// Better - provide context:
let empty_numbers: List<Integer64> = []
// Or use in context where type is known:
let all_numbers = List.append(first: [], second: [1, 2, 3])
```

**Protocol Implementation Requirements:**
```outrun
// Custom types need protocol implementations
struct Point { x: Float64, y: Float64 }

// To use + operator, implement BinaryAddition:
impl BinaryAddition<Point> for Point {
    def add(left: Point, right: Point): Point {
        Point { x: left.x + right.x, y: left.y + right.y }
    }
}

// Now this works:
let result = point1 + point2  // Desugars to BinaryAddition.add(...)
```

## Best Practices

### Type Annotations

While type inference handles most cases, explicit annotations help with:

1. **Public API boundaries** - make interfaces clear
2. **Complex generic types** - avoid inference ambiguity
3. **Performance critical code** - avoid inference overhead
4. **Documentation** - make intent explicit

```outrun
// Good practice for public functions
def process_items(items: List<String>): Result<List<ProcessedItem>, ProcessingError> {
    // Implementation with clear type contract
}
```

### Protocol Design

Follow protocol-first design principles:

```outrun
// Define behaviour first
protocol Serializable<T> {
    def serialize(value: T): String
    def deserialize(text: String): Result<T, ParseError>
}

// Then implement for types
impl Serializable<Person> for Person {
    def serialize(value: Person): String { /* ... */ }
    def deserialize(text: String): Result<Person, ParseError> { /* ... */ }
}
```

### Error Handling

Use Result types for functions that can fail:

```outrun
def divide(a: Float64, b: Float64): Result<Float64, DivisionError> 
when Float64.non_zero?(b) {
    Result.ok(value: a / b)
} otherwise {
    Result.error(error: DivisionError.division_by_zero())
}
```

## Integration Examples

### Package-Level Type Checking

The typechecker processes entire packages together:

```rust
use outrun_typechecker::{Package, typecheck_package};
use outrun_parser::parse_program;

// Create a package
let mut package = Package::new("my_package".to_string());

// Add multiple source files
let core_src = std::fs::read_to_string("src/core.outrun")?;
let utils_src = std::fs::read_to_string("src/utils.outrun")?;

package.add_program(parse_program(&core_src)?);
package.add_program(parse_program(&utils_src)?);

// Type check the entire package
typecheck_package(&mut package)?;
```

### Single File Type Checking

For testing or simple cases:

```rust
use outrun_typechecker::typecheck_program;
use outrun_parser::parse_program;

let source = r#"
    let x = 42
    let y = x + 10
"#;

let mut program = parse_program(source)?;
typecheck_program(&mut program)?;

// Program now has type information attached
```

### Accessing Type Information

After type checking, AST nodes contain type information:

```rust
// Access inferred types from expressions
for item in &program.items {
    if let outrun_parser::ItemKind::Expression(expr) = &item.kind {
        if let Some(type_info) = &expr.type_info {
            println!("Expression type: {}", type_info.resolved_type);
        }
    }
}
```

## Limitations and Future Work

### Current Limitations

- **No generic type parameters** in function definitions yet
- **Limited exhaustiveness checking** for pattern matching
- **No module-level type inference** across package boundaries
- **Basic error recovery** - stops at first error

### Planned Features

- **Generic function support** with type parameter inference
- **Advanced pattern matching** with exhaustiveness checking
- **Cross-package type checking** with compilation caching
- **Enhanced error messages** with fix suggestions
- **IDE integration** for real-time type checking

## Troubleshooting

### Performance Issues

For large codebases:
- **Type check incrementally** - only changed files
- **Use package boundaries** - avoid monolithic packages  
- **Profile type checking** - identify slow inference patterns

### Memory Usage

The typechecker maintains type information in memory:
- **Process files in batches** for very large packages
- **Clear intermediate results** after package completion
- **Monitor AST growth** with attached type information

### Integration Problems

Common integration issues:
- **Parser version compatibility** - ensure matching versions
- **AST structure changes** - update typechecker when parser changes
- **Error handling** - propagate typechecker errors properly

For more detailed technical information, see the [Architecture Guide](ARCHITECTURE.md).