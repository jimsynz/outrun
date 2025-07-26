# Typechecker Examples

## Complete Working Examples

### Example 1: Basic Type Inference

**Source Code** (`examples/basic_inference.outrun`):
```outrun
// Simple variable bindings
let number = 42
let text = "hello world"
let flag = true

// Collection literals
let numbers = [1, 2, 3, 4, 5]
let coordinates = (10.5, 20.3)
let person_data = {"name": "Alice", "age": 30}

// Arithmetic operations (desugared to protocol calls)
let sum = number + 10
let product = number * 2
let negative = -number
```

**Type Checking Result**:
```
✓ number: Integer64
✓ text: String  
✓ flag: Boolean
✓ numbers: List<Integer64>
✓ coordinates: Tuple<Float64, Float64>
✓ person_data: Map<String, Integer64>
✓ sum: Integer64 (via BinaryAddition.add)
✓ product: Integer64 (via BinaryMultiplication.multiply)  
✓ negative: Integer64 (via UnaryMinus.minus)
```

### Example 2: Function Definition and Calls

**Source Code** (`examples/functions.outrun`):
```outrun
// Function with explicit types
def calculate_area(width: Float64, height: Float64): Float64 {
    width * height  // Desugared to BinaryMultiplication.multiply
}

// Function with inferred return type
def greet_person(name: String, age: Integer64) {
    "Hello " + name + ", you are " + Integer64.to_string(age) + " years old"
}

// Function calls
let area = calculate_area(width: 10.0, height: 5.0)
let greeting = greet_person(name: "Bob", age: 25)

// Higher-order function usage
let doubled_numbers = List.map(list: [1, 2, 3], func: fn(x) { x * 2 })
```

**Type Checking Result**:
```
✓ calculate_area: Function<(width: Float64, height: Float64) -> Float64>
✓ greet_person: Function<(name: String, age: Integer64) -> String>
✓ area: Float64
✓ greeting: String
✓ doubled_numbers: List<Integer64>
```

### Example 3: Protocol Implementation

**Source Code** (`examples/protocols.outrun`):
```outrun
// Custom struct
struct Point { 
    x: Float64, 
    y: Float64 
}

// Protocol implementation for custom addition
impl BinaryAddition<Point> for Point {
    def add(left: Point, right: Point): Point {
        Point { 
            x: left.x + right.x,  // Float64 addition
            y: left.y + right.y   // Float64 addition
        }
    }
}

// Usage - now Point supports + operator
let point1 = Point { x: 1.0, y: 2.0 }
let point2 = Point { x: 3.0, y: 4.0 }
let sum = point1 + point2  // Desugars to BinaryAddition.add

// String representation protocol
impl ToString<Point> for Point {
    def to_string(value: Point): String {
        "Point(" + Float64.to_string(value.x) + ", " + Float64.to_string(value.y) + ")"
    }
}

let point_str = ToString.to_string(value: point1)
```

**Type Checking Result**:
```
✓ Point: struct { x: Float64, y: Float64 }
✓ BinaryAddition implementation registered for Point
✓ point1: Point
✓ point2: Point  
✓ sum: Point (via desugared BinaryAddition.add call)
✓ ToString implementation registered for Point
✓ point_str: String
```

### Example 4: Complex Collections and Nesting

**Source Code** (`examples/collections.outrun`):
```outrun
// Nested collections
let matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
let mixed_tuples = [("alice", 25), ("bob", 30), ("charlie", 35)]

// Map with complex values
let user_profiles = {
    "alice": {"age": 25, "email": "alice@example.com"},
    "bob": {"age": 30, "email": "bob@example.com"}
}

// Operations on collections
let first_row = List.head(value: matrix)
let user_ages = List.map(
    list: mixed_tuples, 
    func: fn(tuple) { 
        let (name, age) = tuple
        age
    }
)

// Nested operations with type inference
let total_age = List.fold(
    list: user_ages,
    initial: 0,
    func: fn(acc, age) { acc + age }  // Both additions inferred
)
```

**Type Checking Result**:
```
✓ matrix: List<List<Integer64>>
✓ mixed_tuples: List<Tuple<String, Integer64>>
✓ user_profiles: Map<String, Map<String, String>>
✓ first_row: Option<List<Integer64>>
✓ user_ages: List<Integer64>
✓ total_age: Integer64
```

### Example 5: Error Handling Patterns

**Source Code** (`examples/error_handling.outrun`):
```outrun
// Function that can fail
def safe_divide(a: Float64, b: Float64): Result<Float64, String>
when Float64.non_zero?(b) {
    Result.ok(value: a / b)  // Division desugared to protocol call
} otherwise {
    Result.error(error: "Division by zero")
}

// Pattern matching on Result
def handle_division(numerator: Float64, denominator: Float64): String {
    case safe_divide(a: numerator, b: denominator) {
        Ok(value) -> "Result: " + Float64.to_string(value)
        Error(message) -> "Error: " + message
    }
}

// Chaining operations
let calculations = [
    safe_divide(a: 10.0, b: 2.0),   // Ok(5.0)
    safe_divide(a: 8.0, b: 4.0),    // Ok(2.0)
    safe_divide(a: 5.0, b: 0.0)     // Error("Division by zero")
]

let results = List.map(
    list: calculations,
    func: fn(result) {
        case result {
            Ok(value) -> Float64.to_string(value)
            Error(message) -> "ERR: " + message
        }
    }
)
```

**Type Checking Result**:
```
✓ safe_divide: Function<(a: Float64, b: Float64) -> Result<Float64, String>>
✓ handle_division: Function<(numerator: Float64, denominator: Float64) -> String>
✓ calculations: List<Result<Float64, String>>
✓ results: List<String>
✓ Pattern matching exhaustiveness verified
```

## Common Type Errors and Solutions

### Error 1: Type Mismatch in Operations

**Problematic Code**:
```outrun
let number = 42
let text = "hello"
let result = number + text  // ERROR: Type mismatch
```

**Error Message**:
```
Error: No implementation found for protocol BinaryAddition
  ┌─ example.outrun:3:14
  │
3 │ let result = number + text
  │              ^^^^^^^^^^^^^
  │
  = No implementation of BinaryAddition found for types Integer64 and String
  = Consider implementing BinaryAddition<Integer64, String> or converting types
```

**Solution**:
```outrun
// Convert to compatible types
let result = Integer64.to_string(number) + text
// Or implement the protocol for mixed types (if semantically valid)
```

### Error 2: Empty Collection Inference

**Problematic Code**:
```outrun
let empty_list = []  // ERROR: Cannot infer type
let first = List.head(value: empty_list)
```

**Error Message**:
```
Error: Cannot infer type for empty collection
  ┌─ example.outrun:1:18
  │
1 │ let empty_list = []
  │                  ^^
  │
  = Empty collections need type context or explicit annotation
  = Try: let empty_list: List<T> = [] or use in typed context
```

**Solution**:
```outrun
// Explicit type annotation
let empty_list: List<String> = []

// Or provide context where type can be inferred
let numbers = List.append(first: [], second: [1, 2, 3])  // First list inferred as List<Integer64>
```

### Error 3: Missing Protocol Implementation

**Problematic Code**:
```outrun
struct CustomType { value: Integer64 }
let a = CustomType { value: 10 }
let b = CustomType { value: 20 }
let sum = a + b  // ERROR: No BinaryAddition implementation
```

**Error Message**:
```
Error: No implementation found for protocol BinaryAddition
  ┌─ example.outrun:4:11
  │
4 │ let sum = a + b
  │           ^^^^^
  │
  = No implementation of BinaryAddition found for type CustomType
  = To fix: implement BinaryAddition<CustomType> for CustomType
```

**Solution**:
```outrun
impl BinaryAddition<CustomType> for CustomType {
    def add(left: CustomType, right: CustomType): CustomType {
        CustomType { value: left.value + right.value }
    }
}
```

## Testing Examples

### Unit Test for Type Checking

```rust
#[test]
fn test_basic_arithmetic_inference() {
    let source = "let result = 10 + 20 * 2";
    let mut program = parse_program(source).unwrap();
    
    // Type check the program
    typecheck_program(&mut program).unwrap();
    
    // Verify the result type
    if let Some(item) = program.items.first() {
        if let ItemKind::LetBinding(binding) = &item.kind {
            assert_eq!(
                binding.expression.type_info.as_ref().unwrap().resolved_type,
                "Integer64"
            );
        }
    }
}
```

### Integration Test with Custom Types

```rust
#[test]
fn test_custom_protocol_implementation() {
    let source = r#"
        struct Point { x: Float64, y: Float64 }
        
        impl BinaryAddition<Point> for Point {
            def add(left: Point, right: Point): Point {
                Point { x: left.x + right.x, y: left.y + right.y }
            }
        }
        
        let p1 = Point { x: 1.0, y: 2.0 }
        let p2 = Point { x: 3.0, y: 4.0 }  
        let sum = p1 + p2
    "#;
    
    let mut program = parse_program(source).unwrap();
    
    // Should not fail - Point + Point is valid due to implementation
    typecheck_program(&mut program).unwrap();
    
    // Verify sum has Point type
    // ... test type information extraction
}
```

## Performance Examples

### Benchmark: Large Program Type Checking

```rust
use std::time::Instant;

fn benchmark_large_program_typechecking() {
    // Generate a large program with many expressions
    let mut source = String::new();
    for i in 0..1000 {
        source.push_str(&format!("let var_{} = {} + {} * 2\n", i, i, i + 1));
    }
    
    let start = Instant::now();
    
    let mut program = parse_program(&source).unwrap();
    typecheck_program(&mut program).unwrap();
    
    let duration = start.elapsed();
    println!("Type checked 1000 expressions in {:?}", duration);
}
```

### Memory Usage Analysis

```rust
fn analyze_memory_usage() {
    let source = include_str!("large_example.outrun");
    
    let memory_before = get_memory_usage();
    
    let mut program = parse_program(source).unwrap();
    typecheck_program(&mut program).unwrap();
    
    let memory_after = get_memory_usage();
    
    println!(
        "Type checking added {} KB of memory usage", 
        (memory_after - memory_before) / 1024
    );
}
```

## Advanced Examples

### Example: Generic Protocol Implementation (Future)

```outrun
// This is planned for future versions
protocol Comparable<T> {
    def compare(left: T, right: T): Ordering
}

impl Comparable<List<T>> for List<T> 
where T: Comparable<T> {
    def compare(left: List<T>, right: List<T>): Ordering {
        // Lexicographic comparison implementation
    }
}
```

### Example: Module System Integration (Future)

```outrun
// File: src/math/vector.outrun
module Math.Vector

struct Vector3D { x: Float64, y: Float64, z: Float64 }

// File: src/physics/motion.outrun  
module Physics.Motion

import Math.Vector.Vector3D

def calculate_velocity(position: Vector3D, time: Float64): Vector3D {
    // Implementation using imported type
}
```

For more examples, see the `examples/` directory in the outrun repository.