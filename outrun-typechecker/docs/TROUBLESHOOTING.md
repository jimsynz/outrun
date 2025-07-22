# Typechecker Troubleshooting Guide

## Common Type Errors

### 1. "No implementation found for protocol"

**Error Example**:
```
Error: No implementation found for protocol BinaryAddition
  ┌─ src/main.outrun:5:15
  │
5 │ let result = a + b
  │               ^^^^^
  │
  = No implementation of BinaryAddition found for types CustomType and Integer64
```

**Root Causes**:
- Using operators on custom types without implementing the required protocols
- Type mismatch between operands
- Missing protocol imports

**Solutions**:

1. **Implement the required protocol**:
```outrun
struct CustomType { value: Integer64 }

impl BinaryAddition<CustomType> for CustomType {
    def add(left: CustomType, right: CustomType): CustomType {
        CustomType { value: left.value + right.value }
    }
}
```

2. **Convert types to compatible ones**:
```outrun
// Instead of: custom_value + 42
let result = custom_value.value + 42
```

3. **Check for typos in type names** - the typechecker is case-sensitive.

### 2. "Cannot infer type for empty collection"

**Error Example**:
```
Error: Cannot infer type for empty collection
  ┌─ src/main.outrun:2:18
  │
2 │ let empty_list = []
  │                  ^^
  │
  = Empty collections need type context or explicit annotation
```

**Root Cause**: The typechecker cannot determine what type of elements an empty collection should contain.

**Solutions**:

1. **Provide explicit type annotation**:
```outrun
let empty_numbers: List<Integer64> = []
let empty_names: List<String> = []
```

2. **Use in context where type can be inferred**:
```outrun
// Type inferred from the second list
let all_numbers = List.append(first: [], second: [1, 2, 3])

// Type inferred from function parameter
def process_numbers(numbers: List<Integer64>): Integer64 {
    List.length(numbers)
}
let result = process_numbers([])  // Empty list inferred as List<Integer64>
```

3. **Initialize with at least one element** if possible:
```outrun
let numbers = [0]  // Then remove/replace the placeholder if needed
```

### 3. "Type mismatch in function call"

**Error Example**:
```
Error: Type mismatch in function call
  ┌─ src/main.outrun:4:25
  │
4 │ let result = calculate(a: "hello", b: 42)
  │                        ^^^^^^^^^^^
  │
  = Expected Integer64 but found String for parameter 'a'
  = Function signature: calculate(a: Integer64, b: Integer64) -> Integer64
```

**Root Causes**:
- Passing wrong type to function parameter
- Function signature mismatch
- Type inference produced unexpected result

**Solutions**:

1. **Convert argument to expected type**:
```outrun
// If string represents a number
let result = calculate(a: String.to_integer("42"), b: 42)

// If you need different overload
def calculate(a: String, b: Integer64): String {
    a + Integer64.to_string(b)
}
```

2. **Check function signature** - ensure you're calling the right function:
```outrun
// Make sure this is the function you intended:
def calculate(a: Integer64, b: Integer64): Integer64 { ... }
```

3. **Verify type inference** - check what type the typechecker inferred:
```rust
// In debugging mode, you can see inferred types
```

### 4. "Unification failed" or "Occurs check failed"

**Error Example**:
```
Error: Unification failed - occurs check
  ┌─ src/main.outrun:3:9
  │
3 │ let f = fn(x) { f(x) }
  │         ^^^^^^^^^^^^^^
  │
  = Cannot create infinite type: T = T -> U
```

**Root Cause**: Attempting to create recursive types or infinite type structures.

**Solutions**:

1. **Avoid problematic recursive patterns**:
```outrun
// Instead of infinite recursion:
// let f = fn(x) { f(x) }

// Use proper recursive structure:
def factorial(n: Integer64): Integer64 
when n <= 1 {
    1
} otherwise {
    n * factorial(n - 1)
}
```

2. **Add explicit type annotations** to break inference cycles:
```outrun
def problematic_function(x: SomeSpecificType): ReturnType {
    // Implementation with clear types
}
```

## Performance Issues

### Slow Type Checking

**Symptoms**:
- Type checking takes a long time
- High memory usage during compilation
- Compiler appears to hang

**Debugging Steps**:

1. **Profile your code structure**:
```rust
// Enable timing information (if available in debug mode)
// Check for deeply nested expressions
// Look for very large collection literals
```

2. **Check for pathological cases**:
- Deeply nested function calls
- Very large collection literals
- Complex generic type hierarchies
- Circular dependencies in protocol implementations

**Solutions**:

1. **Break down large expressions**:
```outrun
// Instead of:
let result = very_long_expression + another_long_expression + third_expression

// Use intermediate variables:
let part1 = very_long_expression
let part2 = another_long_expression  
let part3 = third_expression
let result = part1 + part2 + part3
```

2. **Simplify generic hierarchies**:
```outrun
// Avoid overly complex nested generics
// Use type aliases for complex types
alias ComplexType = Map<String, List<Tuple<Integer64, Result<String, Error>>>>
```

3. **Check for exponential blowup**:
- Look for repeated subexpressions that cause duplicate work
- Consider memoization strategies (future typechecker versions)

### High Memory Usage

**Symptoms**:
- Compiler runs out of memory
- Memory usage grows continuously during type checking

**Solutions**:

1. **Process files incrementally**:
```rust
// Instead of loading entire large package:
let mut package = Package::new("large_package".to_string());
// Add files in smaller batches

// Process each batch:
for file_batch in file_batches {
    for file in file_batch {
        package.add_program(parse_program(&file)?);
    }
    typecheck_package(&mut package)?;
    // Clear intermediate results if possible
}
```

2. **Split large files** into smaller modules.

3. **Remove unnecessary type information** after use (if API allows).

## Integration Issues

### Parser Version Mismatch

**Error Example**:
```
Error: Compilation failed
  │
  = Mismatched AST structure - expected Expression.type_info field
```

**Solution**: Ensure typechecker and parser versions are compatible:
```toml
[dependencies]
outrun-parser = "0.3.0"
outrun-typechecker = "0.3.0"  # Must match major.minor version
```

### AST Structure Changes

**Error Example**:
```
Error: Field not found
  │
  = Expression struct missing expected field 'span' or 'type_info'
```

**Solutions**:

1. **Update to compatible versions** of parser and typechecker.

2. **Check breaking changes** in release notes.

3. **Rebuild clean** to ensure no stale dependencies:
```bash
cargo clean
cargo build
```

### Error Handling Integration

**Problem**: Typechecker errors not properly propagated or displayed.

**Solutions**:

1. **Proper error handling**:
```rust
match typecheck_program(&mut program) {
    Ok(_) => println!("Type checking successful"),
    Err(CompilerError::Typechecker(e)) => {
        eprintln!("Type error: {:?}", e);
        // Handle typechecker-specific errors
    }
    Err(CompilerError::Parser(e)) => {
        eprintln!("Parse error: {:?}", e);
        // Handle parser errors
    }
    Err(e) => {
        eprintln!("Compilation error: {:?}", e);
    }
}
```

2. **Error display integration**:
```rust
use miette::{Diagnostic, SourceSpan};

// Ensure errors implement proper Display traits
// Use miette for pretty error reporting
```

## Debugging Strategies

### Enable Debug Output

```rust
// Set environment variable for debug output
std::env::set_var("OUTRUN_DEBUG", "1");

// Or use debug build
cargo build --debug
```

### Manual AST Inspection

```rust
fn debug_program_structure(program: &Program) {
    for (i, item) in program.items.iter().enumerate() {
        println!("Item {}: {:?}", i, item.kind);
        
        // Inspect expressions for type information
        match &item.kind {
            ItemKind::Expression(expr) => {
                println!("  Type info: {:?}", expr.type_info);
                debug_expression_tree(expr, 1);
            }
            _ => {}
        }
    }
}

fn debug_expression_tree(expr: &Expression, depth: usize) {
    let indent = "  ".repeat(depth);
    println!("{}Expression: {:?}", indent, expr.kind);
    println!("{}Type: {:?}", indent, expr.type_info);
    
    // Recursively inspect nested expressions
    // ... implement based on ExpressionKind variants
}
```

### Isolated Testing

Create minimal test cases to isolate problems:

```rust
#[test]
fn test_minimal_problem() {
    // Reduce your problem to the smallest possible example
    let source = "let x = problematic_expression";
    let mut program = parse_program(source).unwrap();
    
    // This should help isolate the specific issue
    let result = typecheck_program(&mut program);
    // Add detailed assertions about what you expect vs. what happens
}
```

## Getting Help

### Information to Provide

When reporting typechecker issues, include:

1. **Outrun source code** that demonstrates the problem
2. **Full error message** including source location
3. **Versions** of outrun-parser and outrun-typechecker  
4. **Environment** (OS, Rust version, etc.)
5. **Expected behavior** vs. actual behavior

### Example Bug Report Template

```
## Problem Description
Brief description of the issue

## Source Code
```outrun
// Minimal code that reproduces the problem
let example = problematic_code
```

## Error Message
```
Full error output here
```

## Environment
- outrun-parser version: 0.3.0
- outrun-typechecker version: 0.3.0  
- Rust version: 1.75.0
- OS: Ubuntu 22.04

## Expected Behavior
What you expected to happen

## Actual Behavior  
What actually happened
```

### Performance Issues

For performance problems, also include:
- **Code size** (number of lines, expressions, etc.)
- **Timing information** (how long type checking takes)
- **Memory usage** (peak memory consumption if known)
- **Hardware specs** (CPU, RAM)

### Community Resources

- **GitHub Issues**: Report bugs and request features
- **Discussions**: Ask questions and share solutions
- **Documentation**: Check for updates and examples
- **Source Code**: Dive into implementation for deep debugging

Remember: The typechecker is actively developed, so issues you encounter may already be fixed in newer versions. Always try updating to the latest version first.