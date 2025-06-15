# Phase 4 Implementation Plan: Function System

## Overview

**Goal**: Complete the Outrun function system with comprehensive function definition validation, enhanced function call resolution, and guard-based function overloading.

**Current State**: 
- ✅ Basic function calls work with named parameter validation and type checking
- ✅ Strong foundation: Type system, traits, scope management all complete
- ❌ **Critical gap**: Function definition validation is completely stubbed out
- ❌ Missing features: Qualified calls, function capture, guard validation, body type checking

**Target State**:
- ✅ Complete function definition validation with parameter and return type checking
- ✅ Enhanced function call resolution (qualified calls, function capture)
- ✅ Guard-based function overloading with exhaustiveness checking
- ✅ 20+ comprehensive tests covering all function system aspects
- ✅ Beautiful error reporting for function-related issues

## Design Principles

### Return Type Requirements
- **All functions must have explicit return types** - no functions without return type annotations
- **All functions must return a value** - every function body contains at least one expression
- Function body type must exactly match declared return type

### Generic Type Support (Simplified)
- **No generic function declarations**: Functions do NOT declare generic parameters like `def foo<T>()`
- **Use existing generic types**: Functions can use generic types in signatures (e.g., `List<String>`, `Option<User>`)
- **No function-level generic constraints**: Only traits and structs have generic constraints
- **No generic type arguments on calls**: No support for `function<Type>(args)` syntax
- **Self type**: `Self` is a special generic that always refers to the implementing concrete type

### Guard System
- Functions ending with `?` must return `Outrun.Core.Boolean`
- Guard functions must be side-effect free
- Guard-based function overloading supported

## Phase 4.1: Function Signature Validation

### Step 1: Complete Function Definition Type Checking
**Location**: `outrun-typechecker/src/checker/functions.rs`

**Current State**: Method is completely stubbed with TODO comments

**Tasks**:
1. **Parameter validation**:
   - Validate all parameter types exist in type registry
   - Check parameter name uniqueness within function
   - Handle existing generic types in parameters (e.g., `List<String>`, `Option<User>`)
   - Validate type annotations are well-formed

2. **Return type validation**:
   - All functions must have explicit return types (no defaults)
   - Validate return type exists in type registry
   - Handle existing generic types in return values (e.g., `Option<User>`)
   - Handle `Self` type references (resolves to implementing concrete type)
   - Ensure return type syntax is well-formed

3. **Guard clause validation**:
   - Functions ending with `?` must return `Outrun.Core.Boolean`
   - Guard functions must be side-effect free (validate recursively)
   - Implement guard expression type checking

**Example Valid Functions**:
```outrun
def find_user(id: Integer64): Option<User> { ... }
def process_list(items: List<String>): List<String> { ... }
def is_valid?(user: User): Boolean { ... }  // Guard function
def log_message(msg: String): Option { ... }  // Explicit Option return
```

**Expected Output**: Complete `check_function_definition()` method with comprehensive validation

### Step 2: Function Body Type Checking Integration
**Location**: Integrate with existing expression checker

**Tasks**:
1. **Block expression validation**: Use existing block type checking from expressions.rs
2. **Return type compatibility**: Ensure function body type matches declared return type exactly
3. **Guard clause body checking**: Validate guard functions only call other guard functions
4. **Option type handling**: Properly handle functions that explicitly return `Option`

**Expected Output**: Full function definition validation with strict return type checking

## Phase 4.2: Function Call Resolution

### Step 3: Enhanced Function Call Validation
**Location**: `outrun-typechecker/src/checker/expressions.rs`

**Current State**: ✅ Basic function calls work well with named parameter validation

**Tasks**:
1. **Qualified function calls**: Implement `Module.function_name()` support
   - Module/namespace resolution
   - Function lookup within specific modules
   - Access control validation

2. **Function capture**: Implement `&function_name` syntax support
   - Function reference validation
   - Function type generation for captured functions
   - Scope-aware capture validation

3. **Generic type propagation**: Ensure return types like `Option<User>` propagate correctly
   - Handle complex generic return types
   - Type substitution in function call results

**Example Function Calls**:
```outrun
// Basic calls (already working)
find_user(id: 123)

// Qualified calls (needs implementation)
Users.find_by_id(id: 123)
Http.Client.get(url: "...")

// Function capture (needs implementation)
let finder = &find_user
let processor = &Users.process_batch
```

**Expected Output**: Complete function call resolution system

## Phase 4.3: Advanced Function Features

### Step 4: Function Overloading with Guards
**Tasks**:
1. **Guard-based overloading**: Support multiple functions with different guard expressions
   - Multiple function definitions with same name
   - Different guard conditions
   - Resolution based on guard evaluation

2. **Resolution algorithm**: Choose correct function implementation based on guard evaluation
   - Guard condition analysis
   - Function selection logic
   - Ambiguity detection and reporting

3. **Exhaustiveness checking**: Similar to case statements, ensure guard combinations are complete
   - Guard coverage analysis
   - Missing case detection
   - Unreachable function detection

**Example Function Overloading**:
```outrun
def divide(a: Integer64, b: Integer64): Float64 when b != 0 {
    Float64.from_integer(a) / Float64.from_integer(b)
}

def divide(a: Integer64, b: Integer64): None when b == 0 {
    // Handle division by zero case
}
```

**Expected Output**: Function overloading support with guard-based dispatch

## Phase 4.4: Testing and Integration

### Step 5: Comprehensive Test Suite
**Location**: `outrun-typechecker/src/tests/test_function_definitions.rs` (new file)

**Test Categories** (20 tests total):

#### Function Definition Validation (8 tests):
1. **Parameter type validation**:
   - Valid parameter types
   - Invalid/missing parameter types
   - Generic types in parameters (`List<String>`)

2. **Return type validation**:
   - Explicit return types (mandatory)
   - Missing return type annotations (error cases)
   - Generic types in return values (`Option<User>`)

3. **Guard function validation**:
   - Functions ending with `?` must return `Boolean`
   - Invalid guard return types
   - Guard function side-effect validation

4. **Parameter name uniqueness**:
   - Duplicate parameter names
   - Valid unique parameter names

#### Function Call Validation (8 tests):
1. **Basic function calls** (already working):
   - Named parameter validation
   - Parameter type checking
   - Return type propagation

2. **Qualified function calls**:
   - `Module.function()` resolution
   - Invalid module/function combinations
   - Access control validation

3. **Function capture syntax**:
   - `&function_name` validation
   - Function type generation
   - Scope validation

4. **Parameter type checking**:
   - Complex generic type arguments
   - Type compatibility validation

#### Advanced Features (4 tests):
1. **Function overloading with guards**:
   - Multiple function definitions
   - Guard-based resolution
   - Exhaustiveness checking

2. **Error cases and recovery**:
   - Invalid function definitions
   - Function call resolution failures
   - Guard validation errors

**Expected Output**: 20+ comprehensive tests with 100% pass rate

### Step 6: Error Message Enhancement
**Tasks**:
1. **Function definition errors**:
   - Clear messages for parameter validation failures
   - Helpful guidance for return type issues
   - Specific guard function error messages

2. **Function call errors**:
   - Clear qualified call resolution failures
   - Function capture validation messages
   - Parameter mismatch explanations

3. **Guard system errors**:
   - Guard return type validation messages
   - Side-effect detection explanations
   - Overloading ambiguity guidance

**New Error Types Needed**:
```rust
TypeError::MissingReturnType {
    span: SourceSpan,
    function_name: String,
}

TypeError::InvalidGuardReturnType {
    span: SourceSpan, 
    function_name: String,
    found_type: String,  // Should be "Boolean"
}

TypeError::DuplicateParameterName {
    span: SourceSpan,
    parameter_name: String,
    function_name: String,
}

TypeError::GuardSideEffect {
    span: SourceSpan,
    function_name: String,
    side_effect_description: String,
}
```

**Expected Output**: Enhanced error reporting for function system

## Implementation Timeline

### Week 1: Core Function Validation
- Complete `check_function_definition()` method in functions.rs
- Implement parameter and return type validation
- Handle generic types in function signatures
- Basic test coverage (function definition tests)

### Week 2: Function Call Enhancements
- Implement qualified function calls (`Module.function()`)
- Add function capture syntax (`&function_name`)
- Enhanced function call validation
- Function call test coverage

### Week 3: Advanced Features & Polish
- Function overloading with guards
- Guard-based function resolution
- Exhaustiveness checking for function guards
- Advanced feature tests

### Week 4: Integration & Polish
- Comprehensive error message refinement
- Integration testing with existing system
- Performance validation
- Documentation updates

## Success Criteria

### Phase 4.1 Success
✅ **Function definitions validated**: All parameter types, return types, guard expressions
✅ **Parameter validation complete**: Type checking, name uniqueness, generic type support
✅ **Return type system working**: Mandatory explicit types, no defaults allowed
✅ **Guard system functional**: Boolean return validation, side-effect checking

### Phase 4.2 Success  
✅ **Enhanced function calls working**: Basic, qualified (`Module.function()`), and captured (`&function`)
✅ **Qualified call resolution**: Module/namespace function lookup
✅ **Function capture support**: `&function_name` syntax with proper typing
✅ **Generic type propagation**: Complex return types handled correctly

### Phase 4.3 Success
✅ **Function overloading working**: Guard-based function resolution
✅ **Exhaustiveness checking**: Complete guard coverage validation
✅ **Resolution algorithm**: Correct function selection based on guards

### Phase 4.4 Success
✅ **Comprehensive test coverage**: 20+ tests covering all function system aspects
✅ **Beautiful error reporting**: Clear function-related error messages with helpful guidance
✅ **Integration complete**: Function system works seamlessly with existing type checker

## Risk Assessment

### Low Risk
- **Basic function validation**: Core expression validation is well-understood and existing infrastructure is solid
- **Function call enhancement**: Basic calls already work, extensions are incremental
- **Test infrastructure**: Strong foundation from existing type checker tests

### Medium Risk
- **Guard system complexity**: Side-effect validation and exhaustiveness checking adds complexity
- **Function overloading**: Resolution algorithm needs careful design to avoid ambiguity
- **Integration testing**: Ensuring all components work together smoothly

### High Risk
- **Performance impact**: Function resolution and validation must remain fast
- **Error message quality**: Users need clear, actionable error messages for function issues

## Dependencies

### Internal Dependencies
- ✅ **Type system foundation**: Complete and working
- ✅ **Trait system**: Complete with dispatch tables  
- ✅ **Expression type checking**: Solid foundation for function body validation
- ✅ **Scope management**: Required for function parameter and variable handling
- ✅ **Error reporting infrastructure**: miette integration for beautiful errors

### No External Dependencies
- ❌ **Phase 3.4 generic system**: Not needed since functions don't declare generic parameters
- ❌ **Parser enhancements**: Current parser supports all needed function syntax
- ❌ **Additional crates**: All required functionality exists in current dependencies

## Future Extensions (Post-Phase 4)

### Type Inference (Phase 5+)
- Reduce need for explicit type annotations in function calls
- Infer generic type arguments where possible
- Maintain trait dispatch efficiency

### Advanced Function Features (Phase 6+)
- Async function support (if needed for actor model)
- Function pipeline optimization
- Advanced function composition features

### Performance Optimization (Phase 7+)
- Function call resolution caching
- Guard evaluation optimization
- Parallel function validation for large codebases

---

## Current Status

**Implementation Status**: Ready to begin Phase 4.1
**Blocking Issues**: None - all dependencies are complete
**Next Steps**: Start implementing `check_function_definition()` method in functions.rs

This plan provides a comprehensive roadmap for completing Outrun's function system while building on the solid foundation established in Phases 1-3.