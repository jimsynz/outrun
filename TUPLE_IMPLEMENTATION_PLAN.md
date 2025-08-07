# Tuple Type Implementation Plan

**Created**: 7 August 2025  
**Updated**: 7 August 2025 (Structural Type Approach)  
**Status**: Planning Phase  
**Priority**: High  
**Estimated Effort**: 2-3 days focused development  

## Problem Statement

**Selected TODO**: `// TODO: Implement tuple types in the typechecker` from `outrun-typechecker/src/inference.rs:6093`

Tuples are currently parsed and interpreted correctly, but the typechecker treats them as generic `"Outrun.Core.Tuple"` placeholders without proper type inference, validation, or pattern matching support.

## Design Decision: Structural Tuple Types

**IMPORTANT**: After researching how other languages handle tuple types (Rust, TypeScript, Haskell), we've decided to implement **structural tuple types** rather than parameterized concrete types.

### Why Structural Types?
- **No Arbitrary Limits**: Can handle any tuple arity the parser supports
- **Proper Type Identity**: `(Int, String)` ≠ `(Int, String, Bool)` 
- **Natural Syntax**: `(T1, T2, T3)` instead of `Tuple<T1, T2, T3>`
- **Better Error Messages**: "Expected (Int, String), got (Int, Bool)" 
- **Modern Language Design**: How Rust, TypeScript, and most modern languages work

### Rejected Approach: `Outrun.Core.Tuple<T1, T2, ...>`
This approach was rejected because:
- Creates infinite concrete types (`Tuple<T>`, `Tuple<T,U>`, `Tuple<T,U,V>`, etc.)
- Requires arbitrary arity limits or complex variadic generics
- Doesn't match how tuples work conceptually (they're structural, not container types)
- Makes error messages verbose and confusing

## Current State Assessment

### ✅ What's Already Working
- **Parser Support**: Full tuple parsing (`(1, 2, 3)`, `(String, Integer)`)
- **AST Representation**: Complete `TuplePattern`, `TupleLiteral` structures
- **Interpreter Support**: Full tuple evaluation and display
- **Basic Type Recognition**: Tuples treated as `"Outrun.Core.Tuple"` placeholder

### ❌ Critical Gaps
1. **No Structural Type Representation**: Tuples need their own `Type::Tuple` variant
2. **Missing Type Inference**: Element types not properly inferred or validated
3. **No Pattern Matching Integration**: Tuple destructuring patterns not type-checked
4. **No Structural Typing**: Can't validate tuple arity or element type compatibility
5. **Registry Confusion**: `"Outrun.Core.Tuple"` placeholder needs to be removed

## Implementation Phases

### Phase 1: Implement Structural Tuple Types
**Files**: `src/types.rs`, `src/registry.rs`  
**Estimated Time**: 4-6 hours  
**Status**: Pending  

**Changes Needed:**

1. **Add Tuple Variant to Type Enum**:
   ```rust
   pub enum Type {
       // ... existing variants
       Tuple {
           element_types: Vec<Type>,
           span: Option<Span>,
       },
   }
   ```

2. **Implement Structural Type Display**:
   ```rust
   impl fmt::Display for Type {
       fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
           match self {
               Type::Tuple { element_types, .. } => {
                   write!(f, "(")?;
                   for (i, elem_type) in element_types.iter().enumerate() {
                       if i > 0 { write!(f, ", ")?; }
                       write!(f, "{}", elem_type)?;
                   }
                   write!(f, ")")
               }
               // ... other variants
           }
       }
   }
   ```

3. **Remove Registry Placeholder**:
   ```rust
   // REMOVE from get_core_types():
   // ("Outrun.Core.Tuple", 0),  // DELETE - tuples are structural, not concrete types
   ```

4. **Implement Structural Equality**:
   - Two tuples are equal if they have the same arity and element types
   - `(Int, String)` ≠ `(String, Int)` (order matters)
   - `(Int, String)` ≠ `(Int, String, Bool)` (arity matters)

**Success Criteria**: 
- Tuple types display as `(Integer64, String)` not `Tuple<Integer64, String>`
- Structural equality works correctly
- No `Outrun.Core.Tuple` references remain

### Phase 2: Implement Tuple Type Inference in Expressions
**Files**: `src/inference.rs`  
**Estimated Time**: 6-8 hours  
**Status**: Pending  

**Changes Needed:**

1. **Fix `resolve_type_annotation()` for Tuples** (Line 6093):
   ```rust
   TypeAnnotation::Tuple { types, .. } => {
       let mut element_types = Vec::new();
       for type_annotation in types {
           element_types.push(self.resolve_type_annotation(type_annotation)?);
       }
       Ok(Type::Tuple { element_types, span: None })
   }
   ```

2. **Update Tuple Expression Inference** (Line 3349):
   ```rust
   ExpressionKind::Tuple(_) => {
       // Get inferred element types from dependency results
       let element_types = self.get_dependency_results(dependency_count);
       Ok(InferenceResult {
           inferred_type: Type::Tuple { element_types, span: None },
           constraints: Vec::new(),
       })
   }
   ```

3. **Handle Tuple Literals in Work Stack** (Line 2838):
   - Properly queue tuple element inference
   - Collect element types for tuple construction

**Success Criteria**: `(1, "hello")` correctly infers as `(Integer64, String)`

### Phase 3: Add Tuple Pattern Matching Type Checking
**Files**: `src/inference.rs`, `src/pattern.rs`  
**Estimated Time**: 4-6 hours  
**Status**: Pending  

**Changes Needed:**

1. **Enhance `infer_pattern_bindings()` for Tuples** (Line 2696):
   ```rust
   outrun_parser::Pattern::Tuple(tuple_pattern) => {
       if let Type::Tuple { element_types, .. } = scrutinee_type {
           // Validate arity matches
           if element_types.len() != tuple_pattern.elements.len() {
               return Err(/* arity mismatch error */);
           }
           // Type check each element pattern
           for (element_pattern, element_type) in 
               tuple_pattern.elements.iter().zip(element_types.iter()) {
               self.infer_pattern_bindings(element_pattern, element_type, bindings)?;
           }
       }
   }
   ```

2. **Add Tuple Destructuring Validation**:
   - Check tuple arity in let bindings: `let (x, y) = tuple_value`
   - Validate element type compatibility
   - Handle nested tuple patterns: `let ((a, b), c) = nested_tuple`

**Success Criteria**: `let (x, y) = tuple` properly type checks and binds variables

### Phase 4: Integrate Tuple Types with Constraints System
**Files**: `src/constraints.rs`, `src/unification.rs`  
**Estimated Time**: 4-6 hours  
**Status**: Pending  

**Changes Needed:**

1. **Add Tuple Unification Rules**:
   ```rust
   (Type::Tuple { element_types: left_elements, .. }, 
    Type::Tuple { element_types: right_elements, .. }) => {
       // Unify element-wise if same arity
       if left_elements.len() == right_elements.len() {
           for (left_elem, right_elem) in left_elements.iter().zip(right_elements.iter()) {
               self.unify(left_elem, right_elem)?;
           }
       }
   }
   ```

2. **Update Constraint Generation**:
   - Generate constraints for tuple element types
   - Handle tuple type variables in generic contexts
   - Support tuple subtyping rules

**Success Criteria**: Tuple types integrate seamlessly with constraint solving

### Phase 5: Add Comprehensive Tuple Type Tests
**Files**: `src/tests/test_tuple_types.rs` (new)  
**Estimated Time**: 2-4 hours  
**Status**: Pending  

**Test Coverage:**

1. **Basic Tuple Types**:
   - `(1, 2)` → `(Integer64, Integer64)`
   - `(1, "hello")` → `(Integer64, String)`
   - `()` → `()` (empty tuple)

2. **Nested Tuples**:
   - `((1, 2), 3)` → `((Integer64, Integer64), Integer64)`

3. **Structural Type Equality**:
   - `(Int, String)` ≠ `(String, Int)` (order matters)
   - `(Int, String)` ≠ `(Int, String, Bool)` (arity matters)
   - `(Int, String)` = `(Int, String)` (structural equality)

3. **Pattern Matching**:
   - `let (x, y) = (1, 2)` - successful destructuring
   - `let (x, y, z) = (1, 2)` - arity mismatch error

4. **Type Annotations**:
   - `let pair: (Integer64, String) = (1, "hello")`
   - Type annotation validation and inference

**Success Criteria**: Comprehensive test coverage ensures robustness

## Risk Assessment

### Low Risk Factors ✅
- **Existing Infrastructure**: Parser and interpreter already handle tuples
- **Isolated Changes**: Mainly extends existing type system patterns
- **Incremental Implementation**: Can be done phase by phase
- **Extensive Test Coverage**: Can validate each phase independently

### Potential Challenges ⚠️
- **Generic Type Integration**: Ensuring tuple types work with existing generic system
- **Performance**: Tuple type checking shouldn't slow down inference significantly
- **Error Messages**: Need clear error messages for tuple arity mismatches

## Impact Assessment

**High Priority** because tuples are:
- **Fundamental Language Feature**: Core to functional programming
- **Blocking Other Features**: Pattern matching, destructuring, multiple return values
- **Already Partially Implemented**: Parser and interpreter ready, just typechecker missing
- **High User Expectation**: Users expect `(1, "hello")` to work properly

## Implementation Notes

### Key Files to Modify
- `outrun-typechecker/src/types.rs` - Add structural `Type::Tuple` variant and display
- `outrun-typechecker/src/inference.rs` - Fix tuple type inference (Line 6093, 3349, 2838, 2696)
- `outrun-typechecker/src/registry.rs` - Remove `"Outrun.Core.Tuple"` placeholder
- `outrun-typechecker/src/constraints.rs` - Add tuple constraint handling
- `outrun-typechecker/src/unification.rs` - Add structural tuple unification rules

### Testing Strategy
- Create dedicated `test_tuple_types.rs` file
- Test each phase incrementally
- Cover edge cases: empty tuples, nested tuples, arity mismatches
- Validate integration with existing type system

## Progress Tracking

- [ ] Phase 1: Extend Type system for tuple representation
- [ ] Phase 2: Implement tuple type inference in expressions  
- [ ] Phase 3: Add tuple pattern matching type checking
- [ ] Phase 4: Integrate tuple types with constraints system
- [ ] Phase 5: Add comprehensive tuple type tests

## Success Metrics

Upon completion, the following should work correctly:

```outrun
// Basic tuple creation and inference
let pair = (1, "hello")  // Infers: (Integer64, String)

// Type annotations
let typed_pair: (Integer64, String) = (42, "world")

// Pattern matching and destructuring
let (x, y) = pair  // x: Integer64, y: String

// Nested tuples
let nested = ((1, 2), "hello")  // ((Integer64, Integer64), String)

// Empty tuples
let unit = ()  // ()

// Structural type checking
let mismatch: (String, Integer64) = (1, "hello")  // ERROR: type mismatch
let arity_error: (Integer64, String, Boolean) = (1, "hello")  // ERROR: arity mismatch
```

## Language Design Rationale

### Structural vs Concrete Types

**Structural Approach (Chosen):**
```outrun
let pair: (Int, String) = (42, "hello")  // Natural, concise
```

**Rejected Concrete Approach:**
```outrun
let pair: Tuple<Int, String> = Tuple.create(42, "hello")  // Verbose, unnatural
```

### Comparison with Other Languages

| Language   | Tuple Syntax | Type Representation | Approach |
|------------|--------------|-------------------|----------|
| **Rust**   | `(i32, String)` | Structural | ✅ Our model |
| **TypeScript** | `[number, string]` | Structural | ✅ Similar |
| **Haskell** | `(Int, String)` | Structural | ✅ Similar |
| **C#**     | `Tuple<int, string>` | Generic concrete | ❌ Rejected |

### Benefits of Structural Approach

1. **Intuitive**: Matches mathematical tuple notation `(a, b, c)`
2. **Concise**: `(Int, String)` vs `Tuple<Int, String>`
3. **Scalable**: No arbitrary arity limits or complex variadic generics
4. **Standard**: Aligns with modern functional language design
5. **Error-friendly**: Clear structural mismatch messages

This implementation will eliminate a major language feature gap and unlock proper structural tuple support across the entire Outrun language stack, following modern functional programming language conventions.