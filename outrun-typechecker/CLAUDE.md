# Working with the Outrun Type Checker

## Project Overview

The Outrun type checker is a mathematically sound static type analysis system for the Outrun programming language. It uses SMT (Satisfiability Modulo Theories) constraint solving with Z3 to validate trait constraints, function signatures, and expressions at compile time, generating efficient dispatch tables for the interpreter.

**Current Status**: ✅ **PRODUCTION READY** - SMT-first type checking, trait system, Self type inference, and package composition support

## 🧮 Mathematical Soundness: Core Principle

**⚠️ CRITICAL**: This type checker is **mathematically sound** - we never use fallbacks, heuristics, or "best guesses". Every type decision must be **proven correct** by SMT constraint solving or fail with a proper error.

### ❌ **NEVER DO THIS**
```rust
// ❌ WRONG: Fallback logic introduces non-determinism
match smt_solver.solve() {
    Ok(solution) => use_solution(solution),
    Err(_) => fallback_to_heuristics(), // 🚨 NEVER!
}

// ❌ WRONG: "Confidence-based" type selection
let best_type = types.iter().max_by_key(|t| confidence_score(t));

// ❌ WRONG: Guessing when constraints are unsatisfiable  
if constraints_unsatisfiable() {
    pick_most_likely_type() // 🚨 NEVER!
}
```

### ✅ **ALWAYS DO THIS**
```rust
// ✅ CORRECT: Either SMT proves a solution exists, or we fail properly
match smt_solver.solve() {
    SolverResult::Satisfiable(model) => extract_proven_solution(model),
    SolverResult::Unsatisfiable(conflicts) => Err(TypeError::ConflictingConstraints(conflicts)),
    SolverResult::Unknown(reason) => Err(TypeError::SolverLimitation(reason)),
}
```

**Rationale**: Type checking is a mathematical proof system. If we can't prove a program is type-safe, we must reject it. Fallbacks undermine correctness and create subtle bugs.

## 🔬 SMT-First Architecture

### Core SMT Pipeline

1. **Constraint Generation**: Convert Outrun type relationships to SMT constraints
2. **Z3 Solving**: Use Z3 SMT solver to find satisfying assignments  
3. **Model Extraction**: Extract concrete type assignments from proven models
4. **Dispatch Generation**: Use proven types for runtime dispatch tables

```rust
// Phase 6: SMT constraint collection during type checking
let constraints = collect_smt_constraints(&programs);

// Phase 7: SMT constraint solving with Z3
let model = z3_solver.solve(constraints)?; // Must succeed or fail properly

// Phase 8: Extract proven type assignments
let type_assignments = extract_model(model); // Mathematically guaranteed

// Phase 9: Generate dispatch tables from proven types
let dispatch_tables = generate_dispatch(type_assignments);
```

### SMT Constraint Types

The system generates these constraint types (all handled by Z3):

```rust
pub enum SMTConstraint {
    /// Type A must implement trait B
    TraitImplemented {
        impl_type: StructuredType,
        trait_type: StructuredType,
    },
    
    /// Universal Self constraint for trait definitions
    /// ∀ Self. (implements(Self, TraitBeingDefined) ∧ implements(Self, BoundTrait))
    UniversalSelfConstraint {
        self_variable_id: TypeNameId,
        trait_being_defined: StructuredType,
        bound_traits: Vec<StructuredType>,
    },
    
    /// Concrete Self binding for trait implementations
    /// Self = ConcreteType
    ConcreteSelfBinding {
        self_variable_id: TypeNameId,
        concrete_type: StructuredType,
    },
    
    /// Self type inference from function call arguments
    /// Generated for calls like Option.some?(value: Option<Integer>) → Self = Option<Integer>
    SelfTypeInference {
        self_variable_id: TypeNameId,
        inferred_type: StructuredType,
        call_site_context: String,
        confidence: InferenceConfidence,
    },
    
    /// Type parameter unification T = ConcreteType
    TypeParameterUnification {
        parameter_name: String,
        concrete_type: StructuredType,
    },
    
    // ... other constraint types
}
```

## 🎯 Self Type Resolution: The Core Problem Solved

### Problem Statement

In calls like `Option.some?(value: return_value)` where `return_value` has type `Option<Integer>`, we need to infer that `Self = Option<Integer>` for trait dispatch to work correctly.

### SMT-Based Solution

**Old Approach (Broken)**:
- Premature Self substitution in compilation phase
- Direct unification attempts 
- Fallback to "best guess" when unification failed

**New Approach (Mathematically Sound)**:
1. **Keep Self as type variable** throughout compilation
2. **Generate SelfTypeInference constraints** from function call arguments
3. **Let Z3 solve** for the Self type using all available constraints
4. **Extract proven Self type** from SMT model
5. **Use proven Self type** for dispatch resolution

```rust
// Example: Option.some?(value: Option<Integer>)
// Generates these SMT constraints:

// 1. Self type inference from argument
SelfTypeInference {
    self_variable_id: Self_call_123,
    inferred_type: Option<Integer>,
    call_site_context: "parameter value in call to some?",
    confidence: High,
}

// 2. Self must implement the trait
TraitImplemented {
    impl_type: TypeVariable(Self_call_123),
    trait_type: Option,
}

// 3. Z3 solves: Self_call_123 = Option<Integer> ✓ (proven!)
```

### Self Semantics by Context

The system correctly handles different Self semantics:

**Trait Definitions**: `Self` means "any type implementing this trait"
```rust
trait Binary when Self: Equality {
    def add(left: Self, right: Self): Self
    //      ^^^^^        ^^^^^       ^^^^
    //      Universally quantified Self variables
}
```

**Trait Implementations**: `Self` means "the specific implementing type"  
```rust
impl Binary for Integer {
    def add(left: Self, right: Self): Self {
    //      ^^^^^        ^^^^^       ^^^^
    //      All resolve to Integer (proven by SMT)
    }
}
```

## 🏗️ Compilation Pipeline

### Phase Overview

```rust
impl CompilerEnvironment {
    pub fn compile_programs(&mut self, programs: Vec<Program>) -> Result<CompilationResult, Vec<TypeError>> {
        // Phases 1-5: AST processing (unchanged)
        self.phase_1_desugaring(&programs)?;
        self.phase_2_trait_extraction(&programs)?;
        self.phase_3_struct_extraction(&programs)?;
        self.phase_4_impl_extraction(&programs)?;
        self.phase_5_function_extraction(&programs)?;
        
        // 🔥 REMOVED: Phase 5.6 - substitute_self_types_in_impl_blocks()
        // This was premature and prevented SMT from seeing Self types!
        
        // Phase 6: SMT constraint collection during type checking
        self.smt_type_check_all(&programs)?;
        
        // Phase 7: SMT constraint solving with Z3
        self.phase_7_smt_constraint_solving()?;
        
        // Phase 8: SMT-guided dispatch table generation  
        self.calculate_dispatch_tables_with_smt(&programs)?;
        
        // Phase 9: Typed AST building
        self.build_typed_ast(&programs)
    }
}
```

### Key Architecture Changes

**✅ What We Added**:
- SMT constraint generation in type checking
- Z3-based constraint solving phase
- Mathematically proven Self type resolution
- SMT-guided dispatch table generation

**❌ What We Removed**:  
- `substitute_self_types_in_impl_blocks()` - premature substitution
- Unification fallback logic - non-mathematical
- "Confidence-based" type selection - unsound

## 🔧 Function Call Type Checking

### SMT-Based Self Inference

```rust
fn infer_implementing_type_with_smt(
    &mut self,
    trait_type_id: TypeNameId,
    trait_func_def: &FunctionDefinition,
    call: &FunctionCall,
) -> Result<StructuredType, TypeError> {
    // 1. Create unique Self type variable for this call
    let self_type_id = self.intern_type_name(&format!("Self_call_{}", call.span.start));
    
    // 2. Generate SelfTypeInference constraints from arguments
    for param in &trait_func_def.parameters {
        if self.is_self_type_annotation(&param.type_annotation) {
            let constraint = SMTConstraint::SelfTypeInference {
                self_variable_id: self_type_id.clone(),
                inferred_type: arg_type.clone(), // From function call argument
                call_site_context: format!("parameter {} in call to {}", param_name, trait_func_def.name.name),
                confidence: InferenceConfidence::High,
            };
            self.add_smt_constraint(constraint);
        }
    }
    
    // 3. Add constraint: Self must implement trait
    let trait_constraint = SMTConstraint::TraitImplemented {
        impl_type: StructuredType::TypeVariable(self_type_id.clone()),
        trait_type: StructuredType::Simple(trait_type_id),
    };
    self.add_smt_constraint(trait_constraint);
    
    // 4. Use Z3 to solve for Self type (NO FALLBACKS!)
    match self.smt_resolve_self_type(&self_type_id) {
        Ok(proven_type) => Ok(proven_type), // ✅ Mathematically proven
        Err(smt_error) => Err(TypeError::SelfResolutionFailed(smt_error)), // ❌ Proper error
    }
}
```

### Self Type Resolution

```rust
pub fn smt_resolve_self_type(&self, self_type_id: &TypeNameId) -> Result<StructuredType, SMTError> {
    // Create Z3 solver and add all constraints
    let mut solver = Z3ConstraintSolver::new();
    solver.add_constraints(&self.unification_context().smt_constraints, self)?;
    
    // Solve constraints (mathematically sound!)
    match solver.solve() {
        SolverResult::Satisfiable(model) => {
            // Extract proven Self type from model
            let self_var_name = format!("Self_{}", self_type_id.hash);
            if let Some(proven_type) = self.extract_self_type_from_model(&model, &self_var_name) {
                Ok(proven_type) // ✅ Z3 mathematically proved this assignment
            } else {
                Err(SMTError::SolverError("Model missing Self variable".to_string()))
            }
        }
        SolverResult::Unsatisfiable(conflicts) => {
            // Constraints are mathematically contradictory
            Err(SMTError::SolvingFailed(format!("Conflicting Self constraints: {:?}", conflicts)))
        }
        SolverResult::Unknown(reason) => {
            // Z3 cannot determine satisfiability (solver limitation)
            Err(SMTError::SolvingFailed(format!("SMT solver limitation: {}", reason)))
        }
    }
    // 🚨 NO FALLBACKS! Either proven or failed.
}
```

## 📊 Type System Components

### StructuredType System

All types preserve structure throughout compilation:

```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StructuredType {
    Simple(TypeNameId),                    // Boolean, String
    Generic { base: TypeNameId, args: Vec<StructuredType> }, // Option<Integer>
    Tuple(Vec<StructuredType>),           // (String, Integer)
    Function { params: Vec<FunctionParam>, return_type: Box<StructuredType> },
    TypeVariable(TypeNameId),             // Self, T, K, V
}
```

**Key Principle**: Never flatten to strings during type checking - only for error reporting.

### SMT Integration Points

1. **Type Checking**: Generate constraints during AST traversal
2. **Constraint Solving**: Use Z3 to find satisfying assignments
3. **Model Extraction**: Convert Z3 models back to StructuredTypes
4. **Dispatch Resolution**: Use proven types for runtime dispatch

## 🧪 Testing Strategy

### Mathematical Soundness Tests

```rust
#[test]
fn test_no_fallbacks_in_self_resolution() {
    // Ensure unsatisfiable constraints properly fail
    let code = r#"
        trait Foo when Self: Bar && Self: Baz {
            def test(value: Self): Self
        }
        
        def broken(): ??? {
            Foo.test(value: incompatible_type) // Should fail, not fallback
        }
    "#;
    
    let result = compile_and_check(code);
    match result {
        Err(TypeError::SelfResolutionFailed(_)) => {}, // ✅ Proper mathematical failure
        Ok(_) => panic!("Should have failed - constraints are unsatisfiable"),
        Err(other) => panic!("Wrong error type - should be SelfResolutionFailed, got {:?}", other),
    }
}

#[test] 
fn test_self_inference_from_arguments() {
    let code = r#"
        def test_function(): Boolean {
            Option.some?(value: String.index_of(value: "hello", search: "world"))
        }
    "#;
    
    let result = compile_and_check(code);
    assert!(result.is_ok(), "SMT should prove Self = Option<Integer> from arguments");
    
    // Verify the SMT system generated correct constraints
    let compilation = result.unwrap();
    let constraints = compilation.smt_constraints();
    assert!(constraints.iter().any(|c| matches!(c, SMTConstraint::SelfTypeInference { .. })));
}
```

### Integration Test Coverage

- **✅ SMT constraint generation**: All constraint types properly generated
- **✅ Z3 integration**: Real SMT solving with satisfiable/unsatisfiable cases  
- **✅ Self type inference**: Function calls correctly infer Self types
- **✅ No fallback paths**: All failure cases properly handled without guessing
- **✅ Trait constraints**: Universal/concrete Self semantics work correctly

## 🚨 Development Guidelines

### Mathematical Soundness Rules

1. **Never implement fallback logic** - if SMT can't prove it, reject it
2. **Always handle all SolverResult cases** - Satisfiable, Unsatisfiable, Unknown
3. **Extract types from models** - don't guess based on constraints
4. **Fail fast on constraint conflicts** - unsatisfiable = type error
5. **No confidence scoring** - either proven or not proven

### SMT Constraint Best Practices

```rust
// ✅ GOOD: Generate precise constraints
let constraint = SMTConstraint::SelfTypeInference {
    self_variable_id: call_specific_id,  // Unique per call site
    inferred_type: concrete_arg_type,    // From actual argument
    call_site_context: specific_context, // For debugging
    confidence: High,                    // Based on constraint precision
};

// ❌ BAD: Vague or overly broad constraints  
let constraint = SMTConstraint::SelfTypeInference {
    self_variable_id: global_self_id,    // Conflicts with other calls
    inferred_type: any_type,             // Too permissive
    call_site_context: "somewhere",     // Useless for debugging
    confidence: guess_confidence(),      // Non-mathematical
};
```

### Error Handling

```rust
// ✅ GOOD: Proper mathematical failure
match smt_solver.solve() {
    SolverResult::Unsatisfiable(conflicts) => {
        Err(TypeError::ConflictingConstraints {
            constraints: conflicts,
            explanation: "These type requirements are mathematically contradictory",
            suggestions: generate_conflict_resolution_suggestions(conflicts),
        })
    }
}

// ❌ BAD: Hiding mathematical impossibility
match smt_solver.solve() {
    SolverResult::Unsatisfiable(_) => {
        eprintln!("Constraints unsatisfiable, trying fallback...");
        try_heuristic_approach() // 🚨 NEVER!
    }
}
```

## 🔧 Useful Commands

```bash
# Run SMT integration tests
cargo test test_smt_integration

# Run type checker with SMT debugging
RUST_LOG=debug cargo test -- --nocapture

# Check mathematical soundness (no fallbacks)
rg -n "fallback|heuristic|guess|confidence" src/ # Should find minimal results

# Verify Z3 integration  
cargo test test_z3_constraint_solving

# Run all type checker tests
cargo test --package outrun-typechecker

# Check for SMT constraint coverage
cargo test test_constraint_generation
```

## 🚀 Future Enhancements

### Advanced SMT Features

- **Quantifier instantiation**: Better handling of universal constraints
- **Unsat core analysis**: Precise conflict identification for error messages
- **SMT optimization**: Minimal satisfying assignments for better performance
- **Incremental solving**: Reuse solver state across compilation units

### Performance Optimizations

- **Constraint caching**: Memoize common constraint patterns
- **Parallel solving**: Independent constraint sets solved concurrently  
- **Smart constraint ordering**: Reduce SMT solver search space
- **Model minimization**: Extract simplest satisfying type assignments

## 📚 Key Principles Summary

1. **Mathematical Soundness**: Every type decision must be proven by SMT or fail properly
2. **No Fallbacks**: Unsatisfiable constraints indicate real type errors, not fallback opportunities  
3. **SMT-First**: Generate constraints, solve with Z3, extract proven solutions
4. **Structured Types**: Preserve type structure throughout compilation pipeline
5. **Self as Variable**: Keep Self as type variable until SMT resolves it to concrete types
6. **Proper Error Handling**: Clear distinction between mathematical impossibility and solver limitations

This type checker provides a mathematically rigorous foundation for Outrun's type system, ensuring that all type decisions are provably correct while maintaining excellent error reporting and performance.