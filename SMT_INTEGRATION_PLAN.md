# SMT Integration Plan for Outrun Typechecker

## Overview

This document outlines the comprehensive plan for integrating Z3 SMT solver into the Outrun typechecker to solve the trait dispatch problem where functions returning trait types (like `Option<T>`) need to be properly resolved to their concrete implementations.

## Key Design Principles

1. **Always use StructuredType**: Never use `TypeNameId` alone - always use `StructuredType` to preserve generic information
2. **No fallback strategies**: Direct integration into core typechecker paths (this is a prototype)
3. **Test-driven approach**: Use extensive existing test corpus to validate changes
4. **Trait union expansion**: `Option<Float>` expands to all combinations of concrete implementations

## Current Problem

When `String.index_of()` returns `Option<Integer>` and we call `Option.some?()` on the result, the typechecker fails because:
- `Option<Integer>` is a trait type, not a concrete type
- `Option.some?()` is only implemented on concrete types like `Outrun.Option.Some<T>` and `Outrun.Option.None<T>`
- We need SMT solving to expand trait types to their concrete implementors

## Phase 1: Foundation & Dependencies

### 1.1 Add Z3 Dependency
```toml
[dependencies]
# Add to existing dependencies in outrun-typechecker/Cargo.toml
z3 = "0.12"  # SMT solver for advanced constraint solving
```

### 1.2 Create SMT Module Structure
```
src/smt/
├── mod.rs                    # Module exports and public API
├── constraints.rs            # SMT constraint definitions
├── translator.rs             # Outrun → SMT-LIB translation
├── solver.rs                 # Z3 interface and solving logic
├── cache.rs                  # Constraint solving result cache
└── suggestions.rs            # Error suggestions from SMT
```

## Phase 2: Constraint System Design

### 2.1 Define SMT Constraint Types
```rust
// src/smt/constraints.rs
#[derive(Debug, Clone, PartialEq)]
pub enum SMTConstraint {
    /// Type A must implement trait B - ALWAYS use StructuredType
    TraitImplemented {
        impl_type: StructuredType,        // ✅ Preserves generic info
        trait_type: StructuredType,       // ✅ Preserves generic info
    },
    
    /// Type A must unify with type B
    TypeUnification {
        type1: StructuredType,
        type2: StructuredType,
        context: String, // For error reporting
    },
    
    /// Generic type must be instantiated with specific concrete types
    GenericInstantiation {
        generic_type: StructuredType,     // ✅ e.g., Option<T>
        concrete_candidates: Vec<StructuredType>, // ✅ e.g., [Option<Int64>, Option<String>]
    },
    
    /// Function signature must match expected parameters/return type
    FunctionSignatureMatch {
        expected: FunctionSignature,      // Uses StructuredType internally
        actual: FunctionSignature,       // Uses StructuredType internally
        call_site: Span,
    },
    
    /// Guard condition must be satisfiable
    GuardCondition {
        condition: BooleanExpression,
        variables: HashMap<String, StructuredType>,
    },
}

#[derive(Debug, Clone)]
pub struct ConstraintSet {
    pub constraints: Vec<SMTConstraint>,
    pub context: ConstraintContext,
    pub priority: ConstraintPriority,
}

#[derive(Debug, Clone)]
pub enum ConstraintPriority {
    Critical,   // Must solve for compilation to succeed
    Important,  // Affects dispatch optimization
    Optional,   // For enhanced error messages
}

// Function signatures also use StructuredType
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub params: Vec<FunctionParam>,      // FunctionParam.param_type is StructuredType
    pub return_type: StructuredType,     // ✅ Not TypeNameId
}
```

### 2.2 Constraint Collection Infrastructure
```rust
// src/smt/collector.rs
pub struct ConstraintCollector {
    constraints: Vec<SMTConstraint>,
    scope_stack: Vec<ConstraintScope>,
    deferred_constraints: Vec<DeferredConstraint>,
}

impl ConstraintCollector {
    pub fn collect_trait_constraint(&mut self, trait_type: &StructuredType, impl_type: &StructuredType);
    pub fn collect_unification_constraint(&mut self, type1: &StructuredType, type2: &StructuredType);
    pub fn collect_function_constraint(&mut self, call: &FunctionCall, signature: &FunctionSignature);
    pub fn defer_complex_constraint(&mut self, constraint: SMTConstraint);
}
```

## Phase 3: SMT Translation Layer

### 3.1 SMT-LIB Translation
```rust
// src/smt/translator.rs
pub struct SMTTranslator {
    type_variables: HashMap<TypeNameId, String>,
    trait_predicates: HashMap<TypeNameId, String>,
    solver_context: Z3Context,
}

impl SMTTranslator {
    /// Convert StructuredType to SMT sort (handles generics properly)
    pub fn translate_structured_type(&mut self, structured_type: &StructuredType) -> Z3Sort {
        match structured_type {
            StructuredType::Simple(type_name_id) => {
                // Convert TypeNameId to string and create SMT sort
                let type_name = self.compiler_env.resolve_type(type_name_id).unwrap();
                self.context.mk_sort(&type_name)
            },
            StructuredType::Generic { base, args } => {
                // Create parameterized SMT sort: Option_Int64, Map_String_Int64, etc.
                let base_name = self.compiler_env.resolve_type(&base).unwrap();
                let arg_sorts: Vec<_> = args.iter()
                    .map(|arg| self.translate_structured_type(arg))
                    .collect();
                self.create_generic_sort(&base_name, arg_sorts)
            },
            StructuredType::Tuple(elements) => {
                let element_sorts: Vec<_> = elements.iter()
                    .map(|elem| self.translate_structured_type(elem))
                    .collect();
                self.context.mk_tuple_sort(&element_sorts)
            },
            // Handle other StructuredType variants...
        }
    }
    
    /// Generate trait implementation predicate: implements(Option<Int64>, Option<T>)
    pub fn create_trait_predicate(&mut self, impl_type: &StructuredType, trait_type: &StructuredType) -> Z3Ast {
        let impl_sort = self.translate_structured_type(impl_type);
        let trait_sort = self.translate_structured_type(trait_type);
        
        // Create SMT predicate: implements(impl_type, trait_type)
        self.implements_relation.apply(&[
            self.context.mk_const(&format!("impl_{}", self.type_counter), &impl_sort),
            self.context.mk_const(&format!("trait_{}", self.type_counter), &trait_sort)
        ])
    }
    
    /// Convert trait implementation constraint to SMT formula
    pub fn translate_trait_constraint(&mut self, constraint: &SMTConstraint) -> Z3Ast;
    
    /// Convert unification constraint to SMT equality
    pub fn translate_unification(&mut self, type1: &StructuredType, type2: &StructuredType) -> Z3Ast;
    
    /// Generate SMT declarations for all types in scope
    pub fn generate_type_declarations(&mut self, types: &[StructuredType]) -> String;
}
```

### 3.2 Z3 Interface
```rust
// src/smt/solver.rs
pub struct Z3ConstraintSolver {
    context: Z3Context,
    solver: Z3Solver,
    type_sort: Z3Sort,
    trait_sort: Z3Sort,
    implements_relation: Z3FuncDecl,
}

impl Z3ConstraintSolver {
    pub fn new() -> Self;
    pub fn add_constraints(&mut self, constraints: &[SMTConstraint]) -> Result<(), SMTError>;
    pub fn solve(&mut self) -> SolverResult;
    pub fn get_model(&self) -> Option<ConstraintModel>;
    pub fn get_unsat_core(&self) -> Vec<SMTConstraint>;
}

#[derive(Debug, Clone)]
pub enum SolverResult {
    Satisfiable(ConstraintModel),
    Unsatisfiable(Vec<SMTConstraint>), // Conflicting constraints
    Unknown(String), // Timeout or resource limits
}
```

## Phase 4: Integration with Existing Type System

### 4.1 Extend UnificationContext
```rust
// Modify src/unification.rs
pub struct UnificationContext {
    // Existing fields...
    pub smt_constraints: Vec<SMTConstraint>,
    pub constraint_collector: Option<ConstraintCollector>,
    pub solver_cache: LruCache<ConstraintSetHash, SolverResult>,
}

impl UnificationContext {
    pub fn add_smt_constraint(&mut self, constraint: SMTConstraint);
    pub fn solve_accumulated_constraints(&mut self) -> Result<ConstraintModel, SMTError>;
    pub fn has_pending_constraints(&self) -> bool;
}
```

### 4.2 Enhance StructuredType System
```rust
// Modify src/unification.rs
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StructuredType {
    // Existing variants...
    
    /// Type with pending SMT constraints
    ConstrainedType {
        base_type: Box<StructuredType>,
        constraints: Vec<SMTConstraint>,
        solver_variables: Vec<String>,
    },
    
    /// Union type representing multiple possible concrete implementations
    TraitUnion {
        trait_type: TypeNameId,
        possible_implementations: Vec<StructuredType>,
        selection_constraints: Vec<SMTConstraint>,
    },
}
```

## Phase 5: Modified Compilation Pipeline

### 5.1 Replace Existing Phases (No Fallback)
```rust
// src/compilation/compiler_environment.rs - Replace existing phases entirely
impl CompilerEnvironment {
    pub fn compile_programs(&mut self, programs: Vec<Program>) -> Result<CompilationResult, Vec<TypeError>> {
        // Phase 1-5: Existing extraction phases (unchanged)
        self.phase_1_desugaring(&programs)?;
        self.phase_2_trait_extraction(&programs)?;
        self.phase_3_struct_extraction(&programs)?;
        self.phase_4_impl_extraction(&programs)?;
        self.phase_5_function_extraction(&programs)?;
        
        // Phase 6: Type checking with SMT constraint collection
        self.phase_6_smt_type_checking(&programs)?;
        
        // Phase 7: SMT constraint solving  
        self.phase_7_smt_constraint_solving()?;
        
        // Phase 8: Dispatch table generation (using SMT results)
        self.phase_8_smt_dispatch_generation(&programs)?;
        
        // Phase 9: Typed AST building
        self.phase_9_typed_ast_building(&programs)
    }

    /// NEW: Phase 7 - SMT constraint solving
    pub fn phase_7_smt_constraint_solving(&mut self) -> Result<(), Vec<TypeError>> {
        println!("🔄 SOLVING SMT constraints");
        
        // 1. Extract all constraints from unification context
        let mut constraint_collector = ConstraintCollector::new();
        self.collect_all_constraints(&mut constraint_collector);
        
        // 2. Solve critical constraints
        let solver_result = self.solve_constraints(constraint_collector.get_critical_constraints())?;
        
        // 3. Apply solutions back to type system
        self.apply_constraint_solutions(solver_result);
        
        // 4. Solve optional constraints for better error messages
        let _ = self.solve_constraints(constraint_collector.get_optional_constraints());
        
        println!("✅ Successfully solved SMT constraints");
        Ok(())
    }
}
```

### 5.2 Modify Type Checking to Use SMT Directly
```rust
// src/compilation/type_checking.rs - Modified core function
impl TypeCheckingVisitor {
    fn check_function_call_type(&mut self, call: &FunctionCall) -> Result<StructuredType, TypeError> {
        // Step 1: Collect all possible function matches
        let possible_functions = self.compiler_environment.get_all_matching_functions(&call.path);
        
        if possible_functions.is_empty() {
            return Err(TypeError::UndefinedFunction { 
                name: call.path.to_string(), 
                span: call.span.to_source_span() 
            });
        }
        
        if possible_functions.len() == 1 {
            // Single match - traditional path
            return self.validate_single_function_match(&possible_functions[0], call);
        }
        
        // Multiple matches - use SMT to resolve
        self.resolve_function_call_with_smt(call, possible_functions)
    }
    
    fn resolve_function_call_with_smt(
        &mut self, 
        call: &FunctionCall, 
        candidates: Vec<UnifiedFunctionEntry>
    ) -> Result<StructuredType, TypeError> {
        // Create SMT constraints for each candidate
        let mut constraints = Vec::new();
        
        for candidate in &candidates {
            let constraint = self.create_function_match_constraint(call, candidate)?;
            constraints.push(constraint);
        }
        
        // Solve constraints with Z3
        let mut solver = Z3ConstraintSolver::new();
        solver.add_constraints(&constraints)?;
        
        match solver.solve() {
            SolverResult::Satisfiable(model) => {
                let selected_function = self.extract_function_from_model(&model, &candidates)?;
                self.validate_selected_function(call, &selected_function)
            },
            SolverResult::Unsatisfiable(conflicting) => {
                Err(TypeError::SMTConstraintUnsatisfiable {
                    conflicting_constraints: conflicting,
                    suggestions: self.generate_smt_suggestions(&constraints),
                    span: call.span.to_source_span(),
                })
            },
            SolverResult::Unknown(reason) => {
                Err(TypeError::Internal(format!("SMT solver failed: {}", reason)))
            }
        }
    }
}
```

## Phase 6: Enhanced Error Reporting

### 6.1 SMT-Based Error Suggestions
```rust
// src/smt/suggestions.rs
pub struct ErrorSuggestionGenerator {
    solver: Z3ConstraintSolver,
    type_registry: TypeRegistry,
}

impl ErrorSuggestionGenerator {
    pub fn suggest_trait_implementations(&mut self, error: &TypeError) -> Vec<String>;
    pub fn suggest_type_annotations(&mut self, error: &TypeError) -> Vec<String>;
    pub fn suggest_function_overloads(&mut self, error: &TypeError) -> Vec<String>;
    pub fn generate_constraint_relaxations(&mut self, constraints: &[SMTConstraint]) -> Vec<ConstraintRelaxation>;
}

#[derive(Debug, Clone)]
pub struct ConstraintRelaxation {
    pub description: String,
    pub relaxed_constraints: Vec<SMTConstraint>,
    pub confidence: f32,
}
```

### 6.2 Enhanced Error Types
```rust
// Extend src/error.rs
#[derive(Debug, Clone)]
pub enum TypeError {
    // Existing variants...
    
    SMTConstraintUnsatisfiable {
        conflicting_constraints: Vec<SMTConstraint>,
        suggestions: Vec<String>,
        span: SourceSpan,
    },
    
    AmbiguousTypeResolution {
        candidate_types: Vec<StructuredType>,
        smt_model: Option<ConstraintModel>,
        span: SourceSpan,
    },
}
```

## Phase 7: Performance Optimizations

### 7.1 Constraint Caching
```rust
// src/smt/cache.rs
pub struct ConstraintCache {
    solved_constraints: LruCache<ConstraintSetHash, SolverResult>,
    type_hierarchies: HashMap<TypeNameId, Vec<TypeNameId>>,
    implementation_cache: HashMap<(TypeNameId, TypeNameId), bool>,
}

impl ConstraintCache {
    pub fn get_cached_solution(&self, constraints: &[SMTConstraint]) -> Option<&SolverResult>;
    pub fn cache_solution(&mut self, constraints: Vec<SMTConstraint>, result: SolverResult);
    pub fn invalidate_type_cache(&mut self, type_id: TypeNameId);
}
```

### 7.2 Incremental Solving
```rust
// src/smt/incremental.rs
pub struct IncrementalSolver {
    solver: Z3ConstraintSolver,
    constraint_levels: Vec<ConstraintLevel>,
    backtrack_points: Vec<SolverState>,
}

impl IncrementalSolver {
    pub fn push_constraint_scope(&mut self);
    pub fn pop_constraint_scope(&mut self);
    pub fn add_constraints_at_level(&mut self, constraints: Vec<SMTConstraint>, level: u32);
    pub fn solve_incremental(&mut self) -> SolverResult;
}
```

## Phase 8: Trait Union Type Expansion

### 8.1 Trait Implementation Discovery
```rust
// src/smt/trait_expansion.rs
pub struct TraitImplementationExpander {
    compiler_env: CompilerEnvironment,
    solver: Z3ConstraintSolver,
}

impl TraitImplementationExpander {
    /// Find all concrete types that implement a given trait
    pub fn find_trait_implementations(&self, trait_type: &StructuredType) -> Vec<StructuredType>;
    
    /// Recursively expand nested generic trait types
    pub fn expand_nested_trait_generics(&self, trait_type: &StructuredType) -> Vec<StructuredType>;
    
    /// Create SMT constraints for trait union types
    pub fn create_union_constraints(&self, union_type: &StructuredType) -> Vec<SMTConstraint>;
}
```

### 8.2 Corrected StructuredType Handling in Core Functions
```rust
// src/compilation/compiler_environment.rs
impl CompilerEnvironment {
    /// Look up trait implementations using full StructuredType (not just TypeNameId)
    pub fn find_trait_implementations(&self, trait_type: &StructuredType) -> Vec<StructuredType> {
        match trait_type {
            StructuredType::Simple(trait_id) => {
                // Find all concrete types implementing this simple trait
                self.find_simple_trait_implementations(trait_id)
            },
            StructuredType::Generic { base, args } => {
                // Find implementations of Generic<T> by:
                // 1. Finding all implementations of Generic  
                // 2. For each T in args, recursively find implementations
                // 3. Create cartesian product of all combinations
                self.find_generic_trait_implementations(base, args)
            },
            _ => Vec::new(),
        }
    }
    
    /// Recursively expand trait types to concrete implementations
    fn find_generic_trait_implementations(
        &self, 
        base_trait: &TypeNameId, 
        args: &[StructuredType]
    ) -> Vec<StructuredType> {
        // Example: Option<Float> becomes:
        // 1. Find Float implementations: [Outrun.Core.Float32, Outrun.Core.Float64]
        // 2. Find Option implementations: [Outrun.Option.Some<T>, Outrun.Option.None<T>]  
        // 3. Create combinations: [Some<Float32>, None<Float32>, Some<Float64>, None<Float64>]
        
        let mut concrete_implementations = Vec::new();
        
        // Recursively expand each argument type
        let expanded_args: Vec<Vec<StructuredType>> = args.iter()
            .map(|arg_type| self.find_trait_implementations(arg_type))
            .collect();
        
        // Get base trait implementations
        let base_implementations = self.find_simple_trait_implementations(base_trait);
        
        // Create cartesian product
        for base_impl in base_implementations {
            for arg_combination in cartesian_product(&expanded_args) {
                if let StructuredType::Generic { base: impl_base, .. } = base_impl {
                    concrete_implementations.push(StructuredType::Generic {
                        base: impl_base,
                        args: arg_combination,
                    });
                }
            }
        }
        
        concrete_implementations
    }
}
```

### 8.3 Modified Function Return Type Handling
```rust
// Modify src/compilation/type_checking.rs
impl TypeCheckingVisitor {
    fn resolve_function_return_type(&mut self, func_def: &FunctionDefinition) -> StructuredType {
        let declared_return_type = &func_def.return_type;
        
        match self.resolve_type_annotation(declared_return_type) {
            Ok(StructuredType::Simple(trait_id)) if self.is_trait(&trait_id) => {
                // Convert trait return type to trait union
                let implementations = self.find_trait_implementations(&trait_id);
                StructuredType::TraitUnion {
                    trait_type: trait_id,
                    possible_implementations: implementations,
                    selection_constraints: self.create_selection_constraints(&trait_id),
                }
            },
            Ok(other_type) => other_type,
            Err(e) => {
                self.errors.push(e);
                StructuredType::Simple(self.error_type_id())
            }
        }
    }
}
```

## Phase 9: Dispatch Resolution Enhancement

### 9.1 SMT-Based Dispatch Resolution
```rust
// Modify src/compilation/compiler_environment.rs
impl CompilerEnvironment {
    pub fn lookup_impl_function_with_smt(
        &self,
        trait_type: &StructuredType,
        impl_type: &StructuredType,
        function_name: AtomId,
    ) -> Option<UnifiedFunctionEntry> {
        // Create SMT constraint for trait implementation requirement
        let constraint = SMTConstraint::TraitImplemented {
            impl_type: impl_type.clone(),
            trait_type: trait_type.clone(),
        };
        
        let mut solver = Z3ConstraintSolver::new();
        solver.add_constraints(&[constraint]).ok()?;
        
        match solver.solve() {
            SolverResult::Satisfiable(model) => {
                // Extract concrete implementation from model
                self.extract_implementation_from_model(&model, trait_type, function_name)
            },
            _ => None,
        }
    }
}
```

## Phase 10: Testing & Validation Strategy

### 10.1 Test-Driven Approach Using Existing Test Corpus
```rust
// Strategy: Implement SMT features incrementally while keeping tests passing

// Phase 1: Basic trait constraint solving
#[test] 
fn test_basic_option_dispatch() {
    // This test should pass with SMT: Option.some?(value: index_of(...))
    let code = r#"
        def test_function() {
            Option.some?(value: String.index_of(value: "hello", search: "world"))
        }
    "#;
    
    let result = compile_and_check(code);
    assert!(result.is_ok(), "Basic Option dispatch should work with SMT");
}

// Phase 2: Nested generic constraints  
#[test]
fn test_nested_generics() {
    let code = r#"
        def complex_function(): Map<String, Option<Integer>> {
            Map.empty()
        }
    "#;
    
    let result = compile_and_check(code);
    assert!(result.is_ok(), "Nested generics should resolve with SMT");
}

// Phase 3: Error suggestions
#[test] 
fn test_smt_error_suggestions() {
    let code = r#"
        def broken_function() {
            Option.some?(value: "not an option type")
        }
    "#;
    
    let result = compile_and_check(code);
    match result {
        Err(TypeError::SMTConstraintUnsatisfiable { suggestions, .. }) => {
            assert!(!suggestions.is_empty(), "SMT should provide suggestions");
        },
        _ => panic!("Expected SMT constraint error with suggestions"),
    }
}
```

### 10.2 Preserve Test Infrastructure
```rust
// src/tests/test_core_library_runtime.rs - Keep all existing tests working
// but enhance them to use SMT solving internally

// Existing test function signatures stay the same:
#[test]
fn test_core_library_stats() {
    let compilation = get_core_library_compilation();
    // ... existing assertions stay the same
    
    // But internally, this now uses SMT constraint solving
    // for trait dispatch resolution
}

// Add new SMT-specific validation:
fn validate_smt_constraint_solving(compilation: &CompilationResult) {
    // Ensure SMT constraints were collected and solved
    assert!(compilation.has_smt_constraints(), "Should have collected SMT constraints");
    assert!(compilation.smt_solutions_complete(), "Should have solved all critical constraints");
}
```

### 10.3 SMT-Specific Test Suite
```rust
// src/tests/test_smt_constraint_solving.rs
#[cfg(test)]
mod tests {
    #[test]
    fn test_trait_union_expansion() {
        // Test Option<Float> expands to all concrete implementations
    }
    
    #[test]
    fn test_nested_generic_constraints() {
        // Test Map<String, Option<Integer>> constraint solving
    }
    
    #[test]
    fn test_constraint_caching() {
        // Test performance of constraint cache
    }
    
    #[test]
    fn test_error_suggestion_generation() {
        // Test SMT-based error suggestions
    }
}
```

### 10.4 Performance Benchmarks
```rust
// benches/smt_performance.rs
fn benchmark_constraint_solving(c: &mut Criterion) {
    c.bench_function("simple_trait_constraints", |b| {
        b.iter(|| solve_simple_constraints())
    });
    
    c.bench_function("complex_nested_generics", |b| {
        b.iter(|| solve_complex_constraints())
    });
}
```

## Implementation Timeline

1. **Week 1-2**: Foundation setup (Phase 1) ✅ **PHASE 1 COMPLETE**
   - ✅ Add Z3 dependency
   - ✅ Create SMT module structure  

2. **Week 3-4**: Constraint system and SMT translation (Phases 2-3) ✅ **PHASES 2-3 COMPLETE**
   - ✅ Define constraint types using StructuredType
   - ✅ Implement StructuredType → SMT translation
   - ✅ Build Z3 interface and solver wrapper
   - ✅ Integrate real Z3 API with constraint solving

3. **Week 5-6**: Type system integration (Phase 4)
   - Extend UnificationContext with SMT constraints
   - Add TraitUnion and ConstrainedType variants

4. **Week 7-8**: Pipeline modification (Phase 5)
   - Replace type checking phase with SMT-aware version
   - Add constraint solving phase

5. **Week 9-10**: Error reporting enhancement (Phase 6)
   - Implement SMT-based error suggestions
   - Enhanced error types and reporting

6. **Week 11-12**: Performance optimization (Phase 7)
   - Constraint caching system
   - Incremental solving capabilities

7. **Week 13-14**: Trait union expansion (Phase 8)
   - Recursive trait implementation discovery
   - Trait union type system

8. **Week 15-16**: Dispatch enhancement (Phase 9)
   - SMT-based function dispatch resolution
   - Integration with existing dispatch tables

9. **Week 17-18**: Testing & validation (Phase 10)
   - Comprehensive test coverage
   - Performance benchmarking

## Key Success Metrics

1. **All existing tests pass**: No regression in functionality
2. **Option dispatch works**: `Option.some?(value: index_of(...))` resolves correctly
3. **Nested generics resolve**: `Map<String, Option<Integer>>` types work properly
4. **Performance acceptable**: SMT solving doesn't significantly slow compilation
5. **Error messages improved**: SMT provides helpful suggestions for type errors

## Critical Implementation Notes

### Always Use StructuredType
- **❌ Wrong**: `TypeNameId` for type comparisons
- **✅ Right**: `StructuredType` to preserve generic information
- **Why**: Generics like `Option<T>` must preserve the `T` information

### Direct Integration (No Fallbacks)
- **❌ Wrong**: Traditional type checking with SMT fallback
- **✅ Right**: SMT solving as primary constraint resolution method
- **Why**: This is a prototype, we can change architecture as needed

### Test-Driven Development
- **❌ Wrong**: Breaking existing tests during implementation
- **✅ Right**: Keep all existing tests passing while adding SMT features
- **Why**: Extensive test corpus validates correctness of changes

### Trait Union Expansion
- **❌ Wrong**: `Option<Float>` as single abstract type
- **✅ Right**: Expand to `[Some<Float32>, None<Float32>, Some<Float64>, None<Float64>]`
- **Why**: Enables concrete dispatch to actual implementations

This plan provides a roadmap for solving the core trait dispatch problem while maintaining the integrity of the existing typechecker architecture.

## Progress Log

### Phase 1 Complete ✅ (Foundation Setup)

**Completed Items:**
- ✅ Added Z3 dependency to Cargo.toml 
- ✅ Created complete SMT module structure (`src/smt/`)
  - `mod.rs` - Main module exports and SMTConstraintSystem
  - `constraints.rs` - SMTConstraint enum and ConstraintSet with StructuredType
  - `solver.rs` - Z3ConstraintSolver wrapper with SolverResult and ConstraintModel
  - `translator.rs` - SMTTranslator for StructuredType → SMT-LIB conversion
  - `cache.rs` - ConstraintCache for performance optimization
  - `suggestions.rs` - ErrorSuggestionGenerator for enhanced error messages
- ✅ Integrated SMT module into lib.rs
- ✅ All code compiles successfully with Z3 dependency
- ✅ Preserved existing test infrastructure (tests still run)

**Key Implementation Notes:**
- Used StructuredType throughout (never TypeNameId alone) ✅
- Placeholder Z3 implementation ready for Phase 3 actual Z3 integration
- Designed for direct integration (no fallback strategies) ✅
- Maintained compatibility with existing compilation pipeline

**Next Phase:** SMT translation layer implementation with actual Z3 API integration.

### Phase 2 Complete ✅ (Constraint System Design)

**Completed Items:**
- ✅ Defined SMTConstraint enum with StructuredType preservation
- ✅ Created ConstraintSet and ConstraintPriority system  
- ✅ Implemented FunctionSignature with StructuredType parameters
- ✅ Added constraint validation and error handling
- ✅ Built comprehensive constraint type system

### Phase 3 Complete ✅ (SMT Translation Layer)

**Completed Items:**
- ✅ Integrated real Z3 API calls into solver.rs
- ✅ Fixed Z3 Symbol type conversion issues (String ownership)
- ✅ Implemented actual Z3 constraint solving with Bool::new_const
- ✅ Added real Z3 constraint addition with SMT formula creation
- ✅ Implemented constraint solving using Z3's check() method
- ✅ Added model extraction from satisfiable results
- ✅ Implemented unsat core extraction for debugging
- ✅ Fixed borrowing issues in extract_model_from_z3 method
- ✅ All SMT code compiles successfully with Z3 dependency

**Key Implementation Details:**
- Real Z3 Context, Solver, and Bool constraints integration ✅
- String values properly passed to Z3's Bool::new_const (not &String) ✅
- Mutable borrowing fixed for SMTTranslator usage ✅
- Actual constraint solving pipeline: add_constraints → solve → extract_model ✅
- SatResult handling: Sat, Unsat, Unknown with proper error paths ✅

**Testing & Validation:**
- ✅ Complete SMT integration test suite created (`test_smt_integration.rs`)
- ✅ Basic functionality tests: system creation, solver creation, constraint models
- ✅ Z3 satisfiability tests with proper assertions:
  - Empty constraint set satisfiability validation
  - Constraint addition and solving verification  
  - Proper error handling for Unknown/Unsatisfiable results
- ✅ Solver state management tests:
  - Active constraint tracking validation
  - Reset functionality verification
  - Post-reset operation validation
- ✅ All tests pass with meaningful assertions that would fail if Z3 integration broken

**Next Phase:** Integration with existing type system (Phase 4) - extend UnificationContext with SMT constraints.

### Phase 4 Complete ✅ (SMT-First Type System Integration)

**Completed Items:**
- ✅ **Removed hybrid approach** - eliminated ConstrainedType and TraitUnion variants from StructuredType
- ✅ **Extended UnificationContext** with comprehensive SMT constraint management
- ✅ **Added SMT-based type checking methods**:
  - `smt_types_compatible()` - replace traditional unification with SMT constraint solving
  - `smt_implements_trait()` - SMT-based trait implementation checking  
  - `smt_resolve_function_call()` - SMT-based function dispatch resolution
- ✅ **Implemented ConstraintCollector** with scope management and deferred constraints
- ✅ **Added SMT constraint accumulation and batch solving**
- ✅ **Marked traditional unification as deprecated** for replacement with SMT
- ✅ **Fixed all compilation issues** with Eq/Hash trait derivation

**Key Architecture Decisions:**
- **SMT-first approach** - All type checking operations generate SMT constraints and use Z3 for resolution
- **Clean separation** - StructuredType represents types, SMT constraints represent type relationships  
- **Future-proof design** - Any new type system complexity (unions, intersections, etc.) can be added as SMT constraints
- **Single code path** - No "simple vs complex" branching, everything goes through SMT for consistency

**Testing & Validation:**
- ✅ **7 comprehensive SMT integration tests** all passing
- ✅ **SMT-based type compatibility testing** validates Z3 integration works end-to-end
- ✅ **Constraint collection and solving workflow** verified with realistic type scenarios
- ✅ **Phase 4 architecture completeness test** confirms all major SMT interfaces work

**Performance Design:**
- **Temporary solvers** for immediate type compatibility checks
- **Constraint accumulation** for batch solving of complex scenarios
- **Caching support** ready for optimization of repeated constraint patterns

**Next Phase:** Enhanced Error Reporting (Phase 6) - implement SMT-based error suggestions and constraint relaxation.

### Phase 5 Complete ✅ (Modified Compilation Pipeline)

**Completed Items:**
- ✅ **Added new SMT-based compilation phases** to replace traditional type checking
- ✅ **Phase 6: SMT constraint collection** during type checking visitor traversal
- ✅ **Phase 7: SMT constraint solving** with accumulated constraints from unification context
- ✅ **Phase 8: SMT-guided dispatch table generation** using constraint solving results
- ✅ **Fixed compilation pipeline ordering** so dispatch calculation happens after SMT solving
- ✅ **Removed old type_check_all method** to clean up codebase as requested
- ✅ **Used actual parser** for realistic test program generation instead of manual AST construction

**Key Architecture Improvements:**
- **SMT-first pipeline** - All type checking flows through SMT constraint generation and solving
- **Proper phase ordering** - SMT solving (Phase 7) determines concrete implementations before dispatch calculation (Phase 8)
- **Clean codebase** - Removed deprecated type checking methods while maintaining functionality
- **Realistic testing** - Used `outrun_parser::parse_program()` for actual Outrun code compilation testing

**Testing & Validation:**
- ✅ **Phase 5 compilation pipeline test** validates end-to-end SMT-based compilation works
- ✅ **Parser integration** confirms real Outrun programs compile through SMT pipeline
- ✅ **All 8 SMT integration tests passing** with comprehensive constraint solving validation
- ✅ **Pipeline execution verified** with debug output showing Phase 6 SMT constraint collection

**Performance Design:**
- **Constraint accumulation** during type checking visitor traversal minimizes solver invocations
- **Batch constraint solving** in dedicated Phase 7 for efficient Z3 utilization
- **SMT result caching** ready for Phase 8 dispatch table optimization

**Next Phase:** Enhanced Error Reporting (Phase 6) - SMT-based error suggestions, constraint relaxation, and improved diagnostic messages using constraint solving results.

### Phase 6 In Progress 🔄 (Type Parameter Unification System)

**Completed Items:**
- ✅ **Added TypeParameterUnification constraint type** - handles `T = Integer` style constraints for generic trait matching
- ✅ **Implemented generic trait definition lookup** - finds `Option<T>` from `Option<Integer>` using stored trait definitions in modules
- ✅ **Added type parameter constraint generation** - creates `T = Integer` when matching `Option<Integer>` to `Option<T>`
- ✅ **Enhanced SMT solver integration** - Z3 can solve type parameter unification constraints
- ✅ **Added constraint translation** - converts TypeParameterUnification to SMT-LIB format for solving
- ✅ **Enhanced Module system** - stores trait definitions for generic parameter extraction
- ✅ **Fixed trait registration** - trait definitions properly stored in modules during compilation

**Key Architecture Achievement:**
- **Core type parameter unification** - The SMT system now correctly handles generic trait dispatch where type parameters like `T` must be consistently unified across expressions
- **Proper generic trait resolution** - `Option<Integer>.some?()` correctly resolves to `Option<T>.some?()` with constraint `T = Integer`
- **SMT constraint satisfiability** - The solver confirms trait implementations exist with proper type parameter substitution

**Debug Evidence of Success:**
```
🧠 SMT function lookup: trait Option<Integer>, impl Option<Integer>, function some?
🔍 Looking for trait definition: Option
✅ Found trait definition with 1 parameters  
🎯 Created constraint: T = Integer
🎯 Resolved trait type: Option<T>
✅ SMT constraint satisfiable - trait implementation exists
```

**Current Status:**
- **Type parameter constraints**: ✅ Working perfectly - generates `T = Integer` constraints correctly
- **SMT constraint solving**: ✅ Successfully determines trait compatibility with proper substitution
- **Final function dispatch**: ⚠️ Still needs to use SMT solutions for concrete function lookup

**Remaining Work:**
The foundation is solid - type parameter unification system correctly constrains all `T` instances to be the same but otherwise unconstrained (no trait bounds yet). Next step is applying SMT constraint solutions to final function dispatch resolution.

**Next Phase:** Apply SMT constraint solutions to dispatch resolution so that solved type parameter assignments are used in final function lookup.