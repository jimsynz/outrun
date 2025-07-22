# Outrun Typechecker v3 Architecture

## Overview

This document defines the architecture for Outrun's third-generation typechecker, built on Hindley-Milner type inference with full protocol constraint solving and exhaustiveness checking.

## Design Philosophy

Following Outrun's **minimalist development philosophy**, this typechecker prioritizes:

1. **Reuse existing infrastructure** - Extend parser AST rather than create new representations
2. **Compose existing patterns** - Build on proven error handling and diagnostic systems
3. **Maximize reusability** - Design components that serve multiple features
4. **Eliminate duplication** - Abstract common patterns into shared utilities

## Core Architecture

### High-Level Components

```
┌─────────────────────────────────┐
│         Package Input           │
│  ┌─────────┐ ┌─────────┐ ┌─────┐ │
│  │ file1   │ │ file2   │ │ ... │ │
│  │ .outrun │ │ .outrun │ │     │ │
│  └─────────┘ └─────────┘ └─────┘ │
└─────────────────────────────────┘
                │
                ▼
┌─────────────────────────────────┐
│      Multi-Phase Processing     │
│                                 │
│  Phase 1: Collect Definitions   │
│  ┌─────────────────────────────┐ │
│  │ Types, Protocols, Structs   │ │
│  └─────────────────────────────┘ │
│                                 │
│  Phase 2: Build Registry        │
│  ┌─────────────────────────────┐ │
│  │ Protocol Implementations    │ │
│  └─────────────────────────────┘ │
│                                 │
│  Phase 3: Type Inference        │
│  ┌─────────────────────────────┐ │
│  │ HM + Constraint Solving     │ │
│  └─────────────────────────────┘ │
└─────────────────────────────────┘
                │
                ▼
┌─────────────────────────────────┐
│      Typed Package Output       │
│  ┌─────────┐ ┌─────────┐ ┌─────┐ │
│  │ typed   │ │ typed   │ │ ... │ │
│  │ file1   │ │ file2   │ │     │ │
│  └─────────┘ └─────────┘ └─────┘ │
└─────────────────────────────────┘
```

### Multi-File Package Processing

**Why Package-Level Type Checking is Essential:**

1. **Cross-File Dependencies**: Types and protocols defined in one file may be used in another
2. **Complete Implementation Registry**: Need to see all protocol implementations across the package to check orphan rules
3. **Exhaustiveness Checking**: Must know all concrete types implementing a protocol for exhaustiveness analysis
4. **Import Resolution**: Alias and import statements create cross-file type references

**Three-Phase Processing:**

```rust
// Phase 1: Collect all definitions across all files
for program in &package.programs {
    context.collect_definitions(program)?;
    // Collects: struct definitions, protocol definitions, type aliases
}

// Phase 2: Build complete implementation registry
for program in &package.programs {
    context.register_implementations(program)?;
    // Registers: impl blocks with orphan rule checking
}

// Phase 3: Type check with complete context
for program in &mut package.programs {
    context.typecheck_program(program)?;
    // Uses: complete type registry + implementation registry
}
```

This approach ensures that by Phase 3, the typechecker has complete knowledge of all types, protocols, and implementations in the package, enabling accurate type checking and exhaustiveness analysis.

### Integration with Existing Parser

**✅ REUSE: Extend parser AST instead of creating parallel structures**

```rust
// EXTEND existing Expression in parser AST
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
    // ADD: Optional type information (None during parsing, Some after type checking)
    pub type_info: Option<TypeInfo>,
}

// EXTEND existing error system
#[derive(Error, Diagnostic, Debug)]
pub enum CompilerError {
    Parser(ParseError),           // Existing
    Typechecker(TypecheckError),  // NEW: Add typechecker variant
}
```

**Rationale**: Rather than creating `TypecheckerAst`, we add optional type information to existing AST nodes. This eliminates duplication and enables seamless integration.

## Data Structures

### Type Representation

**Core Type System**

```rust
/// Unified type representation for concrete types and constraints
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Concrete types: Outrun.Core.Integer64, User, List<String>
    Concrete {
        name: TypeName,
        args: Vec<Type>,
    },
    /// Protocol constraints: Integer, Display, Result<T, E>
    Protocol {
        name: ProtocolName,
        args: Vec<Type>,
    },
    /// Type variables for inference: T, U, V
    Variable(TypeVar),
    /// Self type (special case of type variable)
    SelfType,
    /// Function types: Function<(x: Integer) -> String>
    Function {
        params: Vec<(ParamName, Type)>,
        return_type: Box<Type>,
    },
}

/// Type variable for Hindley-Milner inference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    id: TypeVarId,
    name: Option<String>,  // For debugging/error messages
}

/// Type constraints for protocol bounds
#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    /// T: Display
    Implements { type_var: TypeVar, protocol: ProtocolName },
    /// T: Display || T: Debug  
    LogicalOr { left: Box<Constraint>, right: Box<Constraint> },
    /// T: Display && T: Clone
    LogicalAnd { left: Box<Constraint>, right: Box<Constraint> },
    /// Self == Container<T> (for Self binding)
    SelfEquals { self_type: Type },
}
```

### Hindley-Milner Inference Engine

**Unification Algorithm**

```rust
/// Type unification for HM inference
pub struct Unifier {
    substitutions: HashMap<TypeVar, Type>,
    occurs_check: bool,
}

impl Unifier {
    /// Unify two types, updating substitutions
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), UnificationError> {
        match (t1, t2) {
            (Type::Variable(var), ty) | (ty, Type::Variable(var)) => {
                self.bind_variable(var.clone(), ty.clone())
            }
            (Type::Concrete { name: n1, args: a1 }, Type::Concrete { name: n2, args: a2 }) => {
                if n1 == n2 && a1.len() == a2.len() {
                    for (arg1, arg2) in a1.iter().zip(a2.iter()) {
                        self.unify(arg1, arg2)?;
                    }
                    Ok(())
                } else {
                    Err(UnificationError::TypeMismatch { expected: t1.clone(), found: t2.clone() })
                }
            }
            // ... other cases
        }
    }

    /// Apply substitutions to resolve final types
    pub fn apply(&self, ty: &Type) -> Type { /* ... */ }
}
```

**Type Inference Context**

```rust
/// Context for type inference with environment and constraints
pub struct InferenceContext {
    /// Variable bindings: variable_name -> type
    environment: HashMap<Identifier, Type>,
    /// Fresh type variable generator
    type_var_gen: TypeVarGenerator,
    /// Accumulated constraints during inference
    constraints: Vec<Constraint>,
    /// Unifier for solving type equations
    unifier: Unifier,
}

impl InferenceContext {
    /// Infer type of expression, returning type and constraints
    pub fn infer_expression(&mut self, expr: &Expression) -> Result<Type, InferenceError> {
        match &expr.kind {
            ExpressionKind::IntegerLiteral(_) => {
                Ok(Type::Concrete { 
                    name: TypeName::new("Outrun.Core.Integer64"), 
                    args: vec![] 
                })
            }
            ExpressionKind::FunctionCall(call) => {
                self.infer_function_call(call)
            }
            ExpressionKind::ListLiteral(list) => {
                self.infer_list_literal(list)
            }
            // ... other expression types
        }
    }

    /// Generate fresh type variable
    pub fn fresh_type_var(&mut self) -> Type {
        Type::Variable(self.type_var_gen.fresh())
    }
}
```

### Protocol Implementation Registry

**Implementation Lookup**

```rust
/// Registry of all protocol implementations
pub struct ImplementationRegistry {
    /// Map: (Protocol, Type) -> Implementation
    implementations: HashMap<(ProtocolName, TypeName), Implementation>,
    /// Orphan rule tracking: implementation -> module
    implementation_origins: HashMap<ImplementationId, ModuleName>,
}

impl ImplementationRegistry {
    /// Register implementation with orphan rule checking
    pub fn register_impl(
        &mut self, 
        protocol: ProtocolName,
        type_name: TypeName,
        impl_def: Implementation,
        local_module: ModuleName,
    ) -> Result<(), OrphanRuleError> {
        // Check orphan rule: at least one of protocol or type must be local
        if !self.is_local_protocol(&protocol, &local_module) && 
           !self.is_local_type(&type_name, &local_module) {
            return Err(OrphanRuleError::BothForeign { protocol, type_name });
        }

        // Check for conflicts
        let key = (protocol.clone(), type_name.clone());
        if self.implementations.contains_key(&key) {
            return Err(OrphanRuleError::ConflictingImplementation { protocol, type_name });
        }

        self.implementations.insert(key, impl_def);
        Ok(())
    }

    /// Resolve protocol call to specific implementation
    pub fn resolve_call(
        &self, 
        protocol: &ProtocolName, 
        concrete_type: &Type
    ) -> Result<&Implementation, DispatchError> {
        match concrete_type {
            Type::Concrete { name, .. } => {
                let key = (protocol.clone(), name.clone());
                self.implementations.get(&key)
                    .ok_or_else(|| DispatchError::NoImplementation { 
                        protocol: protocol.clone(), 
                        type_name: name.clone() 
                    })
            }
            _ => Err(DispatchError::NonConcreteType { ty: concrete_type.clone() }),
        }
    }
}
```

### Constraint Solver

**Protocol Constraint Resolution**

```rust
/// Solver for protocol constraints with logical operators
pub struct ConstraintSolver {
    registry: ImplementationRegistry,
    satisfiability_cache: HashMap<Constraint, bool>,
}

impl ConstraintSolver {
    /// Check if constraint is satisfiable by concrete type
    pub fn is_satisfiable(&mut self, constraint: &Constraint, concrete_type: &Type) -> bool {
        match constraint {
            Constraint::Implements { protocol, .. } => {
                self.registry.resolve_call(protocol, concrete_type).is_ok()
            }
            Constraint::LogicalOr { left, right } => {
                self.is_satisfiable(left, concrete_type) || 
                self.is_satisfiable(right, concrete_type)
            }
            Constraint::LogicalAnd { left, right } => {
                self.is_satisfiable(left, concrete_type) && 
                self.is_satisfiable(right, concrete_type)
            }
            Constraint::SelfEquals { self_type } => {
                self.unify_self_type(self_type, concrete_type)
            }
        }
    }

    /// Solve constraints by finding satisfying substitutions
    pub fn solve_constraints(
        &mut self, 
        constraints: &[Constraint]
    ) -> Result<HashMap<TypeVar, Type>, ConstraintError> {
        // Implementation using constraint satisfaction techniques
        // Similar to Rust's trait solver but simpler
        todo!("Implement constraint solving algorithm")
    }
}
```

### Exhaustiveness Checker

**Pattern Coverage Analysis**

```rust
/// Exhaustiveness checker for case expressions and multi-head functions
pub struct ExhaustivenessChecker {
    registry: ImplementationRegistry,
}

impl ExhaustivenessChecker {
    /// Check case expression exhaustiveness
    pub fn check_case_exhaustiveness(
        &self,
        input_type: &Type,
        patterns: &[Pattern],
        protocol_annotation: Option<&ProtocolName>,
    ) -> Result<(), ExhaustivenessError> {
        match protocol_annotation {
            Some(protocol) => {
                // Protocol dispatch case: must handle all implementing types
                let implementing_types = self.registry.get_implementing_types(protocol);
                self.check_protocol_pattern_coverage(&implementing_types, patterns)
            }
            None => {
                // Regular case: check pattern coverage for concrete type
                self.check_concrete_pattern_coverage(input_type, patterns)
            }
        }
    }

    /// Check multi-head function exhaustiveness
    pub fn check_function_exhaustiveness(
        &self,
        function_heads: &[FunctionHead],
    ) -> Result<(), ExhaustivenessError> {
        // Analyze guard coverage using boolean satisfiability
        let guard_expressions: Vec<_> = function_heads.iter()
            .map(|head| &head.guard)
            .collect();
        
        self.check_guard_coverage(&guard_expressions)
    }
}
```

## Integration Strategy

### Extending Parser AST

**✅ MINIMAL CHANGE: Add optional type information**

```rust
// In outrun-parser/src/ast.rs - ADD these fields

/// Type information attached to expressions after type checking
#[derive(Debug, Clone, PartialEq)]
pub struct TypeInfo {
    /// Inferred or annotated type
    pub resolved_type: Type,
    /// Type constraints that were satisfied
    pub constraints: Vec<Constraint>,
    /// Protocol implementation used (for dispatch)
    pub implementation: Option<ImplementationId>,
}

// EXTEND existing Expression
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
    // ADD: Optional type information
    pub type_info: Option<TypeInfo>,  // None during parsing, Some after typechecking
}

// EXTEND existing error enum
pub enum CompilerError {
    Parser(ParseError),
    Typechecker(TypecheckError),  // NEW variant
}
```

### Error Handling Integration

**✅ REUSE: Extend existing miette-based error system**

```rust
// In outrun-typechecker/src/error.rs

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;
use outrun_parser::ParseError;  // REUSE existing error types

#[derive(Error, Diagnostic, Debug)]
pub enum TypecheckError {
    #[error("Type mismatch")]
    #[diagnostic(
        code(outrun::typecheck::type_mismatch),
        help("Expected {expected}, but found {found}")
    )]
    TypeMismatch {
        #[source_code]
        src: String,
        #[label("expected {expected}")]
        span: SourceSpan,
        expected: String,
        found: String,
    },

    #[error("Unification failed")]
    #[diagnostic(code(outrun::typecheck::unification_failed))]
    UnificationFailed {
        #[source_code]
        src: String,
        #[label("cannot unify these types")]
        span: SourceSpan,
        details: String,
    },

    // ... other typecheck-specific errors
}

// COMPOSE with existing parser errors
#[derive(Error, Diagnostic, Debug)]
pub enum CompilerError {
    #[error(transparent)]
    Parser(#[from] ParseError),        // REUSE existing
    
    #[error(transparent)]
    Typechecker(#[from] TypecheckError), // ADD new
}
```

## Implementation Plan

### Phase 1: Core Infrastructure (Tasks #1-2 in Vikunja)

1. **Create basic project structure** - `Cargo.toml`, module organization
2. **Implement type representation** - `Type` enum, `TypeVar`, `Constraint`
3. **Build unification algorithm** - Core HM unification with occurs check
4. **Extend parser AST** - Add `TypeInfo` field to expressions

### Phase 2: Type Inference Engine (Tasks #3-5)

1. **Implement inference context** - Environment, constraint accumulation
2. **Expression type inference** - Handle all expression types
3. **Self type binding** - Special handling for Self in protocols/implementations
4. **Function type inference** - Anonymous functions and signatures

### Phase 3: Protocol System (Tasks #6-7)

1. **Implementation registry** - Protocol -> Type -> Implementation mapping
2. **Orphan rule checking** - Coherence validation
3. **Constraint solver** - Logical constraint satisfaction
4. **Static dispatch resolution** - Compile-time call resolution

### Phase 4: Advanced Features (Tasks #8-11)

1. **Exhaustiveness checking** - Case expressions and multi-head functions
2. **Collection type inference** - Lists, maps, tuples with generic instantiation
3. **Error reporting** - Integration with miette diagnostic system
4. **Comprehensive testing** - Unit and integration test coverage

## Performance Considerations

### Optimization Strategies

**Type Variable Interning**
```rust
/// Interned type variables for fast equality checks
pub struct TypeVarInterner {
    strings: HashMap<String, TypeVarId>,
    next_id: TypeVarId,
}
```

**Constraint Caching**
```rust
/// Cache constraint satisfaction results
pub struct ConstraintCache {
    satisfiability: HashMap<(Constraint, Type), bool>,
    implementation_lookup: HashMap<(ProtocolName, TypeName), ImplementationId>,
}
```

**Incremental Type Checking**
```rust
/// Support for incremental compilation
pub struct TypecheckContext {
    /// Cache type information for unchanged functions
    function_cache: HashMap<FunctionId, TypeInfo>,
    /// Track dependencies for invalidation
    dependencies: DependencyGraph,
}
```

## Testing Strategy

### Unit Testing
- **Unification algorithm** - All type combinations and edge cases
- **Constraint solver** - Logical constraint satisfaction  
- **Exhaustiveness checker** - Pattern coverage analysis
- **Self type binding** - Protocol vs implementation semantics

### Integration Testing
- **Complete programs** - End-to-end type checking
- **Error scenarios** - Expected type errors with clear messages
- **Performance testing** - Large programs and constraint solving
- **Round-trip testing** - Parser + typechecker integration

### Property-Based Testing
- **Type soundness** - Well-typed programs don't get stuck
- **Completeness** - All valid programs type check
- **Constraint satisfaction** - Solved constraints are actually satisfied

## Future Extensions

### Planned Features
- **Type inference optimization** - Local type inference for better performance
- **Advanced exhaustiveness** - Symbolic execution for guard analysis
- **LSP integration** - Hover information and error reporting
- **Incremental compilation** - Fast re-checking of changed code

### Architecture Flexibility
The modular design supports future extensions:
- **Custom constraint domains** - Beyond protocol constraints
- **Alternative inference algorithms** - Local inference, bidirectional checking
- **Advanced dispatch strategies** - JIT compilation, specialization
- **Debugging integration** - Type information preservation for runtime

This architecture provides a solid foundation for a production-quality typechecker that integrates seamlessly with Outrun's existing infrastructure while following the minimalist development philosophy.