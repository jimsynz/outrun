# SMT Integration Plan for Outrun Typechecker

## Overview

This document outlines the integration of Z3 SMT solver into the Outrun typechecker to solve trait dispatch problems where functions return trait types (like `Option<T>`) that need concrete implementation resolution.

## Key Design Principles

1. **Always use StructuredType**: Never use `TypeNameId` alone - always use `StructuredType` to preserve generic information
2. **Direct SMT integration**: No fallback strategies - direct integration into core typechecker paths
3. **Test-driven approach**: Use extensive existing test corpus to validate changes
4. **Trait union expansion**: `Option<Float>` expands to all combinations of concrete implementations

## Current Project Status: 🚧 IN DEVELOPMENT (~75% Complete)

**Major SMT architecture implemented with active debugging required**

### ✅ **Core Infrastructure Complete**
- **SMT module structure**: Complete with all planned files (`constraints.rs`, `solver.rs`, `translator.rs`, `cache.rs`, etc.)
- **Z3 dependency**: Integrated with real Z3 constraint solving
- **Function clause dispatch framework**: Architecture implemented with clause sets and priority ordering
- **Performance caching**: LRU cache system with thread-local optimization (38-41% hit rates)
- **Compilation pipeline**: Phase 6.5 SMT analysis integrated into compilation pipeline

### ⚠️ **Active Issues Requiring Resolution**
- **Test stability**: Tests timeout after 2 minutes, indicating performance bottlenecks
- **Function clause dispatch**: Panic errors `TYPECHECKER BUG: No function clause set found` indicate incomplete integration
- **Runtime reliability**: Multiple test failures with panic conditions during execution
- **Performance optimization**: Despite caching, REPL startup times and test execution remain problematic

## Priority Actions for Current Development Phase

### **Immediate Priorities (Week 1-2)**
1. **Fix function clause dispatch integration** - Resolve `TYPECHECKER BUG: No function clause set found` panics
2. **Optimize SMT solver performance** - Address test timeout issues through constraint batching and solver optimizations
3. **Stabilize test harness** - Ensure reliable test execution without timeouts or panics
4. **Improve error handling** - Replace panic conditions with proper error propagation

### **Medium-term Goals (Week 3-4)**  
1. **Complete guard clause evaluation** - Finish Boolean type checking and side-effect prevention
2. **Enhance performance caching** - Improve cache hit rates and reduce SMT solver overhead
3. **Add comprehensive test coverage** - Create stable test suite for SMT integration features
4. **Implement error suggestions** - Convert SMT constraint failures into helpful user messages

## Technical Architecture Summary

### **Core SMT Components (✅ Implemented)**

**Constraint System** (`src/smt/constraints.rs`):
- `SMTConstraint` enum with trait implementation, type unification, and function signature constraints
- `ConstraintSet` with priority-based organization
- Support for guard conditions and generic type instantiation

**Z3 Integration** (`src/smt/solver.rs`):
- Real Z3 Context and Solver integration with proper resource management
- `Z3ConstraintSolver` wrapper with satisfiability checking and model extraction
- Constraint translation to SMT-LIB format for Z3 solving

**Performance Optimization** (`src/smt/cache.rs`, `src/smt/solver_pool.rs`):
- LRU cache system for constraint solving results
- Thread-local solver pool for reduced Z3 context creation overhead
- Constraint set hashing for efficient cache key generation

### **Function Clause Dispatch (⚠️ Partial Implementation)**

**Compilation Integration** (`src/compilation/compiler_environment.rs`):
- Phase 6.5 SMT clause analysis during compilation pipeline
- Function clause registration with priority-based ordering
- SMT constraint generation for guard clause applicability

**Runtime Dispatch** (`outrun-interpreter/src/function_dispatch.rs`):
- Enhanced function dispatcher with SMT-analyzed clause sets
- Guard expression evaluation framework with Boolean type checking
- Isolated evaluation context for side-effect prevention

**Purity Analysis** (`src/purity.rs`):
- Infrastructure for ensuring guard functions are side-effect-free
- Framework for compile-time purity validation (planned)

### **Known Critical Issues**

1. **Function Clause Integration Gap**: `TYPECHECKER BUG: No function clause set found` panics indicate incomplete connection between compilation and runtime
2. **Performance Bottlenecks**: Test timeouts suggest SMT solving performance needs optimization despite caching
3. **Test Reliability**: Multiple panic conditions during test execution indicate stability issues

## Development Recommendations

### **Debugging Strategy**
1. Focus on function clause dispatch integration - trace compilation → runtime data flow
2. Add comprehensive logging to identify where clause sets are lost between phases
3. Implement graceful error handling instead of panic conditions
4. Create minimal reproduction cases for failing scenarios

### **Performance Strategy**  
1. Profile SMT solver calls to identify bottlenecks
2. Implement constraint batching to reduce solver invocations
3. Add early termination for obvious type compatibility cases
4. Optimize constraint generation to avoid unnecessary complexity

### **Testing Strategy**
1. Create isolated unit tests for each SMT component
2. Add integration tests with timeout protection
3. Implement performance benchmarks for regression testing
4. Create test fixtures for common SMT constraint patterns

## Historical Context

This project represents significant architectural work implementing SMT-first type checking for the Outrun programming language. The major achievement is creating a mathematically sound type system that uses real Z3 SMT solving for trait dispatch resolution, function clause selection, and type constraint validation.

### **Key Technical Milestones**
- **Complete SMT module architecture** with Z3 integration for constraint solving
- **Function clause dispatch framework** supporting guard expressions and priority-based selection  
- **Performance caching system** with LRU eviction and thread-local optimization
- **End-to-end compilation pipeline** with SMT constraint collection and solving phases

### **Current Development Focus**
The project is in active development phase requiring debugging and performance optimization to achieve production stability. While the core architecture is sound and substantial functionality is implemented, resolving integration gaps and performance bottlenecks remains the priority for completing this innovative type system.

---

*This document has been condensed from extensive historical documentation. The previous version contained detailed implementation plans, extensive code examples, and progress logs that represented aspirational rather than current state. This version focuses on the actual current status and actionable next steps.*