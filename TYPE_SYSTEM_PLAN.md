# Outrun Type System Implementation Plan

## Project Overview

**Goal**: Build a complete static type checker for Outrun that validates trait constraints, function signatures, and expressions at compile time, generating efficient dispatch tables for the interpreter.

**Current State**: 
- ✅ Complete parser with 372+ tests
- ✅ Full AST with type annotations, patterns, expressions
- ✅ All language features parsed (traits, structs, functions, etc.)
- ✅ **Phase 1 Complete**: Type system foundation with interning, concrete types, and CLI integration
- ✅ **52 passing typechecker tests** with comprehensive error system
- ✅ **Working typecheck CLI command** with beautiful error reporting
- ⏳ Ready for Phase 2: Core type checking implementation

**Target State**:
- ✅ Static type checking with trait constraint validation
- ✅ Pre-computed trait dispatch tables for interpreter
- ✅ Comprehensive type error reporting with miette
- ✅ Support for all Outrun language features
- ✅ Foundation for future type inference

## Phase 1: Foundation (Week 1-2)

### 1.1 Project Structure
**Goal**: Set up the typechecker crate with clean module organization

**Tasks**:
- [✅] Create `outrun-typechecker/` crate with proper Cargo.toml
- [✅] Set up module structure (`types/`, `checker/`, `dispatch/`)
- [✅] Add dependencies (string-interner, miette, thiserror)
- [✅] Create basic lib.rs with public API stubs

**Deliverables**:
- Working crate that compiles
- Module structure matches architecture design
- Basic test infrastructure in place

### 1.2 Type Interning System
**Goal**: Implement fast TypeId/AtomId system for performance

**Tasks**:
- [✅] Implement `TypeInterner` with string-interner crate
- [✅] Create `TypeId`, `AtomId`, `TraitId` wrapper types
- [✅] Add interning methods and lookup functions
- [✅] Write comprehensive tests for interning behavior

**Deliverables**:
- `types/interning.rs` with full interning system
- Fast equality comparisons for types and atoms
- Thread-safe design for future multi-threading

### 1.3 Concrete Type Definitions
**Goal**: Define all Outrun concrete types and their relationships

**Tasks**:
- [✅] Implement `ConcreteType` enum with all Outrun types
- [✅] Add `StructField`, `FunctionSignature` supporting types
- [✅] Implement type validation and compatibility checking
- [✅] Create type registry and management utilities

**Deliverables**:
- `types/concrete.rs` with complete type system
- Type compatibility and validation functions
- Support for generics and collection types

## Phase 2: Core Type Checking (Week 3-4)

### 2.1 Type Context and Scoping
**Goal**: Build the foundation for type checking with proper scope management

**Tasks**:
- [ ] Implement `TypeContext` with scope stack
- [ ] Add variable and function registration
- [ ] Create scope pushing/popping for blocks
- [ ] Implement symbol resolution with proper scoping rules

**Deliverables**:
- `checker/context.rs` with complete scope management
- Variable shadowing and scope isolation
- Function and type symbol registration

### 2.2 Expression Type Checking
**Goal**: Type check all expression types with proper error reporting

**Tasks**:
- [ ] Implement literal type checking (integers, strings, atoms, etc.)
- [ ] Add binary operation type checking with trait dispatch
- [ ] Implement function call parameter validation
- [ ] Add struct literal field validation
- [ ] Handle collection type checking (lists, maps, tuples)

**Deliverables**:
- `checker/expressions.rs` with complete expression checking
- Proper type propagation and validation
- Clear error messages for type mismatches

### 2.3 Pattern Type Checking
**Goal**: Validate destructuring patterns match their target types

**Tasks**:
- [ ] Implement pattern type checking for all pattern types
- [ ] Add pattern exhaustiveness checking for case expressions
- [ ] Validate pattern variable bindings and scoping
- [ ] Handle recursive pattern validation

**Deliverables**:
- `checker/patterns.rs` with pattern validation
- Exhaustiveness checking for case statements
- Proper variable binding in patterns

## Phase 3: Trait System (Week 5-6)

### 3.1 Trait Definition Processing
**Goal**: Parse and validate trait definitions and constraints

**Tasks**:
- [ ] Implement trait definition registration
- [ ] Add trait constraint validation (A: B && C)
- [ ] Handle generic trait parameters
- [ ] Validate trait function signatures

**Deliverables**:
- `types/traits.rs` with trait definition handling
- Constraint expression evaluation
- Generic parameter validation

### 3.2 Trait Implementation Validation
**Goal**: Validate impl blocks match trait requirements

**Tasks**:
- [ ] Implement trait implementation registration
- [ ] Validate all required trait functions are implemented
- [ ] Check function signature compatibility
- [ ] Handle generic trait implementations with constraints

**Deliverables**:
- Trait implementation validation system
- Clear errors for missing or incompatible implementations
- Support for conditional implementations (when clauses)

### 3.3 Dispatch Table Construction
**Goal**: Build runtime dispatch tables for efficient trait method calls

**Tasks**:
- [ ] Implement dispatch table data structures
- [ ] Build (TraitId, TypeId) -> Function mappings
- [ ] Add binary/unary operator dispatch tables
- [ ] Create static function lookup tables

**Deliverables**:
- `dispatch/lookup.rs` with complete dispatch system
- Pre-computed lookup tables for interpreter
- Efficient trait method resolution

## Phase 4: Function System (Week 7)

### 4.1 Function Signature Validation
**Goal**: Validate function definitions and parameter types

**Tasks**:
- [ ] Implement function signature type checking
- [ ] Add parameter name uniqueness validation
- [ ] Handle return type checking and guard validation
- [ ] Support function overloading with guards

**Deliverables**:
- `checker/functions.rs` with function validation
- Guard expression type checking (must return Boolean)
- Parameter validation with named argument checking

### 4.2 Function Call Resolution
**Goal**: Resolve function calls to correct implementations

**Tasks**:
- [ ] Implement static function call resolution (Module.function)
- [ ] Add trait function call resolution with explicit types
- [ ] Handle function capture syntax (&function_name)
- [ ] Validate argument passing and parameter matching

**Deliverables**:
- Complete function call resolution system
- Support for both static and trait function calls
- Function capture validation and type checking

## Phase 5: Integration (Week 8)

### 5.1 Typed AST Generation
**Goal**: Generate typed AST nodes for interpreter consumption

**Tasks**:
- [ ] Create `TypedProgram` and `TypedExpression` types
- [ ] Add type information to all expression nodes
- [ ] Implement typed AST construction from checking results
- [ ] Create conversion utilities from untyped to typed AST

**Deliverables**:
- Complete typed AST with type information
- Clean conversion from parser AST to typed AST
- Type information preservation for interpreter

### 5.2 Error Reporting Integration
**Goal**: Beautiful error reporting with miette integration

**Tasks**:
- [ ] Implement comprehensive `TypeError` definitions
- [ ] Add miette diagnostic integration with source spans
- [ ] Create helpful error messages and suggestions
- [ ] Add error context and recovery strategies

**Deliverables**:
- `error.rs` with complete error system
- Beautiful error reporting with source highlighting
- Helpful error messages and suggestions

### 5.3 Public API and Documentation
**Goal**: Clean public API for integration with other tools

**Tasks**:
- [ ] Design clean public API for typechecking
- [ ] Add comprehensive documentation and examples
- [ ] Create integration points for CLI and future LSP
- [ ] Write usage examples and getting started guide

**Deliverables**:
- Clean public API in `lib.rs`
- Comprehensive documentation
- Integration examples and tests

## Phase 6: Testing and Validation (Week 9)

### 6.1 Comprehensive Test Suite
**Goal**: Ensure type checker handles all language features correctly

**Tasks**:
- [ ] Write unit tests for all type checking components
- [ ] Add integration tests with real Outrun programs
- [ ] Create error case tests for all error conditions
- [ ] Add performance tests for large programs

**Deliverables**:
- Comprehensive test suite (aim for 200+ tests)
- Coverage of all type checking functionality
- Performance benchmarks and validation

### 6.2 CLI Integration
**Goal**: Integrate type checker with Outrun CLI tool

**Tasks**:
- [✅] Add typecheck command to CLI
- [✅] Integrate with existing parse command
- [✅] Add typed AST output options
- [✅] Create comprehensive error reporting in CLI

**Deliverables**:
- CLI integration with typecheck command
- Beautiful error output in terminal
- Typed AST visualization options

## Success Criteria

### Phase 1 Success
- [✅] Typechecker crate compiles and has basic structure
- [✅] Type interning system works with fast lookups
- [✅] All concrete types defined and validatable

### Phase 2 Success
- [ ] Expression type checking works for all expression types
- [ ] Pattern matching validation handles all pattern types
- [ ] Scope management works correctly with proper isolation

### Phase 3 Success
- [ ] Trait definitions validated and registered correctly
- [ ] Trait implementations checked against trait requirements
- [ ] Dispatch tables built and optimized for runtime

### Phase 4 Success
- [ ] Function signatures validated with proper parameter checking
- [ ] Function calls resolve to correct implementations
- [ ] Guard expressions validated as Boolean-returning

### Phase 5 Success
- [ ] Typed AST generated with complete type information
- [ ] Beautiful error reporting with source highlighting
- [ ] Clean public API ready for integration

### Phase 6 Success
- [ ] Comprehensive test suite with high coverage
- [ ] CLI integration working with real programs
- [ ] Performance suitable for real-world usage

## Risk Assessment

### High Risk
- **Trait dispatch complexity**: The trait system is sophisticated and may need iteration
- **Performance requirements**: Type checking must be fast enough for real-time feedback
- **Error message quality**: Users need clear, actionable error messages

### Medium Risk
- **Generic type handling**: Generic parameters and constraints add complexity
- **Pattern exhaustiveness**: Ensuring complete coverage checking is challenging
- **Integration complexity**: Connecting all components smoothly

### Low Risk
- **Basic type checking**: Core expression validation is well-understood
- **AST integration**: Parser AST is well-designed for type information
- **Test infrastructure**: Strong foundation from parser development

## Dependencies and Assumptions

### External Dependencies
- `string-interner`: Fast string interning for TypeId/AtomId
- `miette`: Error reporting with source highlighting
- `thiserror`: Ergonomic error handling
- `outrun-parser`: Existing parser crate with AST definitions

### Assumptions
- Parser AST is stable and won't require major changes
- Trait system design is sufficient for Outrun's needs
- Performance requirements can be met with chosen architecture
- Error reporting integration will work smoothly with existing infrastructure

## Future Extensions (Post-MVP)

### Type Inference
- Implement Hindley-Milner style type inference
- Reduce need for explicit type annotations
- Maintain trait dispatch efficiency

### Advanced Error Recovery
- Better error recovery during type checking
- Multiple error reporting with fix suggestions
- IDE integration for real-time feedback

### Optimization
- Incremental type checking for large codebases
- Parallel type checking for independent modules
- Advanced dispatch table optimization

### Tooling Integration
- Language Server Protocol (LSP) support
- IDE plugins with type information
- Debugging integration with type information

---

## Next Steps

1. **Review and iterate on this plan** with stakeholders
2. **Start Phase 1.1**: Create the typechecker crate structure
3. **Set up CI/CD** for the new crate with proper testing
4. **Begin implementation** following the phased approach

This plan provides a structured approach to building a complete type system while maintaining quality and testing throughout the process.