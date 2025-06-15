# Outrun Type System Implementation Plan

## Project Overview

**Goal**: Build a complete static type checker for Outrun that validates trait constraints, function signatures, and expressions at compile time, generating efficient dispatch tables for the interpreter.

**Current State**: 
- ✅ Complete parser with 372+ tests
- ✅ Full AST with type annotations, patterns, expressions
- ✅ All language features parsed (traits, structs, functions, etc.)
- ✅ **Phase 1 Complete**: Type system foundation with interning, concrete types, and CLI integration
- ✅ **78 passing typechecker tests** with comprehensive error system
- ✅ **Working typecheck CLI command** with beautiful error reporting
- ✅ **Core expression type checking**: literals, binary ops, identifiers
- ✅ **Collection type checking**: Lists, tuples, maps with homogeneous/heterogeneous typing
- ✅ **Function call parameter validation**: Complete argument validation with type checking
- ✅ **Struct literal field validation**: Complete struct construction with field type checking
- ✅ **Let binding type checking**: Variable registration with pattern matching and scope updates
- ✅ **If expression type checking**: Boolean condition validation and compatible branch types
- ✅ **Block statement type checking**: Complete block processing with let bindings and expressions
- ✅ **TypedBlock, TypedStatement structures**: Full typed AST support for blocks and statements
- ✅ **Case expression type checking**: Guard validation (Boolean), compatible branch types, and block/expression results
- ✅ **Pipe operator type checking**: Basic |> and |? operators with function validation and Option type handling
- ✅ **Fully qualified type names**: All built-in types use `Outrun.Core.*` namespace
- ✅ **Phase 2 Complete**: Core expression and control flow type checking complete with comprehensive coverage
- ✅ **Phase 2.3 Pattern Type Checking Complete**: Comprehensive pattern validation for let bindings
- ✅ **102 passing typechecker tests** - all expression, pattern, and trait definition type checking validated  
- ✅ **Phase 3.1 Complete**: Trait definition processing with comprehensive validation
- ✅ **Phase 3.2 Complete**: Enhanced case statements with trait dispatch and pattern validation integration
- ✅ **Else clause removal**: Removed redundant else clauses from case expressions, now using exhaustiveness checking
- ✅ **Phase 2.4 Complete**: Static trait functions with `defs` keyword for constructor patterns and trait-level utilities
- ✅ **Generic Type Parameter Scoping Complete**: Comprehensive generic type parameter handling in trait definitions including Self type support
- ✅ **Self Parameter Validation Complete**: Instance functions must have Self parameters, static functions cannot have Self parameters
- ✅ **Default Implementation Support Complete**: Full support for trait default implementations with comprehensive override behaviour
- ✅ **Phase 3.3 Complete**: Dispatch table construction with trait implementation and static function dispatch
- ✅ **Phase 4.1 Complete**: Function signature validation with comprehensive parameter checking, guard function validation, and Self type support
- ✅ **Phase 4.2 Complete**: Enhanced function call resolution with qualified calls (Module.function) and function capture syntax (&function)
- ✅ **Phase 4.3 Complete**: Function overloading with guards - comprehensive overload resolution, conflict detection, and guard-based dispatch
- ✅ **170 passing typechecker tests** - complete type system with function definitions, qualified calls, function capture, function overloading, and all language features validated

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
- [✅] Implement `TypeContext` with scope stack
- [✅] Add variable and function registration
- [✅] Create scope pushing/popping for blocks
- [✅] Implement symbol resolution with proper scoping rules

**Deliverables**:
- [✅] `checker/context.rs` with complete scope management
- [✅] Variable shadowing and scope isolation
- [✅] Function and type symbol registration

### 2.2 Expression Type Checking
**Goal**: Type check all expression types with proper error reporting

**Tasks**:
- [✅] Implement literal type checking (integers, strings, atoms, etc.)
- [✅] Add binary operation type checking with trait dispatch
- [✅] Implement function call parameter validation with comprehensive checks
- [✅] Handle collection type checking (lists, maps, tuples)
- [✅] Add struct literal field validation with comprehensive field checking
- [✅] Implement let binding type checking and scope updates with pattern matching
- [✅] Add if expression type checking with Boolean condition and branch validation
- [✅] Add case expression type checking with guard validation (Boolean) and compatible branch types
- [✅] Add pipe operator type checking (|> and |?) with function validation and Option handling

**Deliverables**:
- [✅] `checker/expressions.rs` with core expression checking
- [✅] Proper type propagation and validation for basic expressions
- [✅] Clear error messages for type mismatches
- [✅] Full qualified type names (`Outrun.Core.*`) for built-in types
- [✅] Integration with main TypeChecker pipeline
- [✅] Collection type checking with homogeneous lists, heterogeneous tuples, typed maps

### 2.3 Pattern Type Checking
**Goal**: Validate destructuring patterns match their target types

**Tasks**:
- [✅] Implement pattern type checking for all pattern types
- [✅] Add comprehensive test coverage for pattern validation (9 tests)
- [✅] Handle recursive pattern validation with variable binding collection
- [✅] **Case Statement Enhancement (New Design)**:
  - [✅] Update LANGUAGE_SPEC.md with new case syntax (`case expr as Trait` vs `case expr`)
  - [✅] Update GRAMMAR.bnf with new case statement syntax
  - [✅] Update Pest grammar for new case statement parsing
  - [✅] Update AST structures for trait-dispatch vs concrete-type cases
  - [✅] Fix parser tests to handle new CaseExpression enum structure
  - [✅] Implement trait-based exhaustiveness checking with orphan rules
  - [✅] Add pattern validation integration for case expressions
  - [✅] Test coverage for both case variants and exhaustiveness

**Deliverables**:
- [✅] `checker/patterns.rs` with complete pattern validation
- [✅] Variable binding collection for scope updates
- [✅] Comprehensive test coverage including error cases
- [✅] **Enhanced case statement syntax**: `case expr as TraitName` for trait dispatch (parser implementation)
- [✅] **Case expression enum structure**: Concrete vs Trait variants with comprehensive test fixes
- [✅] **Trait-based exhaustiveness checking** using orphan rule analysis
- [✅] **Case expression pattern integration** with structural vs guard-based matching

### 2.4 Static Trait Functions
**Goal**: Add `defs` keyword for static trait functions with implementation

**Tasks**:
- [✅] Update LANGUAGE_SPEC.md with `defs` syntax and examples
- [✅] Update GRAMMAR.bnf with defs syntax rules
- [✅] Update grammar.pest with defs parsing rules
- [✅] Update outrun-parser AST structures for static functions
- [✅] Add comprehensive parser tests for defs syntax
- [✅] Update outrun crate's sexpr formatter for defs
- [✅] Update typechecker trait representation for static functions
- [✅] Add typechecker tests for static function validation

**Deliverables**:
- [✅] Complete `defs` syntax support across parser and type checker
- [✅] Static function call resolution and validation
- [✅] Constructor pattern support for core types (Result, Option)
- [✅] Foundation for ergonomic trait-level utilities
- [✅] Clear distinction between static functions (implemented in trait) and instance functions (implemented by types)

## Phase 3: Trait System (Week 5-6)

### 3.1 Trait Definition Processing
**Goal**: Parse and validate trait definitions and constraints

**Tasks**:
- [✅] Implement trait definition registration
- [✅] Add trait constraint validation (A: B && C)
- [✅] Handle generic trait parameters
- [✅] Validate trait function signatures

**Deliverables**:
- [✅] `types/traits.rs` with trait definition handling
- [✅] Constraint expression evaluation
- [✅] Generic parameter validation
- [✅] Guard function validation (functions ending with '?' must return Boolean)
- [✅] Type name resolution (Boolean → Outrun.Core.Boolean)
- [✅] Enhanced error handling with InvalidGuardFunction and UndefinedTypeParameter
- [✅] 8 comprehensive test cases covering all trait definition scenarios

### 3.2 Trait Implementation Validation
**Goal**: Validate impl blocks match trait requirements

**Tasks**:
- [✅] Implement trait implementation registration
- [✅] Validate all required trait functions are implemented
- [✅] Check function signature compatibility
- [✅] Handle default trait implementations (function definitions with bodies)
- [✅] Add parser support for trait function definitions with bodies
- ⚠️ Handle generic trait implementations with constraints (BLOCKED: requires Phase 3.4 parser enhancements)

**Deliverables**:
- [✅] Trait implementation validation system with comprehensive error handling
- [✅] Clear errors for missing, duplicate, and incompatible implementations
- [✅] Default implementation support with override capability
- [✅] Parser fixes for trait function definitions with bodies
- [✅] 15+ test cases covering all trait implementation scenarios
- ⚠️ Support for conditional implementations (when clauses) (BLOCKED: requires Phase 3.4 parser enhancements)

### 3.3 Dispatch Table Construction
**Goal**: Build runtime dispatch tables for efficient trait method calls

**Tasks**:
- [✅] Implement dispatch table data structures
- [✅] Build (TraitId, TypeId) -> Function mappings
- [✅] Add binary/unary operator dispatch tables (structure ready for future operator Hash+Eq support)
- [✅] Create static function lookup tables

**Deliverables**:
- [✅] `dispatch/lookup.rs` with complete dispatch system
- [✅] Pre-computed lookup tables for interpreter
- [✅] Efficient trait method resolution
- [✅] Function resolution implementation for trait and static functions
- [✅] Dispatch table validation and coherence checking
- [✅] 6 comprehensive integration tests for dispatch table construction

### 3.4 Generic Type System Completion
**Goal**: Complete parser and type checker support for generic types in all contexts

**Context**: During Phase 3.3 implementation, we discovered that while the parser has AST structures for generic types and can parse basic `impl<T> Trait<T> for Type` syntax, it has significant gaps in generic type support that prevent full generic trait implementation testing.

**Parser Gaps Identified**:
- ❌ **Generic struct definitions**: `struct Container<T>(value: T) {}` fails to parse - generic parameters and field types not captured in AST
- ❌ **Generic type arguments in TypeSpec**: `TypeSpec.generic_args` field exists but is ignored by type resolution methods
- ❌ **Type parameter constraints in struct definitions**: No parser support for `struct Wrapper<T: Display>(value: T) {}`
- ❌ **Generic function definitions**: Functions with generic parameters not supported
- ⚠️ **Limited generic impl blocks**: Only basic syntax works, complex constraint expressions may fail

**Type Checker Integration Gaps**:
- ❌ **TypeSpec generic argument resolution**: `resolve_type_spec_to_trait()` and `resolve_type_spec_to_type()` ignore `TypeSpec.generic_args`
- ❌ **Generic struct field validation**: Struct field types cannot reference generic parameters
- ❌ **Generic function signature validation**: Functions cannot have generic parameters
- ❌ **Generic constraint validation**: `when T: SomeTrait` clauses not validated against actual trait implementations

**Tasks**:
- [ ] **Parser Enhancement**: Add complete generic struct definition parsing with field type parameters
- [ ] **Parser Enhancement**: Implement generic function definition parsing (`def foo<T>(value: T): T`)
- [ ] **Parser Enhancement**: Add struct constraint parsing (`struct Wrapper<T: Display>(value: T)`)
- [ ] **Type Checker Enhancement**: Implement `TypeSpec.generic_args` resolution in trait/type lookup methods
- [ ] **Type Checker Enhancement**: Add generic struct field validation with parameter substitution
- [ ] **Type Checker Enhancement**: Implement generic function signature validation
- [ ] **Integration Testing**: Add comprehensive generic trait implementation tests with real generic types

**Deliverables**:
- [ ] Complete parser support for all generic type syntax (structs, functions, constraints)
- [ ] Full type checker support for generic type resolution and validation
- [ ] Generic struct field type validation with parameter substitution
- [ ] Generic function signature validation and constraint checking
- [ ] Comprehensive test suite covering all generic type scenarios (target: 20+ tests)
- [ ] Documentation of generic type system capabilities and limitations

**Priority**: **High** - Required to complete Phase 3.2 generic trait implementation validation

## Phase 4: Function System (Week 7) ✅ **COMPLETE**

### 4.1 Function Signature Validation ✅ **COMPLETE**
**Goal**: Validate function definitions and parameter types

**Tasks**:
- [✅] Implement function signature type checking
- [✅] Add parameter name uniqueness validation
- [✅] Handle return type checking and guard validation
- [✅] Support function overloading with guards (moved to Phase 4.3)
- [ ] Exhaustiveness checking similar to case statements for functions with guards (optional enhancement)

**Deliverables**:
- [✅] `checker/functions.rs` with comprehensive function validation
- [✅] Guard expression type checking (must return Boolean)
- [✅] Parameter validation with named argument checking
- [✅] Self-aware type resolution for impl block functions
- [✅] 10 comprehensive test cases covering all function definition scenarios

### 4.2 Function Call Resolution ✅ **COMPLETE**
**Goal**: Resolve function calls to correct implementations

**Tasks**:
- [✅] Implement qualified function call resolution (Module.function)
- [✅] Add trait static function call resolution
- [✅] Handle function capture syntax (&function_name)
- [✅] Validate argument passing and parameter matching (for non-qualified calls)

**Deliverables**:
- [✅] Enhanced function call resolution system with qualified calls
- [✅] Support for trait static function calls (ResultTrait.ok())
- [✅] Function capture validation and type checking (&function_name)
- [✅] TypedExpressionKind::FunctionCapture variant with path and arity
- [✅] 8 comprehensive test cases covering qualified calls and function capture

### 4.3 Function Overloading with Guards ✅ **COMPLETE**
**Goal**: Enable function overloading with guard-based dispatch resolution

**Tasks**:
- [✅] Extend FunctionSignature to support guard clauses and overloading
- [✅] Implement function overload conflict detection and validation
- [✅] Add function call resolution algorithm for overloaded functions
- [✅] Create comprehensive test coverage for all overloading scenarios

**Deliverables**:
- [✅] Function overloading support in `checker/context.rs` with scope-based overload storage
- [✅] ConflictingFunctionOverload error type for better error reporting
- [✅] Guard-based function resolution with default case handling
- [✅] PartialEq implementations across typed AST structures for guard clause comparison
- [✅] 9 comprehensive test cases covering overloading, conflicts, and resolution scenarios

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
- [✅] **Type context and scope management** with complete scope stack, variable/function registration, and symbol resolution
- [✅] **Core expression type checking** works for literals, binary ops, identifiers, basic function calls
- [✅] **Collection type checking** for lists, maps, tuples with proper generic type syntax
- [✅] **Built-in type system** uses proper `Outrun.Core.*` namespace
- [✅] **Type error reporting** with clear messages and source spans
- [✅] **Homogeneous/heterogeneous typing** - lists require same type, tuples allow different types
- [✅] Let binding type checking with scope updates and comprehensive pattern matching  
- [✅] If expression type checking with Boolean conditions and compatible branch types
- [✅] Case expression type checking with guard validation (Boolean) and compatible branch types
- [✅] Pipe operator type checking with basic function validation and Option/Maybe type handling
- [✅] **Pattern type checking** validates all pattern types (identifier, literal, tuple, list, struct) with recursive matching and variable binding collection

### Phase 3 Success
- [✅] Trait definitions validated and registered correctly (Phase 3.1 complete)
- [✅] Trait implementations checked against trait requirements (Phase 3.2 complete)
- [✅] Default trait implementations and parser support added
- [✅] Dispatch tables built and optimized for runtime (Phase 3.3 complete)

### Phase 4 Success ✅ **COMPLETE**
- [✅] Function signatures validated with comprehensive parameter checking
- [✅] Function calls resolve to correct implementations (qualified and non-qualified)
- [✅] Guard expressions validated as Boolean-returning
- [✅] Function capture syntax implemented and type-checked
- [✅] Enhanced function definition validation with Self type support

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
