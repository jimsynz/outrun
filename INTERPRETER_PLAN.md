# Outrun Tree-Walking Interpreter Implementation Plan

**Project**: Milestone 1.3 - Tree-walking interpreter for testing and development  
**Started**: December 6, 2025  
**Status**: Planning Complete, Ready for Implementation  

## 📋 Project Overview

Implement a tree-walking interpreter for the Outrun programming language to enable:
- Testing language features during development
- Interactive REPL for experimentation
- Foundation for future compiler development
- Educational insight into language semantics

## 🎯 Core Requirements

### Language Features to Support Initially
- ✅ **Literals**: integers, floats, booleans, strings, atoms
- ✅ **Variables**: let bindings with rebinding allowed
- ✅ **Arithmetic operators**: +, -, *, /, % (trait-based but hard-coded for now)
- ✅ **Comparison operators**: ==, <, >, <=, >=
- ✅ **Logical operators**: &&, ||, !
- ✅ **String interpolation**: "Hello #{name}"
- ✅ **Basic collections**: lists [1, 2, 3] and tuples (1, "hello", true)
- ✅ **If/else expressions**
- ✅ **Simple function calls** (built-ins only for now)

### Deliverables
1. **Value representation system** - Runtime values for all Outrun types
2. **Environment management** - Variable scoping and lookup
3. **Expression evaluator** - Tree-walking evaluation engine
4. **Built-in functions** - Core I/O, type conversion, collection operations
5. **REPL interface** - Interactive development environment
6. **Comprehensive tests** - Unit, integration, and property-based tests

## 🏗️ Architecture Design

### Module Structure
```
outrun-interpreter/
├── src/
│   ├── lib.rs              # Public API and exports
│   ├── value.rs            # Value representation system
│   ├── environment.rs      # Variable scoping and lookup
│   ├── interpreter.rs      # Core evaluation engine
│   ├── builtins.rs         # Built-in function implementations
│   ├── repl.rs             # Interactive REPL
│   └── error.rs           # Runtime error types
├── tests/                  # Comprehensive test suite
└── Cargo.toml             # Dependencies and configuration
```

### Key Dependencies
- `outrun-parser` - AST and parsing (existing)
- `indexmap` - Ordered maps for collections
- `miette` - Beautiful error reporting
- `rustyline` - REPL with history and editing
- `thiserror` - Error handling
- `proptest` - Property-based testing

## 📊 Implementation Progress

### Phase 1: Foundation (Sessions 1-2) ⏳
- [ ] **1.1** Create `outrun-interpreter` crate structure
- [ ] **1.2** Implement `Value` enum with basic operations
- [ ] **1.3** Implement `RuntimeError` types with miette integration
- [ ] **1.4** Create basic `Environment` for variable management
- [ ] **1.5** Implement literal evaluation (integers, floats, booleans, strings, atoms)

### Phase 2: Core Evaluation (Sessions 2-3) ⏳
- [ ] **2.1** Implement arithmetic operators (+, -, *, /, %)
- [ ] **2.2** Implement comparison operators (==, <, >, <=, >=)
- [ ] **2.3** Implement logical operators (&&, ||, !)
- [ ] **2.4** Implement variable lookup and let bindings
- [ ] **2.5** Implement if/else expression evaluation

### Phase 3: Advanced Features (Sessions 3-4) ⏳
- [ ] **3.1** Implement string interpolation with expression evaluation
- [ ] **3.2** Implement list literals and operations
- [ ] **3.3** Implement tuple literals and operations
- [ ] **3.4** Implement basic function call mechanism
- [ ] **3.5** Add comprehensive error handling and span preservation

### Phase 4: Built-ins and I/O (Session 4) ⏳
- [ ] **4.1** Implement I/O functions (print, println)
- [ ] **4.2** Implement type conversion functions
- [ ] **4.3** Implement collection operation functions
- [ ] **4.4** Implement type checking predicates
- [ ] **4.5** Add built-in function registry system

### Phase 5: REPL and Testing (Session 5) ⏳
- [ ] **5.1** Implement basic REPL with rustyline
- [ ] **5.2** Add REPL command system (:help, :quit, :vars)
- [ ] **5.3** Implement pretty-printing for all value types
- [ ] **5.4** Add comprehensive unit tests
- [ ] **5.5** Add integration tests with full programs

### Phase 6: Polish and Documentation (Session 6) ⏳
- [ ] **6.1** Add property-based tests for operators
- [ ] **6.2** Performance testing and optimisation
- [ ] **6.3** CLI integration with interpreter subcommand
- [ ] **6.4** Complete documentation and examples
- [ ] **6.5** Final testing and validation

## 🧪 Testing Strategy

### Test Categories
```
tests/
├── values.rs              # Value operations and type checking
├── environment.rs         # Variable scoping and lookup
├── evaluation.rs          # Expression evaluation correctness
├── builtins.rs           # Built-in function behaviour
├── integration.rs        # Complete program execution
└── properties.rs         # Property-based operator tests
```

### Key Test Scenarios
- **Arithmetic correctness**: All operators with precedence
- **Type safety**: Runtime type checking and error handling
- **String interpolation**: Complex expressions within strings
- **Variable scoping**: Let bindings, rebinding, shadowing
- **Collection operations**: List/tuple creation and access
- **Error cases**: Division by zero, undefined variables, type mismatches
- **REPL interaction**: Command handling, history, error recovery

## 🎯 Success Criteria

### Milestone 1.3 Complete When:
1. ✅ **All basic expressions evaluate correctly** (literals, variables, operators)
2. ✅ **String interpolation works** with complex expressions
3. ✅ **Collections work** (lists and tuples with basic operations)
4. ✅ **Built-in functions available** (I/O, conversion, collection ops)
5. ✅ **REPL functional** with command system and pretty printing
6. ✅ **Test coverage >95%** with comprehensive scenarios
7. ✅ **Error handling complete** with beautiful miette reporting
8. ✅ **CLI integration** for running interpreter on .outrun files

### Demo Programs
```outrun
# Basic arithmetic and variables
let x = 10
let y = 20
let result = x + y * 2
println(message: "Result: #{result}")

# String interpolation
let name = "World"
let greeting = "Hello #{name}! Today is a great day."
println(message: greeting)

# Collections
let numbers = [1, 2, 3, 4, 5]
let info = ("Alice", 30, true)
println(message: "List length: #{List.length(list: numbers)}")

# Control flow
let age = 25
let status = if age >= 18 {
    "adult"
} else {
    "minor"
}
println(message: "Status: #{status}")
```

## 📝 Session Notes

### Session 1 (December 6, 2025)
**Status**: Planning Complete ✅

**Completed**:
- ✅ Comprehensive architecture design
- ✅ Value system specification
- ✅ Environment management design
- ✅ Expression evaluation strategy
- ✅ Built-in function planning
- ✅ Error handling design
- ✅ REPL specification
- ✅ Testing strategy
- ✅ Performance considerations
- ✅ Future extension points
- ✅ Created comprehensive tracking plan file (INTERPRETER_PLAN.md)

**Next Session Goals**:
- Create `outrun-interpreter` crate
- Implement `Value` enum with basic operations
- Implement `RuntimeError` types
- Start basic expression evaluation

**Key Decisions Made**:
- Use IndexMap for ordered collections
- Integrate with existing miette error system
- Build on outrun-parser AST without modifications
- Start with hard-coded operators, trait system later
- REPL with rustyline for professional UX
- Track progress across sessions with dedicated plan file

### Session 2 (TBD)
**Goals**: Foundation implementation
- [ ] Crate structure and dependencies
- [ ] Value system implementation
- [ ] Error handling with miette
- [ ] Basic environment management

### Session 3 (TBD)
**Goals**: Core evaluation engine
- [ ] Arithmetic and comparison operators
- [ ] Variable lookup and let bindings
- [ ] If/else expressions

### Session 4 (TBD)
**Goals**: Advanced features
- [ ] String interpolation
- [ ] Collection literals
- [ ] Function calls and built-ins

### Session 5 (TBD)
**Goals**: REPL and testing
- [ ] Interactive shell
- [ ] Comprehensive test suite

### Session 6 (TBD)
**Goals**: Polish and completion
- [ ] CLI integration
- [ ] Documentation
- [ ] Final validation

## 🔄 Change Log

### 2025-12-06 - Initial Plan Creation
- Created comprehensive implementation plan
- Defined 6-phase development approach
- Specified success criteria and demo programs
- Established testing strategy and session structure

---

**Next Action**: Begin Phase 1 implementation with crate creation and Value system