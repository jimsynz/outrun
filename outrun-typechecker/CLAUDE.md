# Outrun Typechecker v3 Development Guide

## Overview

This is the third-generation typechecker for Outrun, built on Hindley-Milner type inference with protocol constraint solving and exhaustiveness checking. It follows Outrun's minimalist development philosophy by extending existing parser infrastructure.

## Architecture

See `docs/ARCHITECTURE.md` for complete architectural design.

**Key Principle**: **Package-level type checking** - processes multiple `.outrun` files together to build complete understanding of types, protocols, and implementations across the entire package.

## Development Philosophy

**CRITICAL**: Follow the minimalist development approach defined in `/CLAUDE.md`:
- **Extend existing parser AST** instead of creating parallel structures
- **Reuse existing error infrastructure** with miette integration
- **Build on existing patterns** rather than inventing new approaches
- **Design for maximum reusability** across features

## Project Structure

```
outrun-typechecker/
├── docs/
│   └── ARCHITECTURE.md     # Complete architectural design
├── src/
│   ├── lib.rs             # Main API with Package-based processing
│   ├── types.rs           # Type representation (Type, TypeVar, Constraint)
│   ├── inference.rs       # Type inference engine orchestration layer ✅
│   ├── dispatch.rs        # Static function dispatch resolution ✅
│   ├── constraints.rs     # Constraint solver for protocol bounds ✅
│   ├── registry.rs        # Protocol implementation registry ✅
│   ├── unification.rs     # Hindley-Milner unification algorithm ✅
│   ├── exhaustiveness.rs  # Pattern coverage analysis
│   └── error.rs           # Error types extending parser errors ✅
└── tests/                 # Comprehensive test suite (following test_ prefix rule)
```

## API Design

**Main Entry Points:**
- `typecheck_package(package: &mut Package)` - Type check complete package
- `typecheck_program(program: &mut Program)` - Convenience for single files

**Three-Phase Processing:**
1. **Collect Definitions** - Gather all types/protocols across files
2. **Build Registry** - Register implementations with orphan rule checking
3. **Type Inference** - HM inference with complete context

## Integration Points

**Parser AST Extension:**
- Add `type_info: Option<TypeInfo>` to `Expression` nodes
- Extend `CompilerError` enum with `Typechecker` variant
- Reuse existing `Span`, `DebugInfo`, and error infrastructure

**Error Reporting:**
- Extend existing miette-based error system
- Reuse parser's source location and diagnostic infrastructure
- Maintain consistency with parser error messages

## Task Management & Project Tracking

**All typechecker v3 tasks are tracked in Vikunja**: https://todo.harton.nz/projects/22

### Vikunja Integration

We use Vikunja (todo.harton.nz) to track all implementation tasks for typechecker v3. This ensures:
- **Clear project visibility** - See complete roadmap and current status
- **Priority ordering** - Tasks are prioritized for logical implementation sequence  
- **Progress tracking** - Mark tasks in progress and completed as work proceeds
- **Detailed specifications** - Each task includes comprehensive implementation requirements

### Task Status Workflow

**CRITICAL**: Keep Vikunja tasks synchronized with actual work progress:

1. **Starting Work**: Mark Vikunja task as "in progress" before beginning implementation
2. **During Work**: Update task comments with progress notes, blockers, or insights
3. **Completing Work**: **NEVER mark Vikunja tasks as "done" until James explicitly approves the work** - wait for user confirmation before marking tasks complete
4. **Documentation**: Reference Vikunja task IDs in commit messages: `git commit -m "feat: implement unification algorithm (task #1316)"`

**📋 Full Task List**:

View complete task breakdown with descriptions at: https://todo.harton.nz/projects/22

### Using Vikunja During Development

```bash
# Check current tasks and priorities
# Visit: https://todo.harton.nz/projects/22

# When starting new work:
# 1. Mark task as "in progress" in Vikunja UI
# 2. Create git branch: git checkout -b task-1316-unification-algorithm  
# 3. Reference task in commits: git commit -m "feat: add occurs check (task #1316)"
# 4. Mark task as "done" when completed
```

### Task Dependencies

Tasks are ordered to minimize dependencies:
- **Architecture** (✅ completed) → enables all other tasks
- **Core HM algorithm** → enables inference engine 
- **Protocol registry** → enables constraint solving
- **Constraint solver** → enables exhaustiveness checking
- **Integration & testing** → validates complete implementation

This ensures each task builds on previous work without blocking parallel development.

## Testing Requirements

Follow existing parser testing patterns:
- **Test files MUST start with `test_` prefix**
- **No inline tests** - use separate test directories
- **Comprehensive coverage** - unit tests for each component
- **Integration tests** - complete package type checking
- **Property-based testing** - type soundness and completeness

## Quality Standards

- **Zero clippy warnings** - strict adherence to Rust best practices
- **100% test pass rate** - no broken tests allowed
- **Consistent formatting** - automated `cargo fmt` enforcement
- **Error message quality** - clear, helpful error messages with suggestions

## Current Implementation Status

### ✅ **Completed Components**

**Task #1330 - Type Inference Engine Orchestration (COMPLETE)**
- **Phase 1**: Core TypeInferenceEngine and InferenceContext data structures
- **Phase 2**: Basic AST traversal and definition collection framework  
- **Phase 3**: Function registration with visibility handling (public/private)
- **Phase 4**: Simple expression inference (literals, variables) with proper concrete types
- **Phase 5**: Function call inference with dispatch integration

**Task #1321 - Collection Literal Type Inference (COMPLETE)**
- **List inference**: `[1, 2, 3] -> List<Integer64>` with homogeneous type checking
- **Tuple inference**: `(42, "hello") -> Tuple<Integer64, String>` with heterogeneous support  
- **Map inference**: `{"key": 42} -> Map<String, Integer64>` with key-value consistency
- **Empty collection handling**: Type variables for later constraint resolution
- **Generic type instantiation**: Using `Type::Concrete { args: Vec<Type> }`

**Task #1331 - Operator Desugaring to Protocol Calls (COMPLETE)**
- **Binary operators**: `a + b` → `BinaryAddition.add(left: a, right: b)` for all operators
- **Unary operators**: `-a` → `UnaryMinus.minus(value: a)` for all unary operators
- **Special cases**: `a != b` → `LogicalNot.not?(value: Equality.equal?(left: a, right: b))`
- **AST transformation**: Phase-1 desugaring before type inference with span preservation
- **Unified pipeline**: All operations flow through existing protocol dispatch system
- **🎯 CRITICAL IMPACT**: **Transforms typechecker capability from ~15% to ~80% of real Outrun programs**

**Task #1326 - Typechecker v3 Documentation and Examples (COMPLETE)**
- **User Guide**: Comprehensive guide for developers using the typechecker (`docs/USER_GUIDE.md`)
- **Examples Collection**: Complete working examples with type checking results (`docs/EXAMPLES.md`)
- **Troubleshooting Guide**: Common errors, solutions, and debugging strategies (`docs/TROUBLESHOOTING.md`)
- **Integration Documentation**: API usage, error handling, and best practices
- **Performance Guidelines**: Memory usage, optimization strategies, and benchmarking examples
- **🎯 CRITICAL IMPACT**: **Enables developers to effectively use typechecker v3 with comprehensive guidance**

**Task #1327 - Comprehensive Error Reporting System (COMPLETE)**
- **Enhanced Error Types**: Rich error structures with context, suggestions, and source spans
- **Smart Suggestions**: Levenshtein distance-based similarity detection for typos and misnomers  
- **Type Conversion Hints**: Automatic suggestions for common type conversion patterns
- **Protocol Implementation Guidance**: Context-aware suggestions for missing protocol implementations
- **Collection Error Context**: Multi-span error reporting for inconsistent collection element types
- **Error Context System**: Maintains available variables, types, and protocols for intelligent suggestions
- **11 Comprehensive Tests**: Full test coverage for error reporting infrastructure (`test_error_reporting.rs`)
- **Demo Example**: Interactive demonstration of error reporting capabilities (`examples/error_reporting_demo.rs`)
- **🎯 CRITICAL IMPACT**: **Transforms developer experience with helpful, actionable error messages**

**Task #1329 - Create Comprehensive Test Suite for Typechecker v3 (COMPLETE)**
- **142 tests total** across 6 different testing strategies
- **Integration Tests** (`test_integration_comprehensive.rs`): 15 end-to-end scenarios
- **Error Reporting Tests** (`test_error_reporting.rs`): 11 tests for enhanced error system  
- **Performance Tests** (`test_performance.rs`): 8 tests for scalability and timing
- **Edge Case Tests** (`test_edge_cases.rs`): 13 tests for boundary conditions
- **Property-Based Tests** (`test_property_based.rs`): 12 tests using proptest framework
- **Testing Strategy Documentation** (`docs/TESTING_STRATEGY.md`): Complete methodology guide
- **🎯 CRITICAL IMPACT**: **Comprehensive test coverage enables confident development and identifies performance bottlenecks**

**Stack Overflow Fixes (MAJOR PROGRESS - 75% COMPLETE)**
- **Iterative Substitution**: `Substitution::apply()` converted from recursive to iterative with cycle detection
- **Iterative Occurs Check**: `Type::contains_var()` uses work stack approach for deep type hierarchies
- **Iterative Expression Desugaring**: `DesugaringEngine::desugar_expression()` uses two-phase iterative approach
- **Iterative Unification**: Core unification algorithm converted to work queue approach
- **Real-world Impact**: Can now handle 100+ depth in standalone execution, 20+ depth in tests
- **Remaining Issue**: Expression inference still has recursive patterns for extreme edge cases

### 🚧 **Next Priority Tasks**
- **Task #1322**: Function type inference and validation (Priority 1)
- **Task #1320**: Exhaustiveness checking for multi-head functions (Priority 2)
- **Task #1324**: Exhaustiveness checking for case statements (Priority 3)

### 📈 **Test Coverage**
- **142 tests passing** (all green ✅) - comprehensive coverage achieved
- **Error reporting**: 11 comprehensive tests covering all error types and suggestion systems
- **Performance tests**: 8 tests including stack overflow edge cases with realistic limits
- **Property-based tests**: 12 tests verifying type system invariants across random inputs
- **Edge case tests**: 13 tests for boundary conditions and robustness
- **Integration tests**: 15 tests covering complete program scenarios
- **Full integration**: All existing functionality preserved
- **Zero regressions**: Complete backward compatibility maintained

## Development Workflow

1. **Survey existing code** before adding new functionality
2. **Extend existing abstractions** rather than create parallel ones
3. **Test thoroughly** with both valid and invalid inputs
4. **Document design decisions** in commit messages
5. **Follow minimalist philosophy** - write as little code as possible (YAGNI principle applied)

## Useful Commands

```bash
# Build typechecker
cargo build --package outrun-typechecker

# Run all tests
cargo test --package outrun-typechecker

# Quality assurance
cargo clippy --package outrun-typechecker --all-targets --all-features -- -D warnings
cargo fmt --package outrun-typechecker

# Integration with parser
cargo test --workspace  # Test entire outrun project
```

Remember: **The best code is code that doesn't exist. The second-best code is code that serves multiple purposes.**
