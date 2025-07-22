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

### 🚧 **Next Priority Tasks**
- **Task #1322**: Pattern matching type inference
- **Task #1323**: Control flow expressions (if, case, blocks)
- **Task #1324**: Constraint resolution and error reporting integration

### 📈 **Test Coverage**
- **72 tests passing** (all green ✅)
- Collection inference: 3 new comprehensive tests (list, tuple, map)
- Integration with existing dispatch and constraint systems
- Function call inference with proper error handling

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
