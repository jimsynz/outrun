# Typed AST Extension Plan

**Goal**: Extend the typechecker's typed AST to contain the full type-checked result with all nodes present and their typing information.

**Current Status**: The typed AST is extremely minimal (only basic literals) while the parser AST is comprehensive (2100+ lines, 40+ structures). This plan bridges that 95% gap.

## Gap Analysis Summary

### ✅ Current Typed AST Coverage
- Basic literals (integer, float, string, boolean, atom)
- Simple identifiers
- Placeholder mechanism for unsupported constructs

### ❌ Missing from Typed AST (95% of language)
- **Core Language**: Binary/unary operations, function calls, field access, qualified identifiers
- **Collections**: Lists, maps, tuples, struct literals with type validation
- **Control Flow**: If expressions, case expressions, pattern matching
- **Functions**: Definitions, anonymous functions, captures, parameters, guards
- **Type System**: Struct/trait/impl definitions, generics, type annotations
- **Patterns**: All pattern types with variable binding
- **Advanced**: Macros, sigils, string interpolation, attributes
- **Module System**: Imports, aliases, qualified names
- **Statements**: Let bindings, const definitions, blocks
- **Source Info**: Comments, debug info, format preservation, spans

## Implementation Plan

### ⚠️ **Important Discovery: Integration Architecture**

**Issue**: The initial approach of extending `convert_expression_simple()` bypasses the actual type checking system. The TypeCheckingVisitor performs real type checking but doesn't produce a comprehensive typed AST.

**Solution**: Two-phase approach:
1. **Phase 1**: TypeCheckingVisitor validates types and stores results
2. **Phase 2**: New TypedASTBuilder visitor creates comprehensive typed AST using validation results

**Updated Architecture**:
```
Phase 1-4: Extract traits, structs, impls, functions  
Phase 5: TypeCheckingVisitor (validates types, stores results)
Phase 6: TypedASTBuilder (creates comprehensive typed AST using validation results)
```

This maintains separation between type validation and AST construction while ensuring the typed AST contains complete, validated type information.

### Phase 1: Core Expression System (Weeks 1-2)

#### Week 1: Binary Operations & Function Calls
**Files to modify:**
- `outrun-typechecker/src/checker.rs` - Add new `TypedExpressionKind` variants
- `outrun-typechecker/src/visitor.rs` - Update visitor pattern

**New structures:**
```rust
// Add to TypedExpressionKind enum
BinaryOp {
    operator: BinaryOperator,
    left: Box<TypedExpression>,
    right: Box<TypedExpression>,
    trait_method: Option<String>, // Resolved trait method call
},
FunctionCall {
    function_path: TypedFunctionPath,
    arguments: Vec<TypedArgument>,
    resolved_function: FunctionId, // Reference to function registry
    dispatch_method: DispatchMethod, // Static or trait dispatch
},
UnaryOp {
    operator: UnaryOperator,
    operand: Box<TypedExpression>,
    trait_method: Option<String>,
},
FieldAccess {
    object: Box<TypedExpression>,
    field: String,
    field_type: TypeId,
},
```

**Tasks:**
- [x] Define `TypedFunctionPath`, `TypedArgument`, `DispatchMethod` enums
- [x] Add `FunctionCall` and `FieldAccess` to `TypedExpressionKind` (BinaryOp/UnaryOp not needed - desugared)
- [x] Update structures to use `Option<StructuredType>` instead of guessing types
- [x] Create `TypedASTBuilder` visitor that runs after `TypeCheckingVisitor`
- [x] Implement integration with existing 6-phase compilation pipeline
- [x] Write and test TypedASTBuilder visitor (500+ lines implemented)
- [x] **Write comprehensive Week 1 tests (14 tests covering all core functionality)**

**✅ Week 1 Complete** - All core expression infrastructure implemented, integrated, and thoroughly tested

**Week 1 Test Coverage:**
- ✅ All basic literals (integer, float, boolean, string, atom) - 6 tests
- ✅ Identifiers and variable references - 1 test  
- ✅ Function calls (simple, qualified, multi-argument) - 3 tests
- ✅ Field access (simple and chained) - 2 tests
- ✅ Binary operation desugaring verification - 1 test
- ✅ Nested expressions and string interpolation - 2 tests

#### Week 2: Collection Literals
**New structures:**
```rust
// Add to TypedExpressionKind enum
List {
    elements: Vec<TypedExpression>,
    element_type: Option<StructuredType>, // Homogeneous type validation
},
Map {
    entries: Vec<TypedMapEntry>,
    key_type: Option<StructuredType>,
    value_type: Option<StructuredType>,
},
Tuple {
    elements: Vec<TypedExpression>,
    tuple_type: Option<StructuredType>, // Composite tuple type
},
StructLiteral {
    type_path: Vec<String>,
    fields: Vec<TypedStructField>,
    struct_type: Option<StructuredType>,
},
```

**Tasks:**
- [x] Define `TypedMapEntry`, `TypedStructField` structures
- [x] Implement homogeneous type checking for lists
- [x] Add map entry type validation (key-value pairs)
- [x] Implement struct field validation against definitions
- [x] Add support for spread operators in collections (basic support added)
- [x] Write comprehensive collection tests (6 tests covering all collection types)

**✅ Week 2 Complete** - All collection literals supported with typed AST conversion

### Phase 2: Control Flow & Patterns (Weeks 3-4)

#### Week 3: Pattern System
**Files to create/modify:**
- `outrun-typechecker/src/patterns.rs` - New module for pattern types

**New structures:**
```rust
#[derive(Debug, Clone, PartialEq)]
pub struct TypedPattern {
    pub kind: TypedPatternKind,
    pub pattern_type: TypeId,
    pub bound_variables: Vec<BoundVariable>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedPatternKind {
    Identifier(String),
    Literal(TypedLiteral),
    Tuple(Vec<TypedPattern>),
    Struct {
        type_path: Vec<String>,
        fields: Vec<TypedStructFieldPattern>,
    },
    List {
        elements: Vec<TypedPattern>,
        rest: Option<String>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoundVariable {
    pub name: String,
    pub variable_type: TypeId,
    pub span: Span,
}
```

**Tasks:**
- [x] Create `patterns.rs` module with typed pattern structures
- [x] Implement pattern type validation and variable binding collection
- [x] Add exhaustiveness checking foundation for patterns
- [x] Support nested pattern destructuring
- [x] Write comprehensive pattern matching tests (11 tests covering all pattern functionality)

**✅ Week 3 Complete** - Comprehensive pattern system implemented with full type validation and variable binding collection

#### Week 4: Control Flow
**New structures:**
```rust
// Add to TypedExpressionKind enum
IfExpression {
    condition: Box<TypedExpression>,
    then_branch: Box<TypedExpression>,
    else_branch: Option<Box<TypedExpression>>,
    result_type: TypeId,
},
CaseExpression {
    variant: TypedCaseVariant,
    result_type: TypeId,
},

#[derive(Debug, Clone, PartialEq)]
pub enum TypedCaseVariant {
    Concrete {
        expression: Box<TypedExpression>,
        when_clauses: Vec<TypedWhenClause>,
    },
    Trait {
        expression: Box<TypedExpression>,
        trait_name: String,
        trait_id: TraitId,
        type_clauses: Vec<TypedTraitClause>,
    },
}
```

**Tasks:**
- [x] Define `TypedWhenClause`, `TypedAsClause` structures  
- [x] Implement if and case expression conversion in TypedASTBuilder
- [x] Add basic control flow expression support 
- [x] Support trait-based case dispatch structures
- [x] Write comprehensive control flow tests (5 tests covering all control flow scenarios)

**✅ Week 4 Complete** - Control flow expressions (if/case) implemented with typed AST conversion and comprehensive test coverage

### Phase 3: Function & Type System (Weeks 5-6)

#### Week 5: Function Definitions
**New structures:**
```rust
#[derive(Debug, Clone)]
pub struct TypedFunctionDefinition {
    pub name: String,
    pub parameters: Vec<TypedParameter>,
    pub return_type: TypeId,
    pub guard: Option<TypedExpression>,
    pub body: TypedBlock,
    pub function_id: FunctionId,
    pub trait_impl: Option<TraitImplInfo>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypedParameter {
    pub name: String,
    pub param_type: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypedBlock {
    pub statements: Vec<TypedStatement>,
    pub result_type: TypeId,
    pub span: Span,
}
```

**Tasks:**
- [x] Define `TypedStatement` enum with let bindings and expressions
- [x] Implement function parameter validation
- [x] Add guard clause type checking (must return Boolean)
- [x] Support function overloading with guards
- [x] Add anonymous function support
- [x] Write comprehensive function definition tests (7 tests covering all function scenarios)

**✅ Week 5 Complete** - Function definitions implemented with typed AST conversion and comprehensive test coverage

#### Week 6: Type System Items
**Update TypedItemKind:**
```rust
#[derive(Debug, Clone)]
pub enum TypedItemKind {
    Expression(TypedExpression),
    FunctionDefinition(TypedFunctionDefinition),
    StructDefinition(TypedStructDefinition),
    TraitDefinition(TypedTraitDefinition),
    ImplBlock(TypedImplBlock),
    ConstDefinition(TypedConstDefinition),
    LetBinding(TypedLetBinding),
    // Remove Placeholder - all items should be fully typed
}
```

**Tasks:**
- [x] Define all typed item structures (TypedStructDefinition, TypedTraitDefinition, TypedImplBlock, TypedConstDefinition)
- [x] Implement struct field validation and type checking
- [x] Add trait function signature checking and validation (signatures, definitions, static functions)
- [x] Support impl block validation against trait definitions
- [x] Implement const definition type checking with expression validation
- [x] Write comprehensive type system item tests (10 tests covering all type system scenarios)

**✅ Week 6 Complete** - Type system items implemented with typed AST conversion and comprehensive test coverage

### Phase 4: Advanced Features (Weeks 7-8)

#### Week 7: Generics & Type Annotations
**New structures:**
```rust
#[derive(Debug, Clone, PartialEq)]
pub struct TypedGenericContext {
    pub generic_params: Vec<GenericParam>,
    pub constraints: Vec<TraitConstraint>,
    pub substitutions: HashMap<String, TypeId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypedTypeAnnotation {
    pub annotation_kind: TypedTypeAnnotationKind,
    pub resolved_type: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedTypeAnnotationKind {
    Simple { path: Vec<String>, generic_args: Vec<TypedTypeAnnotation> },
    Tuple(Vec<TypedTypeAnnotation>),
    Function { params: Vec<TypedFunctionParam>, return_type: Box<TypedTypeAnnotation> },
}
```

**Tasks:**
- [x] Implement generic parameter resolution with comprehensive context support
- [x] Add comprehensive type annotation validation system (TypedTypeAnnotation structures)
- [x] Support Self type resolution in impl blocks with generic type support
- [x] Implement generic type substitution in TypedASTBuilder
- [x] Add constraint validation foundation for generic types
- [x] Write comprehensive generic type tests (10 tests covering all generic scenarios)

**✅ Week 7 Complete** - Generics and type annotations implemented with comprehensive typed AST support

#### Week 8: Macros & Advanced Syntax
**New structures:**
```rust
// Add to TypedExpressionKind enum
MacroInjection {
    parameter: String,
    injected_expression: Box<TypedExpression>,
},
SigilLiteral {
    sigil_type: String,
    content: TypedStringLiteral,
    parsed_content: Option<TypedExpression>, // If sigil type supports parsing
},
StringInterpolation {
    parts: Vec<TypedStringPart>,
    result_type: TypeId,
},

// Add to TypedItemKind enum
MacroDefinition {
    name: String,
    parameters: Vec<String>,
    body: TypedBlock,
    hygiene_scope: ScopeId,
},
```

**Tasks:**
- [ ] Implement macro parameter injection validation
- [ ] Add sigil literal type checking
- [ ] Support string interpolation expression validation
- [ ] Add attribute validation
- [ ] Write macro and advanced syntax tests

### Phase 5: Production Features (Weeks 9-10)

#### Week 9: Source Preservation & Debug Info
**New structures:**
```rust
#[derive(Debug, Clone)]
pub struct TypedDebugInfo {
    pub comments: Vec<Comment>,
    pub source_file: Option<String>,
    pub original_span: Span,
    pub type_annotations: Vec<TypeAnnotationInfo>,
    pub inferred_types: HashMap<Span, TypeId>,
}

#[derive(Debug, Clone)]
pub struct TypeAnnotationInfo {
    pub span: Span,
    pub declared_type: TypeId,
    pub inferred_type: TypeId,
    pub annotation_source: AnnotationSource,
}
```

**Tasks:**
- [ ] Add debug info preservation to all typed nodes
- [ ] Implement comment attachment to appropriate nodes
- [ ] Track type inference information for IDE support
- [ ] Support original format preservation for literals
- [ ] Add comprehensive span tracking

#### Week 10: Error Recovery & Production Polish
**New structures:**
```rust
// Add to TypedExpressionKind enum
TypeError {
    error: TypeError,
    fallback_type: TypeId,
    recovery_expression: Option<Box<TypedExpression>>,
},

#[derive(Debug, Clone)]
pub struct TypedProgram {
    pub items: Vec<TypedItem>,
    pub type_context: UnificationContext,
    pub function_registry: FunctionRegistry,
    pub compilation_order: Vec<String>,
    pub debug_info: TypedDebugInfo,
    pub error_recovery_info: Vec<ErrorRecoveryInfo>,
    pub compilation_summary: CompilationSummary,
}
```

**Tasks:**
- [ ] Implement error recovery for malformed expressions
- [ ] Add comprehensive error context preservation
- [ ] Support partial type checking with errors
- [ ] Optimize typed AST for memory efficiency
- [ ] Write end-to-end integration tests
- [ ] Performance benchmarking and optimization

## Testing Strategy

### Unit Tests (Each Phase)
- Test each new `TypedExpressionKind` variant individually
- Validate type resolution for all constructs
- Test error cases and edge conditions

### Integration Tests (Each Week)
- Test with real Outrun programs
- Validate round-trip: Parser AST → Typed AST → validation
- Test interaction between different language features

### Performance Tests (Phase 5)
- Large program compilation benchmarks
- Memory usage profiling
- Type checking speed measurements

## Success Criteria

### Phase 1 Complete
- [ ] All basic expressions (binary ops, function calls, collections) have typed variants
- [ ] Type validation works for all supported expressions
- [ ] No crashes on well-formed Outrun programs

### Phase 3 Complete
- [ ] Complete function and type system support
- [ ] All major language constructs represented in typed AST
- [ ] Type checker produces meaningful error messages

### Phase 5 Complete
- [ ] 100% parser AST coverage in typed AST
- [ ] All 400+ parser tests pass with typed AST
- [ ] Performance meets production requirements
- [ ] Ready for IDE integration and code generation

## Progress Tracking

**Current Status**: Phase 4, Week 7 - ✅ Complete | Binary CLI improvements ✅ Complete | Ready for Phase 4, Week 8

**Completed Work**:
- ✅ Defined core typed AST structures (`TypedFunctionPath`, `TypedArgument`, `DispatchMethod`)
- ✅ Added `FunctionCall` and `FieldAccess` variants to `TypedExpressionKind`
- ✅ Updated all structures to use `Option<StructuredType>` instead of default types
- ✅ Discovered integration issue and updated architecture approach
- ✅ Updated visitor trait to work with `Option<StructuredType>`
- ✅ **Implemented TypedASTBuilder visitor (500+ lines)**
- ✅ **Integrated TypedASTBuilder into 6-phase compilation pipeline**
- ✅ **Added typed_programs field to CompilationResult**
- ✅ **Fixed critical desugaring bug in DesugaringVisitor (missing ItemKind::Expression case)**
- ✅ **Completed integration testing with simple Outrun programs**
- ✅ **Added complete collection literal support (List, Map, Tuple, StructLiteral)**
- ✅ **Defined TypedMapEntry and TypedStructField structures with proper Boxing**
- ✅ **Implemented homogeneous type checking for collections**
- ✅ **Added comprehensive collection tests (6 tests, all passing)**
- ✅ **Created comprehensive test coverage for Week 1 and Week 2 (20 tests total)**
- ✅ Fixed clippy warnings and ensured all tests pass (104 tests)
- ✅ Created comprehensive plan document with new architecture
- ✅ **CLI Improvements**: Removed `--spans` option from Parse command, added `--core-lib` option to Typecheck command, enabled typed AST debug printing on successful type checking

**Current Architecture**: 6-Phase Compilation System
1. Phase 1-4: Extract traits, structs, impls, functions
2. Phase 5: TypeCheckingVisitor (validates types, stores results)
3. Phase 6: TypedASTBuilder (creates comprehensive typed AST using validation results)

**Next Milestone**: Phase 4, Week 8 - Macros & Advanced Syntax (Macro parameter injection and sigil literal support)

**Ready for**: Advanced type system features including generics, constraints, and macro support

---

*This plan transforms the minimal typed AST into a comprehensive representation supporting the full Outrun language with proper type information, dispatch tables, and production-ready error reporting.*